required_taxonomy_columns <- c(
  "topic_id",
  "parent_topic_id",
  "label_public",
  "slug",
  "description",
  "is_core",
  "sort_order"
)

load_taxonomy <- function(path) {
  taxonomy <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      parent_topic_id = as.character(parent_topic_id),
      parent_topic_id = dplyr::na_if(parent_topic_id, ""),
      is_core = as.logical(is_core)
    )

  missing_columns <- setdiff(required_taxonomy_columns, names(taxonomy))
  if (length(missing_columns) > 0) {
    stop("Missing taxonomy columns: ", paste(missing_columns, collapse = ", "))
  }

  if (anyDuplicated(taxonomy$topic_id)) {
    stop("taxonomy topic_id values must be unique")
  }

  if (anyDuplicated(taxonomy$slug)) {
    stop("taxonomy slug values must be unique")
  }

  missing_parents <- taxonomy |>
    dplyr::filter(!is.na(parent_topic_id) & !parent_topic_id %in% topic_id)

  if (nrow(missing_parents) > 0) {
    stop("Every parent_topic_id must reference an existing topic_id")
  }

  compute_level <- function(topic_key, data, seen = character()) {
    if (topic_key %in% seen) {
      stop("taxonomy contains a cycle involving topic_id ", topic_key)
    }

    parent_id <- data |>
      dplyr::filter(.data$topic_id == topic_key) |>
      dplyr::pull(.data$parent_topic_id)

    if (length(parent_id) == 0 || all(is.na(parent_id))) {
      return(1L)
    }

    1L + compute_level(parent_id[[1]], data, c(seen, topic_key))
  }

  taxonomy |>
    dplyr::mutate(
      level = purrr::map_int(topic_id, compute_level, data = taxonomy)
    ) |>
    dplyr::arrange(sort_order, label_public)
}

load_ideology_rules <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(
      policy_key = character(),
      base_weight = numeric(),
      label_hint = character(),
      public_reasoning = character()
    ))
  }

  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("policy_key", "label_hint", "public_reasoning")), as.character),
      dplyr::across(dplyr::any_of("base_weight"), as.numeric)
    ) |>
    dplyr::distinct(.data$policy_key, .keep_all = TRUE)
}

screen_public_records <- function(claims, sources) {
  public_sources <- sources |>
    dplyr::filter(
      !is.na(url),
      !is.na(published_at),
      !is.na(candidate_id),
      !is.na(source_id)
    ) |>
    dplyr::distinct(source_id, .keep_all = TRUE)

  public_claims <- claims |>
    dplyr::semi_join(public_sources, by = c("source_id", "candidate_id")) |>
    dplyr::filter(!is.na(event_date), !is.na(claim_id))

  rejected_claims <- claims |>
    dplyr::anti_join(public_claims, by = "claim_id")

  list(
    public_claims = public_claims,
    rejected_claims = rejected_claims,
    public_sources = public_sources
  )
}

detect_analysis_notes <- function(claims, sources, min_confidence = 0.7) {
  if (nrow(claims) == 0 || nrow(sources) == 0) {
    return(tibble::tibble(
      analysis_id = character(),
      candidate_id = character(),
      analysis_type = character(),
      claim_ids = character(),
      source_ids = character(),
      confidence = numeric(),
      public_reasoning_summary = character()
    ))
  }

  enriched_claims <- claims |>
    dplyr::inner_join(
      sources |>
        dplyr::select(source_id, source_confidence = confidence, published_at),
      by = "source_id"
    ) |>
    dplyr::arrange(candidate_id, policy_key, event_date, published_at)

  contradiction_groups <- enriched_claims |>
    dplyr::filter(!is.na(policy_key), !is.na(stance_value)) |>
    dplyr::group_split(candidate_id, policy_key)

  contradiction_notes <- purrr::map_dfr(contradiction_groups, function(group_claims) {
    if (nrow(group_claims) < 2) {
      return(tibble::tibble())
    }

    first_claim <- group_claims |> dplyr::slice_head(n = 1)
    last_claim <- group_claims |> dplyr::slice_tail(n = 1)

    if (
      first_claim$stance_value == last_claim$stance_value ||
      min(first_claim$source_confidence, last_claim$source_confidence) < min_confidence
    ) {
      return(tibble::tibble())
    }

    candidate_key <- first_claim$candidate_id[[1]]
    policy_key <- first_claim$policy_key[[1]]
    note_ids <- c(first_claim$claim_id, last_claim$claim_id)
    source_ids <- c(first_claim$source_id, last_claim$source_id)
    confidence <- min(first_claim$source_confidence, last_claim$source_confidence)
    reasoning <- paste0(
      "El sistema detectó una diferencia sustantiva entre ",
      format(first_claim$event_date, "%Y-%m-%d"),
      " y ",
      format(last_claim$event_date, "%Y-%m-%d"),
      " sobre la política ",
      policy_key,
      "."
    )

    tibble::tribble(
      ~analysis_id, ~candidate_id, ~analysis_type, ~claim_ids, ~source_ids, ~confidence, ~public_reasoning_summary,
      paste0("contradiccion-", candidate_key, "-", policy_key), candidate_key, "contradiccion_interna", paste(note_ids, collapse = "|"), paste(source_ids, collapse = "|"), confidence, reasoning,
      paste0("cambio-", candidate_key, "-", policy_key), candidate_key, "cambio_de_postura", paste(note_ids, collapse = "|"), paste(source_ids, collapse = "|"), confidence, reasoning
    )
  })

  implementation_notes <- enriched_claims |>
    dplyr::filter(
      claim_type == "policy_proposal",
      implementation_detail %in% c(FALSE, 0),
      source_confidence >= min_confidence
    ) |>
    dplyr::transmute(
      analysis_id = paste0("vacio-", claim_id),
      candidate_id,
      analysis_type = "vacio_de_implementacion",
      claim_ids = claim_id,
      source_ids = source_id,
      confidence = source_confidence,
      public_reasoning_summary = paste0(
        "La propuesta registrada para ",
        policy_key,
        " no incluye suficiente detalle de implementación en la fuente analizada."
      )
    )

  dplyr::bind_rows(contradiction_notes, implementation_notes) |>
    dplyr::distinct(analysis_id, .keep_all = TRUE) |>
    dplyr::arrange(candidate_id, analysis_type, dplyr::desc(confidence))
}

collapse_topic_labels <- function(claims, taxonomy) {
  claims |>
    dplyr::count(topic_id, sort = TRUE) |>
    dplyr::left_join(
      taxonomy |>
        dplyr::select(topic_id, label_public),
      by = "topic_id"
    ) |>
    dplyr::slice_head(n = 3) |>
    dplyr::pull(label_public) |>
    paste(collapse = ", ")
}

label_ideology_score <- function(score, signal_count) {
  if (is.na(score) || signal_count < 1) {
    return("Evidencia insuficiente")
  }

  base_label <- dplyr::case_when(
    score <= -0.55 ~ "Izquierda",
    score < -0.20 ~ "Centroizquierda",
    score <= 0.20 ~ "Centro",
    score <= 0.55 ~ "Centroderecha",
    TRUE ~ "Derecha"
  )

  if (signal_count < 3 && base_label == "Izquierda") {
    return("Centroizquierda")
  }

  if (signal_count < 3 && base_label == "Derecha") {
    return("Centroderecha")
  }

  base_label
}

compute_ideology_profiles <- function(claims, sources, ideology_rules = NULL) {
  if (
    is.null(ideology_rules) ||
    nrow(ideology_rules) == 0 ||
    nrow(claims) == 0 ||
    nrow(sources) == 0
  ) {
    return(tibble::tibble(
      candidate_id = character(),
      ideology_score = numeric(),
      ideology_label = character(),
      ideology_signal_count = integer(),
      ideology_rationale = character()
    ))
  }

  enriched <- claims |>
    dplyr::filter(.data$claim_type == "policy_proposal", !is.na(.data$policy_key)) |>
    dplyr::inner_join(
      sources |>
        dplyr::select("source_id", source_confidence = "confidence"),
      by = "source_id"
    ) |>
    dplyr::inner_join(ideology_rules, by = "policy_key") |>
    dplyr::mutate(
      stance_direction = dplyr::case_when(
        !is.na(.data$stance_value) & .data$stance_value > 0 ~ 1,
        !is.na(.data$stance_value) & .data$stance_value < 0 ~ -1,
        TRUE ~ 1
      ),
      raw_signal = .data$base_weight * .data$stance_direction,
      weight = pmax(abs(.data$base_weight) * .data$source_confidence, 0.01),
      weighted_signal = .data$raw_signal * .data$source_confidence
    )

  if (nrow(enriched) == 0) {
    return(tibble::tibble(
      candidate_id = character(),
      ideology_score = numeric(),
      ideology_label = character(),
      ideology_signal_count = integer(),
      ideology_rationale = character()
    ))
  }

  enriched |>
    dplyr::group_split(.data$candidate_id) |>
    purrr::map_dfr(function(group_claims) {
      candidate_key <- group_claims$candidate_id[[1]]
      ideology_score <- sum(group_claims$weighted_signal, na.rm = TRUE) / sum(group_claims$weight, na.rm = TRUE)
      signal_count <- nrow(group_claims)
      top_signals <- group_claims |>
        dplyr::arrange(dplyr::desc(abs(.data$weighted_signal)), dplyr::desc(.data$source_confidence)) |>
        dplyr::slice_head(n = 3)

      rationale <- if (nrow(top_signals) == 0) {
        "Todavía no hay señales programáticas suficientes para una lectura ideológica."
      } else {
        paste0(
          "Se apoya en señales como ",
          paste(
            paste0(
              top_signals$policy_key,
              " (",
              top_signals$label_hint,
              ")"
            ),
            collapse = ", "
          ),
          "."
        )
      }

      tibble::tibble(
        candidate_id = candidate_key,
        ideology_score = ideology_score,
        ideology_label = label_ideology_score(ideology_score, signal_count),
        ideology_signal_count = signal_count,
        ideology_rationale = rationale
      )
    })
}

build_candidate_dossiers <- function(candidates, claims, analysis_notes, sources, taxonomy, ideology_rules = NULL) {
  if (nrow(candidates) == 0) {
    return(tibble::tibble())
  }

  ideology_profiles <- compute_ideology_profiles(claims, sources, ideology_rules)

  purrr::pmap_dfr(candidates, function(...) {
    row <- tibble::as_tibble(list(...))
    candidate_key <- row$candidate_id[[1]]
    candidate_claims <- claims |> dplyr::filter(candidate_id == candidate_key)
    candidate_last_date <- suppressWarnings(max(candidate_claims$event_date, na.rm = TRUE))

    row |>
      dplyr::mutate(
        total_claims = nrow(candidate_claims),
        total_sources = sum(sources$candidate_id == candidate_key),
        total_analysis_notes = sum(analysis_notes$candidate_id == candidate_key),
        top_topics = collapse_topic_labels(candidate_claims, taxonomy),
        last_event_date = if (is.infinite(candidate_last_date)) as.Date(NA) else as.Date(candidate_last_date)
      )
  }) |>
    dplyr::left_join(ideology_profiles, by = "candidate_id") |>
    dplyr::mutate(
      top_topics = dplyr::if_else(top_topics == "", "Sin temas públicos todavía", top_topics),
      ideology_label = dplyr::coalesce(.data$ideology_label, "Evidencia insuficiente"),
      ideology_rationale = dplyr::coalesce(
        .data$ideology_rationale,
        "Todavía no hay suficientes señales programáticas mapeadas para ubicarlo en el espectro."
      ),
      ideology_signal_count = dplyr::coalesce(.data$ideology_signal_count, 0L)
    )
}

build_daily_digest <- function(claims, analysis_notes, date) {
  daily_claims <- claims |>
    dplyr::filter(event_date == as.Date(date))

  claim_ids_for_day <- daily_claims$claim_id

  daily_analysis <- if (length(claim_ids_for_day) == 0 || nrow(analysis_notes) == 0) {
    analysis_notes[0, , drop = FALSE]
  } else {
    analysis_notes |>
      dplyr::filter(
        purrr::map_lgl(
          claim_ids,
          \(claim_ids_text) any(stringr::str_detect(claim_ids_text, stringr::fixed(claim_ids_for_day)))
        )
      )
  }

  tibble::tibble(
    digest_date = as.Date(date),
    total_claims = nrow(daily_claims),
    total_candidates = dplyr::n_distinct(daily_claims$candidate_id),
    total_analysis_notes = nrow(daily_analysis)
  )
}
