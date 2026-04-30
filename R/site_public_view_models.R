normalize_public_collection <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(list())
  }

  if (is.data.frame(x)) {
    rows <- split(x, seq_len(nrow(x)))
    return(lapply(rows, as.list))
  }

  if (is.list(x) && length(x) == 1 && is.data.frame(x[[1]])) {
    return(normalize_public_collection(x[[1]]))
  }

  if (is.list(x) && !is.data.frame(x) && !is.null(names(x))) {
    lengths <- vapply(x, length, integer(1))
    non_zero_lengths <- lengths[lengths > 0]
    is_columnar <- length(non_zero_lengths) > 0 &&
      length(unique(non_zero_lengths)) == 1 &&
      unique(non_zero_lengths)[[1]] > 1 &&
      all(vapply(x, \(value) is.atomic(value) || is.null(value), logical(1)))

    if (is_columnar) {
      row_count <- unique(non_zero_lengths)[[1]]
      return(lapply(seq_len(row_count), function(index) {
        lapply(x, function(column) {
          if (length(column) < index) {
            return(NULL)
          }

          column[[index]]
        })
      }))
    }
  }

  if (!is.list(x)) {
    return(as.list(x))
  }

  if (!is.null(x$artifact_type) || !is.null(x$section_id) || !is.null(x$topic_id) || !is.null(x$candidate_id)) {
    return(list(x))
  }

  if (!is.null(names(x))) {
    return(unname(x))
  }

  x
}

normalize_public_scalar <- function(x, default = NA_character_) {
  values <- normalize_public_vector(x)

  if (length(values) == 0) {
    return(default)
  }

  values[[1]]
}

normalize_public_vector <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character())
  }

  if (is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }

  values <- as.character(x)
  values <- stats::na.omit(values)
  values[values != ""]
}

normalize_public_artifact <- function(artifact) {
  if (is.null(artifact) || !is.list(artifact)) {
    return(NULL)
  }

  artifact$candidate_ids <- normalize_public_vector(artifact$candidate_ids)
  artifact$source_ids <- normalize_public_vector(artifact$source_ids)
  artifact$claim_ids <- normalize_public_vector(artifact$claim_ids)
  artifact$sections <- normalize_public_collection(artifact$sections)
  artifact
}

read_editorial_packages_public <- function(project_dir = ".") {
  packages <- read_public_json("editorial_packages.json", project_dir = project_dir)

  purrr::keep(
    lapply(normalize_public_collection(packages), normalize_public_artifact),
    Negate(is.null)
  )
}

read_candidate_policy_dossiers_public <- function(project_dir = ".") {
  dossiers <- read_public_json("candidate_policy_dossiers.json", project_dir = project_dir)

  normalize_public_collection(dossiers)
}

read_comparison_essays_public <- function(project_dir = ".") {
  essays <- read_public_json("comparison_essays.json", project_dir = project_dir)

  normalize_public_collection(essays)
}

find_candidate_policy_dossier_public <- function(candidate_id, project_dir = ".") {
  dossiers <- read_candidate_policy_dossiers_public(project_dir = project_dir)
  matches <- purrr::keep(dossiers, \(dossier) identical(normalize_public_scalar(dossier$candidate_id, default = ""), candidate_id))

  if (length(matches) == 0) {
    return(NULL)
  }

  matches[[1]]
}

find_public_artifact <- function(artifacts, artifact_type) {
  matches <- purrr::keep(artifacts, \(artifact) identical(artifact$artifact_type %||% NA_character_, artifact_type))

  if (length(matches) == 0) {
    return(NULL)
  }

  normalize_public_artifact(matches[[1]])
}

artifact_section_by_id <- function(artifact, section_id) {
  if (is.null(artifact)) {
    return(NULL)
  }

  sections <- normalize_public_collection(artifact$sections)
  matches <- purrr::keep(sections, \(section) identical(section$section_id %||% NA_character_, section_id))

  if (length(matches) == 0) {
    return(NULL)
  }

  matches[[1]]
}

public_evidence_label <- function(state) {
  dplyr::case_when(
    identical(state, "solid") ~ "diferencia visible",
    identical(state, "partial") ~ "lectura parcial",
    TRUE ~ "posición trazable pendiente"
  )
}

public_topic_label <- function(topic_id) {
  stringr::str_to_title(gsub("[-_]+", " ", normalize_public_scalar(topic_id, default = "este tema")))
}

looks_like_internal_comparison_note <- function(text) {
  normalized <- normalize_public_scalar(text, default = "")

  normalized != "" && (
    grepl("[a-z0-9-]+_[a-z0-9-]+\\s*:", normalized, perl = TRUE) ||
      grepl("[a-z0-9-]+\\s*=\\s*[A-Za-zÁÉÍÓÚáéíóúñÑ]", normalized, perl = TRUE)
  )
}

is_insufficient_public_value <- function(value) {
  normalized <- tolower(normalize_public_scalar(value, default = ""))

  normalized == "" ||
    grepl("sin evidencia suficiente|sin base suficiente|evidencia insuficiente|sin posicion trazable|posición trazable pendiente", normalized, perl = TRUE)
}

topic_row_supported_candidates <- function(topic_row) {
  candidate_rows <- normalize_public_collection(topic_row$candidate_rows)

  purrr::keep(candidate_rows, function(row) {
    !is_insufficient_public_value(row$priority)
  })
}

comparison_topic_evidence_state <- function(topic_row) {
  candidate_rows <- normalize_public_collection(topic_row$candidate_rows)
  supported_rows <- topic_row_supported_candidates(topic_row)
  strong_rows <- purrr::keep(candidate_rows, function(row) {
    priority <- tolower(normalize_public_scalar(row$priority, default = ""))
    specificity <- tolower(normalize_public_scalar(row$specificity, default = ""))
    feasibility <- tolower(normalize_public_scalar(row$feasibility, default = ""))

    priority == "alta" ||
      specificity == "alta" ||
      feasibility %in% c("relativamente evaluable", "parcialmente evaluable")
  })

  if (length(supported_rows) == 0) {
    return("insufficient")
  }

  if (length(supported_rows) == length(candidate_rows) && length(strong_rows) >= 2) {
    return("solid")
  }

  "partial"
}

validation_badge_view_model <- function(validation_report, validation_status = NULL) {
  processed_status <- if (is.data.frame(validation_status) && nrow(validation_status) > 0 && "status" %in% names(validation_status)) {
    validation_status$status[[1]]
  } else {
    NULL
  }

  processed_summary <- if (is.data.frame(validation_status) && nrow(validation_status) > 0 && "summary" %in% names(validation_status)) {
    validation_status$summary[[1]]
  } else {
    NULL
  }

  status <- validation_report$status %||% processed_status %||% "unknown"
  summary <- validation_report$summary %||% processed_summary %||% "Sin validación metodológica publicada."

  list(
    status = status,
    tone = dplyr::case_when(
      identical(status, "pass") ~ "ok",
      identical(status, "pass_with_warnings") ~ "warning",
      identical(status, "block") ~ "danger",
      TRUE ~ "neutral"
    ),
    label = dplyr::case_when(
      identical(status, "pass") ~ "Metodología verificada",
      identical(status, "pass_with_warnings") ~ "Metodología con advertencias",
      identical(status, "block") ~ "Validación bloqueada",
      TRUE ~ "Sin validación pública"
    ),
    summary = summary
  )
}

candidate_lookup_public <- function(candidates) {
  if (nrow(candidates) == 0) {
    return(list())
  }

  stats::setNames(
    lapply(seq_len(nrow(candidates)), function(index) {
      row <- candidates[index, , drop = FALSE]
      list(
        candidate_id = row$candidate_id[[1]],
        slug = row$slug[[1]] %||% row$candidate_id[[1]],
        president_name = row$president_name[[1]] %||% row$candidate_id[[1]],
        watchlist_active = isTRUE(row$watchlist_active[[1]]),
        watchlist_priority = row$watchlist_priority[[1]] %||% NA_integer_,
        photo_url = row$photo_url[[1]] %||% "",
        photo_alt = row$photo_alt[[1]] %||% row$president_name[[1]] %||% row$candidate_id[[1]],
        photo_credit = row$photo_credit[[1]] %||% "",
        photo_source_url = row$photo_source_url[[1]] %||% ""
      )
    }),
    candidates$candidate_id
  )
}

build_public_comparison_summary <- function(topic_id, candidate_names, evidence_state) {
  topic_label <- public_topic_label(topic_id)
  candidate_phrase <- if (length(candidate_names) >= 2) {
    paste(head(candidate_names, 2), collapse = " y ")
  } else if (length(candidate_names) == 1) {
    candidate_names[[1]]
  } else {
    "las candidaturas observadas"
  }

  dplyr::case_when(
    identical(evidence_state, "solid") ~ paste0(
      "En ",
      topic_label,
      ", ",
      candidate_phrase,
      " ya muestran una diferencia visible con evidencia pública comparable."
    ),
    identical(evidence_state, "partial") && length(candidate_names) >= 2 ~ paste0(
      "En ",
      topic_label,
      ", ",
      candidate_phrase,
      " permiten una lectura parcial: hay contraste útil, pero la evidencia sigue siendo desigual."
    ),
    identical(evidence_state, "partial") ~ paste0(
      "En ",
      topic_label,
      ", solo ",
      candidate_phrase,
      " cuenta hoy con material trazable para una lectura parcial; el resto sigue siendo desigual."
    ),
    TRUE ~ paste0(
      "En ",
      topic_label,
      ", todavía faltan posiciones públicas trazables para sostener una comparación firme entre candidaturas."
    )
  )
}

build_homepage_comparison_blocks <- function(comparison_report, candidates, taxonomy_lookup = tibble::tibble(), limit = 3) {
  if (is.null(comparison_report)) {
    return(list())
  }

  topic_rows <- normalize_public_collection(comparison_report$topic_comparison)
  if (length(topic_rows) == 0) {
    return(list())
  }

  candidate_lookup <- candidate_lookup_public(candidates)

  blocks <- lapply(topic_rows, function(topic_row) {
    topic_request <- normalize_topic_request(
      normalize_public_scalar(topic_row$topic_id, default = "sin-tema"),
      taxonomy_lookup = taxonomy_lookup
    )
    handoff_topic_id <- topic_request$root_topic_id %||% normalize_public_scalar(topic_row$topic_id, default = "sin-tema")
    supported_rows <- topic_row_supported_candidates(topic_row)
    supported_ids <- purrr::map_chr(supported_rows, \(row) normalize_public_scalar(row$candidate_id, default = NA_character_))
    supported_ids <- stats::na.omit(supported_ids)
    known_supported_ids <- unname(supported_ids[supported_ids %in% names(candidate_lookup)])
    evidence_state <- comparison_topic_evidence_state(topic_row)
    candidate_names <- unname(vapply(known_supported_ids, function(candidate_id) {
      candidate_lookup[[candidate_id]]$president_name %||% candidate_id
    }, character(1)))

    list(
      topic_id = normalize_public_scalar(topic_row$topic_id, default = "sin-tema"),
      summary = build_public_comparison_summary(
        topic_id = topic_row$topic_id,
        candidate_names = candidate_names,
        evidence_state = evidence_state
      ),
      candidate_ids = known_supported_ids,
      candidate_names = candidate_names,
      evidence_state = evidence_state,
      public_label = public_evidence_label(evidence_state),
      supported_candidate_count = length(known_supported_ids),
      handoff = list(
        topic_or_axis = handoff_topic_id,
        section_anchor = paste0("topic-", handoff_topic_id),
        candidate_ids = known_supported_ids,
        candidate_destinations = unname(lapply(known_supported_ids, function(candidate_id) {
          candidate_meta <- candidate_lookup[[candidate_id]]
          list(
            candidate_id = candidate_id,
            href = paste0(
              "candidatos/",
              candidate_meta$slug %||% candidate_id,
              ".html?from=homepage&topic=",
              utils::URLencode(handoff_topic_id, reserved = TRUE),
              "#propuestas-y-posiciones-publicas"
            ),
            fallback_destination = paste0("candidatos/", candidate_meta$slug %||% candidate_id, ".html")
          )
        })),
        evidence_state = evidence_state,
        fallback_destination = "comparador.html"
      )
    )
  })

  state_score <- c(insufficient = 1L, partial = 2L, solid = 3L)
  ordered_blocks <- blocks[order(
    -vapply(blocks, \(block) state_score[[block$evidence_state]], integer(1)),
    -vapply(blocks, \(block) block$supported_candidate_count, integer(1)),
    vapply(blocks, \(block) block$topic_id, character(1))
  )]

  ordered_blocks[seq_len(min(limit, length(ordered_blocks)))]
}

build_public_key_comparison_note <- function(raw_note, comparison_blocks, daily_update = NULL) {
  if (!looks_like_internal_comparison_note(raw_note)) {
    normalized <- normalize_public_scalar(raw_note, default = "")
    if (normalized != "") {
      return(normalized)
    }
  }

  if (length(comparison_blocks) > 0) {
    lead_block <- comparison_blocks[[1]]
    topic_label <- public_topic_label(lead_block$topic_id)
    candidate_phrase <- if (length(lead_block$candidate_names) >= 2) {
      paste(head(lead_block$candidate_names, 2), collapse = " y ")
    } else if (length(lead_block$candidate_names) == 1) {
      lead_block$candidate_names[[1]]
    } else {
      "varias candidaturas"
    }

    return(
      paste0(
        "La comparación más útil hoy aparece en ",
        topic_label,
        ": ",
        candidate_phrase,
        " muestran una ",
        public_evidence_label(lead_block$evidence_state),
        "."
      )
    )
  }

  fallback_note <- artifact_section_by_id(daily_update, "open_questions")$body %||% ""
  if (fallback_note != "") {
    return(fallback_note)
  }

  "Todavía no hay suficiente base comparativa para una lectura editorial fuerte."
}

homepage_candidate_summary <- function(candidate_id, claims, taxonomy_lookup = tibble::tibble(), policy_dossier = NULL) {
  if (!is.null(policy_dossier)) {
    sections <- normalize_public_collection(policy_dossier$sections)
    first_policy <- purrr::detect(sections, \(section) normalize_public_scalar(section$section_id, default = "") != "trayectoria-contexto")
    body <- normalize_public_scalar(first_policy$body, default = "")
    if (body != "") {
      return(stringr::str_trunc(gsub("\\{src:[^}]+\\}", "", body), 320))
    }
  }

  if (!is.data.frame(claims) || nrow(claims) == 0) {
    return("Ficha en construcción con fuentes trazables.")
  }

  candidate_claims <- claims |>
    dplyr::filter(.data$candidate_id == .env$candidate_id, .data$claim_type == "policy_proposal") |>
    dplyr::left_join(taxonomy_lookup, by = "topic_id") |>
    dplyr::mutate(
      root_label = dplyr::coalesce(.data$root_label, .data$topic_id),
      root_sort_order = dplyr::coalesce(as.integer(.data$root_sort_order), 999L)
    ) |>
    dplyr::arrange(.data$root_sort_order, dplyr::desc(.data$specificity_score), dplyr::desc(.data$event_date))

  if (nrow(candidate_claims) == 0) {
    return("La ficha conserva contexto público, pero todavía requiere más propuestas explícitas para una síntesis programática densa.")
  }

  topics <- candidate_claims |>
    dplyr::count(.data$root_label, sort = TRUE) |>
    dplyr::slice_head(n = 3) |>
    dplyr::pull(.data$root_label)
  lead <- candidate_claims$summary_text[[1]]

  paste0(
    "Su agenda pública se mueve sobre todo alrededor de ",
    paste(topics, collapse = ", "),
    ". La señal más visible por ahora es esta: ",
    lead,
    "."
  )
}

build_homepage_roster <- function(candidates, claims = tibble::tibble(), taxonomy_lookup = tibble::tibble(), policy_dossiers = list()) {
  if (nrow(candidates) == 0) {
    return(list())
  }

  ordered <- candidates |>
    dplyr::filter(.data$watchlist_active %in% TRUE) |>
    dplyr::mutate(
      watchlist_sort = dplyr::if_else(
        .data$watchlist_active %in% TRUE,
        dplyr::coalesce(as.integer(.data$watchlist_priority), 999L),
        999L
      )
    ) |>
    dplyr::arrange(.data$watchlist_sort, .data$ballot_position, .data$president_name)

  lapply(seq_len(nrow(ordered)), function(index) {
    row <- ordered[index, , drop = FALSE]
    list(
      candidate_id = row$candidate_id[[1]],
      slug = row$slug[[1]] %||% row$candidate_id[[1]],
      president_name = row$president_name[[1]] %||% row$candidate_id[[1]],
      watchlist_active = isTRUE(row$watchlist_active[[1]]),
      watchlist_priority = row$watchlist_priority[[1]] %||% NA_integer_,
      href = paste0("candidatos/", row$slug[[1]] %||% row$candidate_id[[1]], ".html"),
      photo_url = row$photo_url[[1]] %||% "",
      photo_alt = row$photo_alt[[1]] %||% row$president_name[[1]] %||% row$candidate_id[[1]],
      photo_credit = row$photo_credit[[1]] %||% "",
      photo_source_url = row$photo_source_url[[1]] %||% "",
      policy_summary = homepage_candidate_summary(
        row$candidate_id[[1]],
        claims,
        taxonomy_lookup,
        policy_dossier = policy_dossiers[[row$candidate_id[[1]]]]
      )
    )
  })
}

build_homepage_view_model <- function(project_dir = ".", comparison_limit = 3) {
  candidates <- read_candidate_registry_public(project_dir = project_dir)
  taxonomy_path <- file.path(project_dir, "config", "taxonomy_v1.csv")
  taxonomy <- if (file.exists(taxonomy_path)) {
    read_taxonomy_public(project_dir = project_dir)
  } else {
    tibble::tibble()
  }
  taxonomy_lookup <- taxonomy_root_lookup_public(taxonomy)
  editorial_packages <- read_editorial_packages_public(project_dir = project_dir)
  homepage_brief <- find_public_artifact(editorial_packages, "homepage_brief")
  daily_update <- find_public_artifact(editorial_packages, "daily_update")
  comparison_report <- read_public_json("comparison_report.json", project_dir = project_dir)
  policy_dossiers <- read_candidate_policy_dossiers_public(project_dir = project_dir)
  policy_dossiers_by_candidate <- stats::setNames(policy_dossiers, purrr::map_chr(policy_dossiers, \(dossier) normalize_public_scalar(dossier$candidate_id, default = "")))
  validation_report <- read_public_json("validation_report.json", project_dir = project_dir)
  validation_status <- read_processed_table("validation_status.csv", project_dir = project_dir)
  claims <- read_public_table("claim_records.json", project_dir = project_dir)

  top_changes <- artifact_section_by_id(homepage_brief, "top_changes")$body %||%
    artifact_section_by_id(daily_update, "what_changed")$body %||%
    "Todavía no hay suficiente material público para resumir cambios recientes."

  key_comparison_note <- artifact_section_by_id(homepage_brief, "key_comparison_note")$body %||%
    artifact_section_by_id(daily_update, "open_questions")$body %||%
    "Todavía no hay suficiente base comparativa para una lectura editorial fuerte."

  caveats <- artifact_section_by_id(homepage_brief, "caveats")$body %||%
    "La portada debe declarar cuando la evidencia sigue siendo parcial o insuficiente."

  comparison_blocks <- build_homepage_comparison_blocks(
    comparison_report = comparison_report,
    candidates = candidates,
    taxonomy_lookup = taxonomy_lookup,
    limit = comparison_limit
  )

  key_comparison_note <- build_public_key_comparison_note(
    raw_note = artifact_section_by_id(homepage_brief, "key_comparison_note")$body %||%
      artifact_section_by_id(daily_update, "open_questions")$body,
    comparison_blocks = comparison_blocks,
    daily_update = daily_update
  )

  list(
    artifact_id = homepage_brief$artifact_id %||% NA_character_,
    title = homepage_brief$title %||% "Resumen ejecutivo",
    dek = homepage_brief$dek %||% "Todavía no hay suficiente material para un resumen editorial completo.",
    top_changes = top_changes,
    key_comparison_note = key_comparison_note,
    caveats = caveats,
    methodology_badge = validation_badge_view_model(validation_report, validation_status),
    comparison_blocks = comparison_blocks,
    roster = build_homepage_roster(candidates, claims = claims, taxonomy_lookup = taxonomy_lookup, policy_dossiers = policy_dossiers_by_candidate),
    empty_state = if (length(comparison_blocks) == 0) {
      "Todavía no hay suficientes diferencias comparables para destacar en portada."
    } else {
      NULL
    }
  )
}

taxonomy_root_lookup_public <- function(taxonomy) {
  if (!is.data.frame(taxonomy) || nrow(taxonomy) == 0) {
    return(tibble::tibble())
  }

  parent_lookup <- stats::setNames(taxonomy$parent_topic_id, taxonomy$topic_id)
  label_lookup <- stats::setNames(taxonomy$label_public, taxonomy$topic_id)
  description_lookup <- stats::setNames(taxonomy$description, taxonomy$topic_id)
  sort_lookup <- stats::setNames(taxonomy$sort_order, taxonomy$topic_id)

  find_root <- function(topic_id) {
    parent_id <- parent_lookup[[topic_id]]
    if (is.null(parent_id) || is.na(parent_id) || identical(parent_id, "")) {
      return(topic_id)
    }

    Recall(parent_id)
  }

  roots <- vapply(taxonomy$topic_id, find_root, character(1))

  tibble::tibble(
    topic_id = taxonomy$topic_id,
    topic_label = taxonomy$label_public,
    root_topic_id = roots,
    root_label = unname(label_lookup[roots]),
    root_description = unname(description_lookup[roots]),
    root_sort_order = unname(sort_lookup[roots])
  ) |>
    dplyr::mutate(
      root_label = dplyr::coalesce(.data$root_label, .data$topic_label, .data$root_topic_id),
      root_description = dplyr::coalesce(.data$root_description, ""),
      root_sort_order = dplyr::coalesce(as.integer(.data$root_sort_order), 999L)
    )
}

normalize_public_table <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(tibble::tibble())
  }

  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }

  tibble::as_tibble(x)
}

read_public_table <- function(filename, project_dir = ".") {
  normalize_public_table(read_public_json(filename, project_dir = project_dir))
}

ensure_public_table_columns <- function(data, defaults = list()) {
  data <- tibble::as_tibble(data)
  row_count <- nrow(data)

  for (column in names(defaults)) {
    if (column %in% names(data)) {
      next
    }

    data[[column]] <- rep(defaults[[column]], row_count)
  }

  data
}

normalize_topic_request <- function(topic_id, taxonomy_lookup = tibble::tibble()) {
  if (is.null(topic_id) || length(topic_id) == 0 || is.na(topic_id) || identical(topic_id, "")) {
    return(NULL)
  }

  normalized <- as.character(topic_id[[1]])
  if (!is.data.frame(taxonomy_lookup) || nrow(taxonomy_lookup) == 0 || !all(c("topic_id", "root_topic_id", "root_label") %in% names(taxonomy_lookup))) {
    return(list(
      requested_topic_id = normalized,
      root_topic_id = normalized,
      label = public_topic_label(normalized)
    ))
  }

  matches <- taxonomy_lookup |>
    dplyr::filter(.data$topic_id == normalized | .data$root_topic_id == normalized) |>
    dplyr::slice_head(n = 1)

  if (nrow(matches) == 0) {
    return(list(
      requested_topic_id = normalized,
      root_topic_id = normalized,
      label = public_topic_label(normalized)
    ))
  }

  list(
    requested_topic_id = normalized,
    root_topic_id = matches$root_topic_id[[1]],
    label = matches$root_label[[1]]
  )
}

comparison_topic_ids_by_candidate <- function(comparison_report, taxonomy_lookup = tibble::tibble()) {
  topic_rows <- normalize_public_collection(comparison_report$topic_comparison)
  if (length(topic_rows) == 0) {
    return(list())
  }

  mapping <- list()
  for (topic_row in topic_rows) {
    topic_request <- normalize_topic_request(
      normalize_public_scalar(topic_row$topic_id, default = NA_character_),
      taxonomy_lookup = taxonomy_lookup
    )
    root_topic_id <- topic_request$root_topic_id %||% normalize_public_scalar(topic_row$topic_id, default = NA_character_)
    supported_rows <- topic_row_supported_candidates(topic_row)
    for (row in supported_rows) {
      candidate_id <- normalize_public_scalar(row$candidate_id, default = NA_character_)
      if (is.na(candidate_id)) {
        next
      }

      mapping[[candidate_id]] <- unique(c(mapping[[candidate_id]], root_topic_id))
    }
  }

  mapping
}

candidate_topic_state_public <- function(candidate_id, root_topic_id, comparable_topic_ids, documented_topic_ids) {
  if (!is.na(root_topic_id) && root_topic_id %in% (comparable_topic_ids[[candidate_id]] %||% character())) {
    return("comparable")
  }

  if (!is.na(root_topic_id) && root_topic_id %in% (documented_topic_ids[[candidate_id]] %||% character())) {
    return("documented_only")
  }

  "empty"
}

candidate_topic_state_label <- function(state) {
  dplyr::case_when(
    identical(state, "comparable") ~ "comparable",
    identical(state, "documented_only") ~ "documentado, aún no comparable",
    TRUE ~ "sin evidencia pública suficiente"
  )
}

build_candidate_policy_topic_sections <- function(candidate_claims, taxonomy_lookup, comparable_topic_ids) {
  if (!is.data.frame(candidate_claims) || nrow(candidate_claims) == 0) {
    return(list(comparable = list(), documented_only = list()))
  }

  enriched_claims <- candidate_claims |>
    dplyr::left_join(taxonomy_lookup, by = "topic_id") |>
    dplyr::mutate(
      root_topic_id = dplyr::coalesce(.data$root_topic_id, .data$topic_id),
      root_label = dplyr::coalesce(.data$root_label, .data$topic_id),
      root_description = dplyr::coalesce(.data$root_description, ""),
      root_sort_order = dplyr::coalesce(as.integer(.data$root_sort_order), 999L),
      topic_label = dplyr::coalesce(.data$topic_label, .data$topic_id)
    ) |>
    dplyr::arrange(.data$root_sort_order, dplyr::desc(.data$event_date))

  sections <- lapply(split(enriched_claims, enriched_claims$root_topic_id), function(section_claims) {
    root_topic_id <- section_claims$root_topic_id[[1]]
    state <- if (root_topic_id %in% comparable_topic_ids) "comparable" else "documented_only"

    list(
      topic_id = root_topic_id,
      topic_label = section_claims$root_label[[1]],
      topic_description = section_claims$root_description[[1]] %||% "",
      state = state,
      state_label = candidate_topic_state_label(state),
      claim_rows = section_claims
    )
  })

  ordered_sections <- sections[order(
    vapply(sections, \(section) if (identical(section$state, "comparable")) 1L else 2L, integer(1)),
    vapply(sections, \(section) normalize_public_scalar(section$topic_label, default = ""), character(1))
  )]

  list(
    comparable = unname(Filter(\(section) identical(section$state, "comparable"), ordered_sections)),
    documented_only = unname(Filter(\(section) identical(section$state, "documented_only"), ordered_sections))
  )
}

build_candidate_policy_view_model <- function(candidate_id, topic_id = NULL, from = NULL, project_dir = ".") {
  candidates <- read_candidate_registry_public(project_dir = project_dir)
  claims <- read_public_table("claim_records.json", project_dir = project_dir)
  sources <- read_public_table("source_records.json", project_dir = project_dir)
  taxonomy <- read_taxonomy_public(project_dir = project_dir)
  comparison_report <- read_public_json("comparison_report.json", project_dir = project_dir)
  policy_dossier <- find_candidate_policy_dossier_public(candidate_id, project_dir = project_dir)

  taxonomy_lookup <- taxonomy_root_lookup_public(taxonomy)
  candidate_row <- candidates |>
    dplyr::filter(.data$candidate_id == .env$candidate_id) |>
    dplyr::slice_head(n = 1)
  candidate_name <- candidate_row$president_name[[1]] %||% candidate_id
  candidate_slug <- candidate_row$slug[[1]] %||% candidate_id

  policy_claims <- claims |>
    dplyr::filter(.data$candidate_id == .env$candidate_id, .data$claim_type == "policy_proposal")

  raw_documented_topic_ids <- unique(policy_claims$topic_id[!is.na(policy_claims$topic_id)])
  documented_topic_ids <- split(
    raw_documented_topic_ids,
    rep(candidate_id, length(raw_documented_topic_ids))
  )
  if (length(documented_topic_ids) == 0) {
    documented_topic_ids <- list()
  }
  if (length(documented_topic_ids[[candidate_id]]) > 0) {
    mapped_root_ids <- taxonomy_lookup$root_topic_id[match(documented_topic_ids[[candidate_id]], taxonomy_lookup$topic_id)]
    documented_topic_ids[[candidate_id]] <- unique(dplyr::coalesce(mapped_root_ids, documented_topic_ids[[candidate_id]]))
  }

  comparable_topic_ids <- comparison_topic_ids_by_candidate(comparison_report, taxonomy_lookup = taxonomy_lookup)
  sections <- build_candidate_policy_topic_sections(
    candidate_claims = policy_claims,
    taxonomy_lookup = taxonomy_lookup,
    comparable_topic_ids = comparable_topic_ids[[candidate_id]] %||% character()
  )

  topic_focus <- normalize_topic_request(topic_id, taxonomy_lookup = taxonomy_lookup)
  focus_state <- if (is.null(topic_focus)) {
    NULL
  } else {
    candidate_topic_state_public(
      candidate_id = candidate_id,
      root_topic_id = topic_focus$root_topic_id,
      comparable_topic_ids = comparable_topic_ids,
      documented_topic_ids = documented_topic_ids
    )
  }

  list(
    candidate_id = candidate_id,
    candidate_slug = candidate_slug,
    candidate_name = candidate_name,
    from = from %||% "",
    topic_focus = if (is.null(topic_focus)) NULL else c(topic_focus, list(state = focus_state, state_label = candidate_topic_state_label(focus_state))),
    comparable_sections = sections$comparable,
    documented_sections = sections$documented_only,
    editorial_sections = if (is.null(policy_dossier)) list() else normalize_public_collection(policy_dossier$sections),
    source_library = sources |>
      dplyr::filter(.data$candidate_id == .env$candidate_id) |>
      dplyr::arrange(dplyr::desc(.data$published_at)),
    empty_state = nrow(policy_claims) == 0
  )
}

build_comparison_view_model <- function(project_dir = ".") {
  candidates <- read_candidate_registry_public(project_dir = project_dir)
  taxonomy <- read_taxonomy_public(project_dir = project_dir)
  claims <- read_public_table("claim_records.json", project_dir = project_dir)
  sources <- read_public_table("source_records.json", project_dir = project_dir)
  comparison_essays <- read_comparison_essays_public(project_dir = project_dir)

  watchlist_candidates <- candidates |>
    dplyr::filter(.data$watchlist_active %in% TRUE) |>
    dplyr::arrange(.data$watchlist_priority, .data$ballot_position)

  if (nrow(watchlist_candidates) == 0 || nrow(claims) == 0) {
    return(list(
      topics = list(),
      sources = sources,
      empty_state = "Todavía no hay material público suficiente para comparar programas entre candidatos."
    ))
  }

  taxonomy_lookup <- taxonomy_root_lookup_public(taxonomy)
  candidate_lookup <- candidate_lookup_public(watchlist_candidates)

  if (length(comparison_essays) > 0) {
    topics <- lapply(comparison_essays, function(essay) {
      topic_id <- normalize_public_scalar(essay$topic_id, default = "tema")
      topic_request <- normalize_topic_request(topic_id, taxonomy_lookup = taxonomy_lookup)
      list(
        topic_id = topic_id,
        topic_label = normalize_public_scalar(essay$title, default = topic_request$label %||% public_topic_label(topic_id)),
        topic_description = "",
        body = normalize_public_scalar(essay$body, default = ""),
        source_ids = normalize_public_vector(essay$source_ids),
        candidate_sections = list()
      )
    })

    return(list(
      topics = topics,
      sources = sources,
      empty_state = NULL
    ))
  }

  comparison_claims <- claims |>
    dplyr::filter(.data$candidate_id %in% watchlist_candidates$candidate_id) |>
    dplyr::filter(.data$claim_type %in% c("policy_proposal", "campaign_status")) |>
    dplyr::left_join(taxonomy_lookup, by = "topic_id") |>
    dplyr::mutate(
      root_topic_id = dplyr::coalesce(.data$root_topic_id, .data$topic_id),
      root_label = dplyr::coalesce(.data$root_label, .data$topic_id),
      root_description = dplyr::coalesce(.data$root_description, ""),
      root_sort_order = dplyr::coalesce(as.integer(.data$root_sort_order), 999L)
    ) |>
    dplyr::filter(!.data$root_topic_id %in% c("vida-publica"))

  if (nrow(comparison_claims) == 0) {
    return(list(
      topics = list(),
      sources = sources,
      empty_state = "Todavía no hay propuestas públicas trazables para comparar en la watchlist."
    ))
  }

  root_topics <- comparison_claims |>
    dplyr::distinct(.data$root_topic_id, .data$root_label, .data$root_description, .data$root_sort_order) |>
    dplyr::arrange(.data$root_sort_order, .data$root_label)

  topics <- lapply(seq_len(nrow(root_topics)), function(index) {
    topic_meta <- root_topics[index, , drop = FALSE]
    root_topic_id <- topic_meta$root_topic_id[[1]]
    label <- topic_meta$root_label[[1]] %||% public_topic_label(root_topic_id)
    topic_claims <- comparison_claims |>
      dplyr::filter(.data$root_topic_id == .env$root_topic_id)
    candidate_sections <- lapply(seq_len(nrow(watchlist_candidates)), function(candidate_index) {
      candidate_row <- watchlist_candidates[candidate_index, , drop = FALSE]
      candidate_id <- candidate_row$candidate_id[[1]]
      candidate_claims <- topic_claims |>
        dplyr::filter(.data$candidate_id == .env$candidate_id) |>
        dplyr::arrange(dplyr::desc(.data$claim_type == "policy_proposal"), dplyr::desc(.data$specificity_score), dplyr::desc(.data$event_date)) |>
        dplyr::distinct(.data$summary_text, .keep_all = TRUE)

      list(
        candidate_id = candidate_id,
        candidate_name = candidate_row$president_name[[1]] %||% candidate_id,
        href = paste0(
          "candidatos/",
          candidate_row$slug[[1]] %||% candidate_id,
          ".html?from=comparador&topic=",
          utils::URLencode(root_topic_id, reserved = TRUE),
          "#propuestas-y-posiciones-publicas"
        ),
        state = if (nrow(candidate_claims) > 0) "documented" else "not_documented",
        claim_rows = candidate_claims
      )
    })

    candidates_with_claims <- vapply(candidate_sections, \(section) if (nrow(section$claim_rows) > 0) section$candidate_name else NA_character_, character(1))
    candidates_with_claims <- stats::na.omit(candidates_with_claims)

    summary <- if (length(candidates_with_claims) == 0) {
      paste0("En ", label, ", todavía falta una propuesta pública suficientemente clara en la watchlist para comparar posiciones de fondo.")
    } else {
      paste0(
        "En ",
        label,
        ", la diferencia política aparece en cómo ",
        paste(candidates_with_claims, collapse = ", "),
        " definen prioridades, instrumentos y límites de acción estatal."
      )
    }

    list(
      topic_id = root_topic_id,
      topic_label = label,
      topic_description = topic_meta$root_description[[1]] %||% "",
      summary = summary,
      candidate_sections = candidate_sections
    )
  })

  list(
    topics = topics,
    sources = sources,
    empty_state = if (length(topics) == 0) "Todavía no hay filas comparativas públicas para el comparador programático." else NULL
  )
}

build_source_library_view_model <- function(project_dir = ".") {
  candidates <- read_candidate_registry_public(project_dir = project_dir) |>
    dplyr::select("candidate_id", "president_name")
  promoted_sources <- read_public_table("source_records.json", project_dir = project_dir) |>
    ensure_public_table_columns(list(
      source_id = NA_character_,
      candidate_id = NA_character_,
      published_at = NA_character_,
      source_tier = NA_character_,
      source_type = NA_character_,
      source_name = NA_character_,
      url = NA_character_,
      title = NA_character_,
      confidence = NA_real_
    )) |>
    dplyr::left_join(candidates, by = "candidate_id") |>
    dplyr::mutate(
      library_status = "promoted",
      library_status_label = "Integrada al sistema"
    ) |>
    dplyr::arrange(dplyr::desc(.data$published_at), .data$title)
  pending_sources <- read_public_table("manual_source_library.json", project_dir = project_dir) |>
    ensure_public_table_columns(list(
      entry_id = NA_character_,
      candidate_id = NA_character_,
      source_name = NA_character_,
      source_tier = NA_character_,
      source_type = NA_character_,
      url = NA_character_,
      title = NA_character_,
      published_at = NA_character_,
      status = NA_character_,
      status_reason = NA_character_,
      candidate_confidence = NA_real_,
      source_files = NA_character_
    )) |>
    dplyr::left_join(candidates, by = "candidate_id") |>
    dplyr::mutate(
      president_name = dplyr::coalesce(.data$president_name, "Por clasificar"),
      library_status_label = "Pendiente de clasificar"
    ) |>
    dplyr::arrange(dplyr::desc(.data$candidate_confidence), dplyr::desc(.data$published_at), .data$title)

  list(
    promoted_sources = promoted_sources,
    pending_sources = pending_sources,
    empty_state = if (nrow(promoted_sources) == 0 && nrow(pending_sources) == 0) {
      "Todavía no hay fuentes públicas ni hallazgos manuales válidos cargados."
    } else {
      NULL
    }
  )
}
