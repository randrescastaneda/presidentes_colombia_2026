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
    TRUE ~ "evidencia insuficiente"
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
    grepl("sin evidencia suficiente|sin base suficiente|evidencia insuficiente", normalized, perl = TRUE)
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
        watchlist_priority = row$watchlist_priority[[1]] %||% NA_integer_
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
      " cuenta hoy con evidencia suficiente para una lectura parcial; el resto sigue siendo desigual."
    ),
    TRUE ~ paste0(
      "En ",
      topic_label,
      ", la evidencia pública sigue siendo insuficiente para sostener una comparación firme entre candidaturas."
    )
  )
}

build_homepage_comparison_blocks <- function(comparison_report, candidates, limit = 3) {
  if (is.null(comparison_report)) {
    return(list())
  }

  topic_rows <- normalize_public_collection(comparison_report$topic_comparison)
  if (length(topic_rows) == 0) {
    return(list())
  }

  candidate_lookup <- candidate_lookup_public(candidates)

  blocks <- lapply(topic_rows, function(topic_row) {
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
        topic_or_axis = normalize_public_scalar(topic_row$topic_id, default = "sin-tema"),
        section_anchor = paste0("topic-", normalize_public_scalar(topic_row$topic_id, default = "sin-tema")),
        candidate_ids = known_supported_ids,
        candidate_destinations = unname(lapply(known_supported_ids, function(candidate_id) {
          candidate_meta <- candidate_lookup[[candidate_id]]
          list(
            candidate_id = candidate_id,
            href = paste0(
              "candidatos/",
              candidate_meta$slug %||% candidate_id,
              ".html?from=homepage&topic=",
              utils::URLencode(normalize_public_scalar(topic_row$topic_id, default = "sin-tema"), reserved = TRUE),
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

build_homepage_roster <- function(candidates) {
  if (nrow(candidates) == 0) {
    return(list())
  }

  ordered <- candidates |>
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
      href = paste0("candidatos/", row$slug[[1]] %||% row$candidate_id[[1]], ".html")
    )
  })
}

build_homepage_view_model <- function(project_dir = ".", comparison_limit = 3) {
  candidates <- read_candidate_registry_public(project_dir = project_dir)
  editorial_packages <- read_editorial_packages_public(project_dir = project_dir)
  homepage_brief <- find_public_artifact(editorial_packages, "homepage_brief")
  daily_update <- find_public_artifact(editorial_packages, "daily_update")
  comparison_report <- read_public_json("comparison_report.json", project_dir = project_dir)
  validation_report <- read_public_json("validation_report.json", project_dir = project_dir)
  validation_status <- read_processed_table("validation_status.csv", project_dir = project_dir)

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
    roster = build_homepage_roster(candidates),
    empty_state = if (length(comparison_blocks) == 0) {
      "Todavía no hay suficientes diferencias comparables para destacar en portada."
    } else {
      NULL
    }
  )
}
