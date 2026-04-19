read_processed_table <- function(filename, project_dir = ".") {
  path <- file.path(project_dir, "data", "processed", filename)

  if (!file.exists(path)) {
    return(tibble::tibble())
  }

  readr::read_csv(path, show_col_types = FALSE)
}

read_public_json <- function(filename, project_dir = ".") {
  path <- file.path(project_dir, "data", "public", filename)

  if (!file.exists(path)) {
    return(list())
  }

  jsonlite::read_json(path, simplifyVector = TRUE)
}

read_candidate_registry_public <- function(project_dir = ".") {
  readr::read_csv(
    file.path(project_dir, "config", "candidate_registry.csv"),
    show_col_types = FALSE
  ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(
          c(
            "candidate_id",
            "slug",
            "president_name",
            "vicepresident_name",
            "coalition",
            "party_or_group",
            "source_url",
            "tracking_note"
          )
        ),
        as.character
      ),
      dplyr::across(dplyr::any_of(c("ballot_position", "watchlist_priority")), as.integer),
      dplyr::across(dplyr::any_of("watchlist_active"), as.logical),
      dplyr::across(dplyr::any_of("source_date"), as.Date)
    )
}

`%||%` <- function(x, y) {
  if (length(x) == 0 || all(is.na(x)) || identical(x, "")) {
    return(y)
  }

  x
}

read_taxonomy_public <- function(project_dir = ".") {
  readr::read_csv(
    file.path(project_dir, "config", "taxonomy_v1.csv"),
    show_col_types = FALSE
  ) |>
    dplyr::mutate(
      parent_topic_id = dplyr::na_if(as.character(.data$parent_topic_id), ""),
      sort_order = as.integer(.data$sort_order)
    )
}

read_ideology_rules_public <- function(project_dir = ".") {
  path <- file.path(project_dir, "config", "ideology_rules.csv")

  if (!file.exists(path)) {
    return(tibble::tibble())
  }

  readr::read_csv(path, show_col_types = FALSE)
}

emit_callout <- function(text, type = "note") {
  cat(
    paste0(
      "::: {.callout-", type, "}\n",
      text,
      "\n:::\n"
    )
  )
}

safe_kable <- function(data, columns = NULL, col.names = NULL) {
  if (!is.null(columns)) {
    data <- data |>
      dplyr::select(dplyr::all_of(columns))
  }

  if (nrow(data) == 0) {
    emit_callout("Todavía no hay datos públicos suficientes para esta sección.", "note")
    return(invisible(NULL))
  }

  print(knitr::kable(data, col.names = col.names))
}

escape_html <- function(text) {
  if (length(text) == 0 || all(is.na(text))) {
    return("")
  }

  escaped <- gsub("&", "&amp;", text, fixed = TRUE)
  escaped <- gsub("<", "&lt;", escaped, fixed = TRUE)
  escaped <- gsub(">", "&gt;", escaped, fixed = TRUE)
  gsub('"', "&quot;", escaped, fixed = TRUE)
}

html_link <- function(label, url) {
  if (is.na(url) || identical(url, "")) {
    return(escape_html(label))
  }

  paste0('<a href="', escape_html(url), '">', escape_html(label), "</a>")
}

format_public_date <- function(value) {
  if (length(value) == 0 || all(is.na(value))) {
    return("fecha sin precisar")
  }

  parsed_date <- if (inherits(value, "POSIXct")) {
    as.Date(value)
  } else if (inherits(value, "Date")) {
    value
  } else {
    suppressWarnings(as.Date(value))
  }

  if (is.na(parsed_date)) {
    return(as.character(value[[1]]))
  }

  months_es <- c(
    "enero", "febrero", "marzo", "abril", "mayo", "junio",
    "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
  )

  paste0(
    as.integer(format(parsed_date, "%d")),
    " de ",
    months_es[as.integer(format(parsed_date, "%m"))],
    " de ",
    format(parsed_date, "%Y")
  )
}

normalize_sentence <- function(text) {
  if (length(text) == 0 || all(is.na(text))) {
    return("")
  }

  cleaned <- stringr::str_squish(as.character(text[[1]]))
  cleaned <- gsub("[[:space:]]+$", "", cleaned)
  cleaned <- gsub("[.]+$", "", cleaned)
  cleaned
}

source_row_by_id <- function(source_id, sources) {
  if (is.na(source_id) || nrow(sources) == 0) {
    return(tibble::tibble())
  }

  target_source_id <- source_id

  sources |>
    dplyr::filter(.data$source_id == target_source_id) |>
    dplyr::slice_head(n = 1)
}

source_anchor_id <- function(source_id) {
  paste0("source-", gsub("[^A-Za-z0-9_-]", "-", source_id))
}

source_reference_html <- function(source_row) {
  if (nrow(source_row) == 0) {
    return("fuente no disponible")
  }

  html_link(source_row$source_name[[1]], source_row$url[[1]])
}

taxonomy_root_lookup <- function(taxonomy) {
  if (nrow(taxonomy) == 0) {
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
    find_root(parent_id)
  }

  taxonomy |>
    dplyr::mutate(
      root_topic_id = purrr::map_chr(.data$topic_id, find_root),
      topic_label = .data$label_public,
      root_label = unname(label_lookup[.data$root_topic_id]),
      root_description = unname(description_lookup[.data$root_topic_id]),
      root_sort_order = unname(sort_lookup[.data$root_topic_id])
    ) |>
    dplyr::select(
      .data$topic_id,
      .data$topic_label,
      .data$root_topic_id,
      .data$root_label,
      .data$root_description,
      .data$root_sort_order
    )
}

analysis_type_label <- function(value) {
  dplyr::case_when(
    value == "contradiccion_interna" ~ "Contradicción interna",
    value == "cambio_de_postura" ~ "Cambio de postura",
    value == "vacio_de_implementacion" ~ "Vacío de implementación",
    value == "desalineacion_programa_vs_declaracion" ~ "Desalineación entre programa y declaración",
    TRUE ~ stringr::str_replace_all(value %||% "Análisis", "_", " ")
  )
}

confidence_public_label <- function(value) {
  if (is.na(value)) {
    return("sin puntaje")
  }

  dplyr::case_when(
    value >= 0.9 ~ "alta",
    value >= 0.75 ~ "media-alta",
    value >= 0.6 ~ "media",
    TRUE ~ "baja"
  )
}

source_links_from_ids <- function(source_ids_text, sources) {
  if (is.na(source_ids_text) || identical(source_ids_text, "") || nrow(sources) == 0) {
    return("sin enlaces adicionales")
  }

  source_ids <- unique(strsplit(source_ids_text, "\\|")[[1]])
  rows <- sources |>
    dplyr::filter(.data$source_id %in% source_ids) |>
    dplyr::distinct(.data$source_id, .keep_all = TRUE)

  if (nrow(rows) == 0) {
    return("sin enlaces adicionales")
  }

  paste(vapply(seq_len(nrow(rows)), function(i) {
    source_reference_html(rows[i, , drop = FALSE])
  }, character(1)), collapse = ", ")
}

claim_paragraph_html <- function(claim_row, sources) {
  source_row <- source_row_by_id(claim_row$source_id[[1]], sources)
  date_text <- format_public_date(claim_row$event_date[[1]])
  position_text <- normalize_sentence(claim_row$position_text[[1]] %||% claim_row$summary_text[[1]])
  summary_text <- normalize_sentence(claim_row$summary_text[[1]])
  summary_extra <- if (
    identical(summary_text, "") ||
    identical(tolower(summary_text), tolower(position_text))
  ) {
    ""
  } else {
    paste0(" ", summary_text, ".")
  }

  topic_prefix <- if (
    "topic_label" %in% names(claim_row) &&
    !is.na(claim_row$topic_label[[1]]) &&
    !is.na(claim_row$root_label[[1]]) &&
    claim_row$topic_label[[1]] != claim_row$root_label[[1]]
  ) {
    paste0("En ", tolower(claim_row$topic_label[[1]]), ", ")
  } else {
    ""
  }

  paste0(
    '<p class="narrative-paragraph"><span class="narrative-date">',
    escape_html(date_text),
    ".</span> ",
    escape_html(topic_prefix),
    escape_html(position_text),
    ".",
    escape_html(summary_extra),
    ' <span class="narrative-source">Fuente: ',
    source_reference_html(source_row),
    ".</span></p>"
  )
}

analysis_paragraph_html <- function(note_row, sources) {
  support_links <- source_links_from_ids(note_row$source_ids[[1]], sources)

  paste0(
    '<p class="narrative-paragraph"><strong>',
    escape_html(analysis_type_label(note_row$analysis_type[[1]])),
    ".</strong> ",
    escape_html(normalize_sentence(note_row$public_reasoning_summary[[1]])),
    '. <span class="narrative-meta">Confianza ',
    escape_html(confidence_public_label(note_row$confidence[[1]])),
    ".</span> <span class=\"narrative-source\">Sustento: ",
    support_links,
    ".</span></p>"
  )
}

ideology_family_class <- function(label) {
  dplyr::case_when(
    label %in% c("Izquierda", "Centroizquierda") ~ "left",
    label %in% c("Derecha", "Centroderecha") ~ "right",
    label == "Centro" ~ "center",
    TRUE ~ "unknown"
  )
}

emit_candidate_header <- function(candidate_meta, candidate_dossier) {
  if (nrow(candidate_meta) == 0) {
    emit_callout("No hay metadatos públicos para este candidato.", "warning")
    return(invisible(NULL))
  }

  dossier_row <- if (nrow(candidate_dossier) == 0) tibble::tibble() else candidate_dossier[1, , drop = FALSE]
  ideology_label <- dossier_row$ideology_label[[1]] %||% "Evidencia insuficiente"
  ideology_class <- ideology_family_class(ideology_label)
  last_event <- dossier_row$last_event_date[[1]] %||% NA
  total_claims <- dossier_row$total_claims[[1]] %||% 0
  total_sources <- dossier_row$total_sources[[1]] %||% 0
  total_notes <- dossier_row$total_analysis_notes[[1]] %||% 0

  cat(
    paste0(
      '<div class="profile-hero">',
      '<div class="profile-hero__main">',
      '<p class="eyebrow">Tarjetón ', candidate_meta$ballot_position[[1]], "</p>",
      '<p class="profile-hero__meta"><strong>Vicepresidencia:</strong> ', escape_html(candidate_meta$vicepresident_name[[1]]), "</p>",
      '<p class="profile-hero__meta"><strong>Watchlist activa:</strong> ', ifelse(isTRUE(candidate_meta$watchlist_active[[1]]), "Sí", "No"), "</p>",
      '<p class="profile-hero__meta"><strong>Último evento público:</strong> ', escape_html(format_public_date(last_event)), "</p>",
      "</div>",
      '<div class="profile-hero__aside">',
      '<span class="spectrum-chip spectrum-chip--', ideology_class, '">', escape_html(ideology_label), "</span>",
      '<p class="profile-hero__meta"><strong>Hallazgos:</strong> ', total_claims, "</p>",
      '<p class="profile-hero__meta"><strong>Fuentes:</strong> ', total_sources, "</p>",
      '<p class="profile-hero__meta"><strong>Notas IA:</strong> ', total_notes, "</p>",
      "</div>",
      "</div>"
    )
  )
}

emit_candidate_ideology <- function(candidate_dossier) {
  if (nrow(candidate_dossier) == 0) {
    emit_callout("Todavía no hay suficiente material estructurado para ubicar esta candidatura en el espectro.", "note")
    return(invisible(NULL))
  }

  dossier_row <- candidate_dossier[1, , drop = FALSE]
  ideology_label <- dossier_row$ideology_label[[1]] %||% "Evidencia insuficiente"
  ideology_rationale <- dossier_row$ideology_rationale[[1]] %||% ""
  signal_count <- dossier_row$ideology_signal_count[[1]] %||% 0
  score_text <- if (!is.na(dossier_row$ideology_score[[1]] %||% NA)) {
    format(round(dossier_row$ideology_score[[1]], 2), nsmall = 2)
  } else {
    "sin cálculo"
  }

  cat(
    paste0(
      '<div class="prose-card">',
      '<p class="narrative-paragraph"><strong>Ubicación analítica provisional:</strong> ',
      escape_html(ideology_label),
      '. Este indicador se calcula con reglas públicas sobre políticas ya mapeadas, ponderadas por trazabilidad de fuente y señal ideológica. ',
      'Se apoyó en ',
      signal_count,
      ' señales programáticas y arrojó un puntaje agregado de ',
      escape_html(score_text),
      ".</p>",
      '<p class="narrative-paragraph">',
      escape_html(ideology_rationale),
      ' Este índice no reemplaza la lectura directa del programa, pero sí da una orientación rápida sobre hacia dónde empujan sus propuestas concretas.</p>',
      "</div>"
    )
  )
}

emit_candidate_background <- function(candidate_claims, candidate_sources) {
  background_claims <- candidate_claims |>
    dplyr::filter(.data$claim_type %in% c("biography", "campaign_status")) |>
    dplyr::arrange(dplyr::desc(.data$event_date))

  if (nrow(background_claims) == 0) {
    emit_callout("Todavía no hay suficiente contexto público narrado para esta candidatura.", "note")
    return(invisible(NULL))
  }

  paragraphs <- vapply(seq_len(nrow(background_claims)), function(index) {
    claim_paragraph_html(background_claims[index, , drop = FALSE], candidate_sources)
  }, character(1))

  cat(paste0('<div class="prose-card">', paste(paragraphs, collapse = ""), "</div>"))
}

emit_candidate_policy_sections <- function(candidate_claims, candidate_sources, taxonomy) {
  policy_claims <- candidate_claims |>
    dplyr::filter(.data$claim_type == "policy_proposal")

  if (nrow(policy_claims) == 0) {
    emit_callout("Todavía no hay propuestas públicas trazables para este candidato.", "note")
    return(invisible(NULL))
  }

  taxonomy_lookup <- taxonomy_root_lookup(taxonomy)
  enriched_claims <- policy_claims |>
    dplyr::left_join(taxonomy_lookup, by = "topic_id") |>
    dplyr::mutate(
      root_topic_id = dplyr::coalesce(.data$root_topic_id, .data$topic_id),
      root_label = dplyr::coalesce(.data$root_label, .data$topic_id),
      root_description = dplyr::coalesce(.data$root_description, ""),
      root_sort_order = dplyr::coalesce(.data$root_sort_order, 999L),
      topic_label = dplyr::coalesce(.data$topic_label, .data$topic_id)
    ) |>
    dplyr::arrange(.data$root_sort_order, dplyr::desc(.data$event_date))

  sections <- enriched_claims |>
    dplyr::group_split(.data$root_topic_id)

  rendered_sections <- vapply(sections, function(section_claims) {
    root_label <- section_claims$root_label[[1]]
    root_description <- section_claims$root_description[[1]]
    paragraphs <- vapply(seq_len(nrow(section_claims)), function(index) {
      claim_paragraph_html(section_claims[index, , drop = FALSE], candidate_sources)
    }, character(1))

    paste0(
      '<section class="topic-section">',
      "<h3>", escape_html(root_label), "</h3>",
      if (!is.na(root_description) && root_description != "") {
        paste0('<p class="topic-section__intro">', escape_html(root_description), "</p>")
      } else {
        ""
      },
      '<div class="prose-card">',
      paste(paragraphs, collapse = ""),
      "</div>",
      "</section>"
    )
  }, character(1))

  cat(paste(rendered_sections, collapse = "\n"))
}

emit_candidate_analysis <- function(candidate_analysis, candidate_sources) {
  if (nrow(candidate_analysis) == 0) {
    emit_callout("Aún no hay notas analíticas públicas para esta candidatura.", "note")
    return(invisible(NULL))
  }

  notes <- candidate_analysis |>
    dplyr::arrange(dplyr::desc(.data$confidence))

  paragraphs <- vapply(seq_len(nrow(notes)), function(index) {
    analysis_paragraph_html(notes[index, , drop = FALSE], candidate_sources)
  }, character(1))

  cat(paste0('<div class="prose-card">', paste(paragraphs, collapse = ""), "</div>"))
}

emit_candidate_source_library <- function(candidate_sources) {
  if (nrow(candidate_sources) == 0) {
    emit_callout("Todavía no hay fuentes públicas cargadas para esta candidatura.", "note")
    return(invisible(NULL))
  }

  source_items <- candidate_sources |>
    dplyr::distinct(.data$source_id, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(.data$published_at))

  items <- vapply(seq_len(nrow(source_items)), function(index) {
    row <- source_items[index, , drop = FALSE]
    paste0(
      '<li id="', source_anchor_id(row$source_id[[1]]), '">',
      '<strong>', html_link(row$source_name[[1]], row$url[[1]]), "</strong>",
      ". ",
      escape_html(row$title[[1]] %||% "Sin título visible"),
      ". ",
      escape_html(format_public_date(row$published_at[[1]])),
      '. <span class="narrative-meta">Tier: ',
      escape_html(row$source_tier[[1]] %||% "sin clasificar"),
      ".</span></li>"
    )
  }, character(1))

  cat(paste0('<ol class="source-list">', paste(items, collapse = ""), "</ol>"))
}

emit_comparison_sections <- function(claims, candidates, taxonomy, sources, dossiers) {
  policy_claims <- claims |>
    dplyr::filter(.data$claim_type == "policy_proposal")

  if (nrow(policy_claims) == 0) {
    emit_callout("Todavía no hay material público suficiente para comparar propuestas entre candidatos.", "note")
    return(invisible(NULL))
  }

  taxonomy_lookup <- taxonomy_root_lookup(taxonomy)
  ideology_lookup <- dossiers |>
    dplyr::select(.data$candidate_id, .data$ideology_label)

  candidate_order <- candidates |>
    dplyr::mutate(
      watchlist_rank = dplyr::if_else(.data$watchlist_active %in% TRUE, .data$watchlist_priority, 999L)
    ) |>
    dplyr::arrange(.data$watchlist_rank, .data$ballot_position)

  enriched_claims <- policy_claims |>
    dplyr::left_join(taxonomy_lookup, by = "topic_id") |>
    dplyr::left_join(
      candidate_order |>
        dplyr::select(.data$candidate_id, .data$president_name, .data$watchlist_active, .data$watchlist_priority, .data$ballot_position),
      by = "candidate_id"
    ) |>
    dplyr::left_join(ideology_lookup, by = "candidate_id") |>
    dplyr::mutate(
      root_topic_id = dplyr::coalesce(.data$root_topic_id, .data$topic_id),
      root_label = dplyr::coalesce(.data$root_label, .data$topic_id),
      root_description = dplyr::coalesce(.data$root_description, ""),
      root_sort_order = dplyr::coalesce(.data$root_sort_order, 999L),
      topic_label = dplyr::coalesce(.data$topic_label, .data$topic_id)
    )

  topic_sections <- enriched_claims |>
    dplyr::arrange(.data$root_sort_order, .data$root_label, dplyr::desc(.data$watchlist_active), .data$watchlist_priority, .data$ballot_position) |>
    dplyr::group_split(.data$root_topic_id)

  rendered_topics <- vapply(topic_sections, function(topic_claims) {
    topic_claims <- topic_claims |>
      dplyr::arrange(dplyr::desc(.data$watchlist_active), .data$watchlist_priority, .data$ballot_position, dplyr::desc(.data$event_date))

    root_label <- topic_claims$root_label[[1]]
    root_description <- topic_claims$root_description[[1]]
    present_ids <- unique(topic_claims$candidate_id)
    missing_names <- candidate_order |>
      dplyr::filter(!.data$candidate_id %in% present_ids) |>
      dplyr::pull(.data$president_name)

    candidate_cards <- topic_claims |>
      dplyr::group_split(.data$candidate_id) |>
      purrr::map_chr(function(candidate_group) {
        candidate_name <- candidate_group$president_name[[1]]
        ideology_label <- candidate_group$ideology_label[[1]] %||% "Evidencia insuficiente"
        ideology_class <- ideology_family_class(ideology_label)
        paragraphs <- vapply(seq_len(nrow(candidate_group)), function(index) {
          claim_paragraph_html(candidate_group[index, , drop = FALSE], sources)
        }, character(1))

        paste0(
          '<article class="comparison-card">',
          "<h3>", escape_html(candidate_name), "</h3>",
          '<p class="comparison-card__meta"><span class="spectrum-chip spectrum-chip--', ideology_class, '">',
          escape_html(ideology_label),
          "</span></p>",
          paste(paragraphs, collapse = ""),
          "</article>"
        )
      }) |>
      paste(collapse = "")

    paste0(
      '<section class="topic-section">',
      "<h2>", escape_html(root_label), "</h2>",
      if (!is.na(root_description) && root_description != "") {
        paste0('<p class="topic-section__intro">', escape_html(root_description), "</p>")
      } else {
        ""
      },
      '<div class="comparison-grid">', candidate_cards, "</div>",
      if (length(missing_names) > 0) {
        paste0(
          '<p class="topic-section__footnote"><strong>Sin hallazgos públicos trazables todavía en esta categoría:</strong> ',
          escape_html(paste(missing_names, collapse = ", ")),
          ".</p>"
        )
      } else {
        ""
      },
      "</section>"
    )
  }, character(1))

  cat(paste(rendered_topics, collapse = "\n"))
}
