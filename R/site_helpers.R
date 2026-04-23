site_helper_env <- environment()

if (!exists("build_homepage_view_model", envir = site_helper_env, inherits = TRUE)) {
  current_project_dir <- get0("project_dir", envir = site_helper_env, inherits = TRUE, ifnotfound = NULL)
  candidate_paths <- unique(stats::na.omit(c(
    if (!is.null(current_project_dir)) file.path(current_project_dir, "R", "site_public_view_models.R") else NA_character_,
    "R/site_public_view_models.R",
    "site_public_view_models.R"
  )))

  existing_path <- candidate_paths[file.exists(candidate_paths)][1]
  if (length(existing_path) == 1 && !is.na(existing_path)) {
    source(existing_path, local = site_helper_env)
  }
}

rm(site_helper_env)

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

read_program_documents_public <- function(project_dir = ".") {
  read_processed_table("program_documents.csv", project_dir = project_dir)
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

emit_candidate_policy_sections <- function(candidate_policy, candidate_sources = NULL, taxonomy = NULL) {
  if (is.data.frame(candidate_policy)) {
    policy_claims <- candidate_policy |>
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
    return(invisible(NULL))
  }

  if (is.null(candidate_policy) || isTRUE(candidate_policy$empty_state)) {
    emit_callout("Todavía no hay propuestas públicas trazables para este candidato.", "note")
    return(invisible(NULL))
  }

  render_topic_sections <- function(sections, bucket_id, heading, intro, sources) {
    if (length(sections) == 0) {
      return(
        paste0(
          '<div class="prose-card" data-policy-bucket="', bucket_id, '">',
          '<h3>', escape_html(heading), '</h3>',
          '<p class="narrative-paragraph">', escape_html(intro), "</p>",
          "</div>"
        )
      )
    }

    rendered_sections <- vapply(sections, function(section) {
      claim_rows <- section$claim_rows
      paragraphs <- vapply(seq_len(nrow(claim_rows)), function(index) {
        claim_paragraph_html(claim_rows[index, , drop = FALSE], sources)
      }, character(1))

      paste0(
        '<section class="topic-section" data-topic-id="', escape_html(section$topic_id), '" data-topic-state="', escape_html(section$state), '">',
        '<div class="comparison-highlight__header">',
        '<h3>', escape_html(section$topic_label), '</h3>',
        '<span class="evidence-chip evidence-chip--', if (identical(section$state, "comparable")) "solid" else "partial", '">',
        escape_html(section$state_label),
        '</span>',
        '</div>',
        if (!is.na(section$topic_description) && section$topic_description != "") {
          paste0('<p class="topic-section__intro">', escape_html(section$topic_description), "</p>")
        } else {
          ""
        },
        '<div class="prose-card">',
        paste(paragraphs, collapse = ""),
        "</div>",
        "</section>"
      )
    }, character(1))

    paste0(
      '<div class="policy-topic-bucket" data-policy-bucket="', bucket_id, '">',
      '<div class="prose-card">',
      '<h3>', escape_html(heading), '</h3>',
      '<p class="narrative-paragraph">', escape_html(intro), "</p>",
      '</div>',
      paste(rendered_sections, collapse = "\n"),
      "</div>"
    )
  }

  cat(
    paste(
      c(
        render_topic_sections(
          candidate_policy$comparable_sections %||% list(),
          bucket_id = "comparable",
          heading = "Propuestas comparables",
          intro = "Estas propuestas ya sostienen una comparación pública útil con otras candidaturas.",
          sources = candidate_policy$source_library %||% tibble::tibble()
        ),
        render_topic_sections(
          candidate_policy$documented_sections %||% list(),
          bucket_id = "documented-only",
          heading = "Propuestas documentadas aún no comparables",
          intro = "Estas propuestas ya son publicables y trazables, pero todavía no tienen base suficiente para una comparación transversal firme.",
          sources = candidate_policy$source_library %||% tibble::tibble()
        )
      ),
      collapse = "\n"
    )
  )
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

program_document_href <- function(path_or_url, project_dir = ".") {
  value <- path_or_url %||% ""
  if (is.na(value) || identical(value, "")) {
    return("")
  }

  if (grepl("^(https?|data):", value, perl = TRUE)) {
    return(value)
  }

  resolved <- if (grepl("^/", value, perl = TRUE)) {
    normalizePath(value, winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(file.path(project_dir, value), winslash = "/", mustWork = FALSE)
  }

  if (is.na(resolved) || identical(resolved, "")) {
    return("")
  }

  if (!grepl("^/", resolved, perl = TRUE)) {
    return(resolved)
  }

  project_root <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  if (startsWith(resolved, paste0(project_root, "/"))) {
    return(sub(paste0("^", stringr::fixed(project_root), "/?"), "", resolved))
  }

  resolved
}

program_document_link_html <- function(path_or_url, label, project_dir = ".") {
  value <- path_or_url %||% ""
  if (is.na(value) || identical(value, "")) {
    return("")
  }

  href <- program_document_href(value, project_dir = project_dir)

  if (is.na(href) || identical(href, "")) {
    return("")
  }

  paste0('<a class="card-link" href="', escape_html(href), '">', escape_html(label), "</a>")
}

emit_candidate_program_documents <- function(candidate_program_documents, project_dir = ".") {
  if (nrow(candidate_program_documents) == 0) {
    emit_callout("Todavía no hay documento oficial del programa cargado para esta candidatura.", "note")
    return(invisible(NULL))
  }

  rows <- candidate_program_documents |>
    dplyr::distinct(.data$document_id, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(.data$is_primary), dplyr::desc(.data$published_at))

  cards <- vapply(seq_len(nrow(rows)), function(index) {
    row <- rows[index, , drop = FALSE]
    actions <- c(
      program_document_link_html(row$pdf_path[[1]], "Abrir PDF", project_dir = project_dir),
      program_document_link_html(row$markdown_path[[1]], "Abrir Markdown", project_dir = project_dir),
      program_document_link_html(row$download_url[[1]], "Fuente oficial", project_dir = project_dir)
    )
    actions <- actions[actions != ""]

    paste0(
      '<article class="program-document-card">',
      '<div class="program-document-card__header">',
      '<p class="eyebrow">', if (isTRUE(row$is_primary[[1]])) "Documento base" else "Documento complementario", "</p>",
      '<span class="evidence-chip evidence-chip--', if (isTRUE(row$is_primary[[1]])) "solid" else "partial", '">',
      escape_html(row$coverage_label[[1]] %||% "Cobertura pendiente"),
      "</span>",
      "</div>",
      "<h3>", escape_html(row$title[[1]] %||% row$document_id[[1]]), "</h3>",
      '<p class="comparison-card__meta"><strong>Rol:</strong> ', escape_html(row$document_role[[1]] %||% "programa"), "</p>",
      '<p class="comparison-card__meta"><strong>Fecha:</strong> ', escape_html(format_public_date(row$published_at[[1]])), "</p>",
      '<p class="comparison-card__meta"><strong>Estado:</strong> ', escape_html(row$coverage_label[[1]] %||% "Sin cobertura"), "</p>",
      '<p class="comparison-card__meta"><strong>Descarga:</strong> ', escape_html(row$download_status[[1]] %||% "sin registrar"), "</p>",
      '<p class="comparison-card__meta"><strong>Conversión:</strong> ', escape_html(row$conversion_status[[1]] %||% "sin registrar"), "</p>",
      if (length(actions) > 0) {
        paste0('<div class="program-document-card__actions">', paste(actions, collapse = " · "), "</div>")
      } else {
        '<p class="comparison-card__meta">Todavía no hay enlaces públicos disponibles para este documento.</p>'
      },
      if (!is.na(row$notes[[1]] %||% NA_character_) && row$notes[[1]] %||% "" != "") {
        paste0('<p class="topic-section__footnote">', escape_html(row$notes[[1]]), "</p>")
      } else {
        ""
      },
      "</article>"
    )
  }, character(1))

  cat(paste0('<div class="program-document-grid">', paste(cards, collapse = ""), "</div>"))
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

emit_comparison_sections <- function(comparison_model, candidates = NULL, taxonomy = NULL, program_documents = NULL) {
  if (!is.null(comparison_model$topics)) {
    if (length(comparison_model$topics) == 0) {
      emit_callout(comparison_model$empty_state %||% "Todavía no hay material público suficiente para comparar programas entre candidatos.", "note")
      return(invisible(NULL))
    }

    rendered_topics <- vapply(comparison_model$topics, function(topic) {
      cards <- vapply(topic$candidate_cards, function(card) {
        primary_doc <- card$primary_document
        links <- c(
          if (!is.null(primary_doc$pdf_path %||% NULL) && (primary_doc$pdf_path %||% "") != "") program_document_link_html(primary_doc$pdf_path, "PDF", project_dir = ".") else "",
          if (!is.null(primary_doc$markdown_path %||% NULL) && (primary_doc$markdown_path %||% "") != "") program_document_link_html(primary_doc$markdown_path, "Markdown", project_dir = ".") else "",
          if (!is.null(primary_doc$download_url %||% NULL) && (primary_doc$download_url %||% "") != "") program_document_link_html(primary_doc$download_url, "Fuente", project_dir = ".") else ""
        )
        links <- links[links != ""]

        paste0(
          '<article class="comparison-matrix-card">',
          "<h3>", escape_html(card$candidate_name), "</h3>",
          '<p class="comparison-card__meta"><strong>Destino en ficha:</strong> ',
          if ((card$href %||% "") != "") {
            paste0('<a class="card-link" href="', escape_html(card$href), '">', escape_html(card$destination_state_label), "</a>")
          } else {
            escape_html(card$destination_state_label)
          },
          "</p>",
          '<dl class="comparison-matrix-card__stats">',
          '<div><dt>Prioridad</dt><dd>', escape_html(card$priority), "</dd></div>",
          '<div><dt>Instrumento</dt><dd>', escape_html(card$instrument), "</dd></div>",
          '<div><dt>Especificidad</dt><dd>', escape_html(card$specificity), "</dd></div>",
          '<div><dt>Coherencia</dt><dd>', escape_html(card$coherence), "</dd></div>",
          '<div><dt>Factibilidad</dt><dd>', escape_html(card$feasibility), "</dd></div>",
          "</dl>",
          if (!is.null(primary_doc)) {
            paste0('<p class="comparison-card__meta"><strong>Documento base:</strong> ', escape_html(primary_doc$title %||% primary_doc$document_id %||% "sin documento oficial cargado"), "</p>")
          } else {
            '<p class="comparison-card__meta"><strong>Documento base:</strong> sin documento oficial cargado.</p>'
          },
          if (length(links) > 0) {
            paste0('<div class="comparison-matrix-card__links">', paste(links, collapse = " · "), "</div>")
          } else {
            ""
          },
          "</article>"
        )
      }, character(1))

      paste0(
        '<section class="topic-section" data-topic-id="', escape_html(topic$topic_id), '">',
        '<div class="comparison-highlight__header">',
        "<h2>", escape_html(topic$topic_label), "</h2>",
        '<span class="evidence-chip evidence-chip--', escape_html(topic$evidence_state), '">', escape_html(topic$public_label), '</span>',
        '</div>',
        if (!is.na(topic$topic_description) && topic$topic_description != "") {
          paste0('<p class="topic-section__intro">', escape_html(topic$topic_description), "</p>")
        } else {
          ""
        },
        '<p class="narrative-paragraph">', escape_html(topic$summary), "</p>",
        '<div class="comparison-matrix-grid">', paste(cards, collapse = ""), "</div>",
        "</section>"
      )
    }, character(1))

    cat(paste(rendered_topics, collapse = "\n"))
    return(invisible(NULL))
  }

  if (is.null(comparison_model) || length(comparison_model) == 0) {
    emit_callout("Todavía no hay material público suficiente para comparar programas entre candidatos.", "note")
    return(invisible(NULL))
  }

  topic_rows <- normalize_public_collection(comparison_model$topic_comparison)
  if (length(topic_rows) == 0) {
    emit_callout("Todavía no hay filas comparativas públicas para el comparador programático.", "note")
    return(invisible(NULL))
  }

  taxonomy_lookup <- taxonomy_root_lookup(taxonomy)
  topic_lookup <- taxonomy_lookup |>
    dplyr::distinct(.data$root_topic_id, .data$root_label, .data$root_description, .data$root_sort_order)

  candidate_order <- candidates |>
    dplyr::mutate(
      watchlist_rank = dplyr::if_else(.data$watchlist_active %in% TRUE, .data$watchlist_priority, 999L)
    ) |>
    dplyr::arrange(.data$watchlist_rank, .data$ballot_position)

  documents_by_candidate <- split(program_documents, program_documents$candidate_id)

  rendered_topics <- vapply(topic_rows, function(topic_row) {
    topic_id <- normalize_public_scalar(topic_row$topic_id, default = "tema")
    topic_meta <- topic_lookup |>
      dplyr::filter(.data$root_topic_id == topic_id) |>
      dplyr::slice_head(n = 1)

    topic_label <- if (nrow(topic_meta) == 0) public_topic_label(topic_id) else topic_meta$root_label[[1]]
    topic_description <- if (nrow(topic_meta) == 0) "" else topic_meta$root_description[[1]] %||% ""
    candidate_rows <- normalize_public_collection(topic_row$candidate_rows)

    cards <- vapply(seq_len(nrow(candidate_order)), function(index) {
      candidate_row <- candidate_order[index, , drop = FALSE]
      candidate_id <- candidate_row$candidate_id[[1]]
      comparison_row <- purrr::keep(candidate_rows, \(row) identical(normalize_public_scalar(row$candidate_id, default = ""), candidate_id))
      comparison_row <- if (length(comparison_row) == 0) NULL else comparison_row[[1]]
      documents <- documents_by_candidate[[candidate_id]]
      primary_doc <- if (is.null(documents) || nrow(documents) == 0) {
        tibble::tibble()
      } else {
        documents |>
          dplyr::arrange(dplyr::desc(.data$is_primary), dplyr::desc(.data$published_at)) |>
          dplyr::slice_head(n = 1)
      }

      links <- c(
        if (nrow(primary_doc) > 0) program_document_link_html(primary_doc$pdf_path[[1]], "PDF", project_dir = ".") else "",
        if (nrow(primary_doc) > 0) program_document_link_html(primary_doc$markdown_path[[1]], "Markdown", project_dir = ".") else "",
        if (nrow(primary_doc) > 0) program_document_link_html(primary_doc$download_url[[1]] %||% primary_doc$official_page_url[[1]], "Fuente", project_dir = ".") else ""
      )
      links <- links[links != ""]

      paste0(
        '<article class="comparison-matrix-card">',
        "<h3>", escape_html(candidate_row$president_name[[1]]), "</h3>",
        '<dl class="comparison-matrix-card__stats">',
        '<div><dt>Prioridad</dt><dd>', escape_html(normalize_public_scalar(comparison_row$priority, default = "Sin evidencia suficiente")), "</dd></div>",
        '<div><dt>Instrumento</dt><dd>', escape_html(normalize_public_scalar(comparison_row$instrument, default = "Sin evidencia suficiente")), "</dd></div>",
        '<div><dt>Especificidad</dt><dd>', escape_html(normalize_public_scalar(comparison_row$specificity, default = "Sin evidencia suficiente")), "</dd></div>",
        '<div><dt>Coherencia</dt><dd>', escape_html(normalize_public_scalar(comparison_row$coherence, default = "Sin evidencia suficiente")), "</dd></div>",
        '<div><dt>Factibilidad</dt><dd>', escape_html(normalize_public_scalar(comparison_row$feasibility, default = "Sin evidencia suficiente")), "</dd></div>",
        "</dl>",
        if (nrow(primary_doc) > 0) {
          paste0('<p class="comparison-card__meta"><strong>Documento base:</strong> ', escape_html(primary_doc$title[[1]] %||% primary_doc$document_id[[1]]), "</p>")
        } else {
          '<p class="comparison-card__meta"><strong>Documento base:</strong> sin documento oficial cargado.</p>'
        },
        if (length(links) > 0) {
          paste0('<div class="comparison-matrix-card__links">', paste(links, collapse = " · "), "</div>")
        } else {
          ""
        },
        "</article>"
      )
    }, character(1))

    paste0(
      '<section class="topic-section">',
      "<h2>", escape_html(topic_label), "</h2>",
      if (!is.na(topic_description) && topic_description != "") {
        paste0('<p class="topic-section__intro">', escape_html(topic_description), "</p>")
      } else {
        ""
      },
      '<p class="narrative-paragraph">', escape_html(normalize_public_scalar(topic_row$summary, default = "Sin resumen comparativo publicado.")), "</p>",
      '<div class="comparison-matrix-grid">', paste(cards, collapse = ""), "</div>",
      "</section>"
    )
  }, character(1))

  cat(paste(rendered_topics, collapse = "\n"))
}
