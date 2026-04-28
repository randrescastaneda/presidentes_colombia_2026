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
            "tracking_note",
            "photo_url",
            "photo_alt",
            "photo_credit",
            "photo_source_url"
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

if (!exists("normalize_public_match_text", inherits = TRUE)) {
  normalize_public_match_text <- function(x) {
    value <- paste(stats::na.omit(as.character(x %||% "")), collapse = " ")
    value <- iconv(value, to = "ASCII//TRANSLIT")
    value[is.na(value)] <- ""
    tolower(value)
  }
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

source_number_lookup <- function(sources) {
  if (!is.data.frame(sources) || nrow(sources) == 0) {
    return(stats::setNames(integer(), character()))
  }

  source_items <- sources |>
    dplyr::distinct(.data$source_id, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(.data$published_at), .data$source_name)

  stats::setNames(seq_len(nrow(source_items)), source_items$source_id)
}

claim_reference_html <- function(claim_row, sources, source_numbers = NULL) {
  source_id <- claim_row$source_id[[1]] %||% NA_character_
  source_row <- source_row_by_id(source_id, sources)
  if (nrow(source_row) == 0) {
    return('<sup class="source-ref">[fuente]</sup>')
  }

  if (is.null(source_numbers)) {
    source_numbers <- source_number_lookup(sources)
  }

  source_number <- source_numbers[[source_id]] %||% 1
  label <- paste0("[", source_number, "]")
  href <- paste0("#", source_anchor_id(source_id))
  paste0('<sup class="source-ref"><a href="', escape_html(href), '">', escape_html(label), "</a></sup>")
}

external_claim_reference_html <- function(claim_row, sources, source_numbers = NULL) {
  source_id <- claim_row$source_id[[1]] %||% NA_character_
  source_row <- source_row_by_id(source_id, sources)
  if (nrow(source_row) == 0) {
    return('<sup class="source-ref">[fuente]</sup>')
  }

  if (is.null(source_numbers)) {
    source_numbers <- source_number_lookup(sources)
  }

  source_number <- source_numbers[[source_id]] %||% 1
  label <- paste0("[", source_number, "]")
  paste0(
    '<sup class="source-ref"><a href="',
    escape_html(source_row$url[[1]] %||% ""),
    '">',
    escape_html(label),
    "</a></sup>"
  )
}

sentence_case_text <- function(text) {
  value <- normalize_sentence(text)
  if (value == "") {
    return("")
  }

  paste0(toupper(substr(value, 1, 1)), substr(value, 2, nchar(value)))
}

claim_policy_sentence <- function(claim_row, candidate_name = NULL) {
  text <- sentence_case_text(claim_row$summary_text[[1]] %||% claim_row$position_text[[1]] %||% "")
  if (text == "") {
    return("La fuente registra una posición pública relevante.")
  }

  text
}

topic_policy_reading <- function(topic_id, claim_rows, candidate_name) {
  text <- normalize_public_match_text(paste(claim_rows$summary_text, claim_rows$position_text, collapse = " "))
  topic <- topic_id %||% ""

  if (grepl("fiscal|economia|productividad|empleo|emprendimiento", topic, perl = TRUE)) {
    if (grepl("baja de impuestos|recorte|mercado|empresa|inversion|formalizacion|regimen simple", text, perl = TRUE)) {
      return(paste0("La lectura económica apunta a una agenda de oferta: aliviar cargas, activar inversión privada y presentar la productividad como condición para financiar otras promesas."))
    }
    if (grepl("subsidio|redistrib|renta|social|estado|publico|desigualdad", text, perl = TRUE)) {
      return(paste0("La orientación económica privilegia el rol redistributivo y coordinador del Estado, con énfasis en gasto social, cierre de brechas o intervención pública."))
    }
    return("La lectura económica es de manejo mixto: combina estabilidad macro, crecimiento y algún grado de intervención pública, pero todavía requiere mayor detalle de instrumentos.")
  }

  if (grepl("salud", topic, perl = TRUE)) {
    if (grepl("eps|mixto|auditoria|pagaremos|deudas|orden", text, perl = TRUE)) {
      return("En salud, la propuesta se mueve hacia corrección institucional del sistema existente: ordenar pagos, auditoría, aseguramiento o reglas de funcionamiento antes que reemplazo completo.")
    }
    return("En salud, la señal principal es de intervención para resolver acceso, medicamentos o atención represada; falta precisar con más detalle el balance entre financiación, prestadores y aseguramiento.")
  }

  if (grepl("seguridad|defensa|justicia|paz", topic, perl = TRUE)) {
    if (grepl("carcel|pena de muerte|militar|pie de fuerza|ordenes de captura|mano dura|bombarde", text, perl = TRUE)) {
      return("La agenda de seguridad enfatiza coerción estatal, expansión de capacidades policiales o militares y sanción penal; la tensión principal está en cómo equilibrar eficacia, derechos y capacidad institucional.")
    }
    if (grepl("paz|dialog|sometimiento|restaurativa|prevencion|oportunidades", text, perl = TRUE)) {
      return("La lectura de seguridad combina control institucional con salidas negociadas, preventivas o territoriales; su punto crítico es demostrar que esos mecanismos reducen violencia sin producir impunidad.")
    }
    return("La posición se ubica en recuperación de autoridad y coordinación institucional, aunque todavía necesita más detalle operativo para evaluar resultados probables.")
  }

  if (grepl("educacion", topic, perl = TRUE)) {
    return("En educación, la propuesta funciona como política de movilidad social y productividad: busca ampliar acceso, permanencia o capacidades, pero la evaluación depende de financiación, focalización y capacidad territorial.")
  }

  if (grepl("ambiente|energia|transicion|agua", topic, perl = TRUE)) {
    return("La señal ambiental y energética conecta modelo productivo con sostenibilidad: el punto decisivo es si la transición se formula como cambio gradual, expansión de oferta o restricción a sectores extractivos.")
  }

  if (grepl("derechos|genero|cuidado", topic, perl = TRUE)) {
    return("La agenda social y de derechos revela una concepción del Estado como garante de protección y acceso; el reto es convertir el enfoque en rutas institucionales, presupuesto y cobertura verificable.")
  }

  if (grepl("instituciones|anticorrupcion|reforma", topic, perl = TRUE)) {
    return("La lectura institucional se concentra en reglas, controles y confianza pública; la calidad de la propuesta depende de si pasa de la consigna anticorrupción a mecanismos concretos de contratación, sanción y vigilancia.")
  }

  "La evidencia permite describir una orientación pública trazable; la principal cautela es no convertir una señal de campaña en un diseño completo cuando la fuente no entrega mecanismo, costo o secuencia."
}

topic_uncertainty_sentence <- function(claim_rows) {
  if (nrow(claim_rows) == 0) {
    return("No hay señales públicas suficientes en este tema dentro del corpus actual.")
  }

  low_specificity <- mean(claim_rows$specificity_score %||% 0, na.rm = TRUE) < 1.25
  mechanism_count <- sum(!is.na(claim_rows$mechanism_text) & claim_rows$mechanism_text != "", na.rm = TRUE)

  if (low_specificity && mechanism_count == 0) {
    return("Lo menos claro es la implementación: la fuente permite conocer la prioridad, pero no todavía costos, secuencia ni responsables concretos.")
  }
  if (mechanism_count == 0) {
    return("La postura es visible, pero la arquitectura de ejecución todavía queda incompleta.")
  }

  "Hay instrumentos identificables, aunque todavía falta contrastarlos con costos fiscales, tiempos de implementación y capacidad institucional."
}

topic_implication_sentence <- function(topic_id, claim_rows, candidate_name) {
  topic <- normalize_public_match_text(topic_id %||% "")
  text <- normalize_public_match_text(paste(claim_rows$summary_text, claim_rows$position_text, claim_rows$mechanism_text, collapse = " "))

  if (grepl("fiscal|economia|productividad|empleo|emprendimiento", topic, perl = TRUE)) {
    if (grepl("impuesto|austeridad|recorte|regla fiscal|deuda", text, perl = TRUE)) {
      return("La implicación fiscal es central: el lector debe mirar si la promesa reduce ingresos, reasigna gasto o aumenta obligaciones permanentes, porque de eso depende su compatibilidad con otras áreas del programa.")
    }
    return("La implicación económica principal es el balance entre crecimiento, capacidad estatal y distribución: la propuesta sólo será creíble si conecta instrumentos productivos con financiación y ejecución territorial.")
  }

  if (grepl("seguridad|defensa|justicia|paz", topic, perl = TRUE)) {
    return("La implicación institucional es alta: estas propuestas requieren coordinación entre Presidencia, Fuerza Pública, Fiscalía, jueces, cárceles y gobiernos territoriales; por eso la diferencia entre consigna y política ejecutable está en la cadena de implementación.")
  }

  if (grepl("salud|educacion|proteccion|derechos|genero|cuidado", topic, perl = TRUE)) {
    return("La implicación social es de cobertura y capacidad operativa: el punto decisivo no es sólo reconocer el derecho o el problema, sino definir quién atiende, con qué presupuesto, en qué territorio y bajo qué regla de priorización.")
  }

  if (grepl("ambiente|energia|transicion|agua|tierras|transporte|vivienda", topic, perl = TRUE)) {
    return("La implicación territorial es fuerte: el efecto de la propuesta dependerá de licencias, coordinación regional, financiación de infraestructura y capacidad de articular comunidades, empresas y autoridades nacionales.")
  }

  if (grepl("instituciones|anticorrupcion|reforma", topic, perl = TRUE)) {
    return("La implicación institucional está en pasar de principios generales a reglas verificables: controles, sanciones, procedimientos, autoridad responsable e indicadores de cumplimiento.")
  }

  paste0("La implicación política es que ", candidate_name, " está usando este tema para mostrar prioridades de gobierno; la solidez de esa señal depende de cuánto detalle agregue sobre responsables, costos y secuencia.")
}

claim_source_ids <- function(claim_rows) {
  unique(stats::na.omit(claim_rows$source_id[claim_rows$source_id != ""]))
}

source_note_matches_topic <- function(note_row, topic_id) {
  note_topics <- unlist(strsplit(note_row$topic_id[[1]] %||% "", "[|]", fixed = FALSE))
  topic_id %in% note_topics || note_row$topic_id[[1]] %||% "" == topic_id
}

source_note_reference_html <- function(source_id, sources, source_numbers = NULL, external = FALSE) {
  source_row <- source_row_by_id(source_id, sources)
  if (nrow(source_row) == 0) {
    return('<sup class="source-ref">[fuente]</sup>')
  }

  if (is.null(source_numbers)) {
    source_numbers <- source_number_lookup(sources)
  }

  label <- paste0("[", source_numbers[[source_id]] %||% 1, "]")
  href <- if (external) source_row$url[[1]] %||% "" else paste0("#", source_anchor_id(source_id))
  paste0('<sup class="source-ref"><a href="', escape_html(href), '">', escape_html(label), "</a></sup>")
}

candidate_topic_overview_html <- function(section, claim_rows, candidate_name, sources, source_numbers) {
  source_count <- length(claim_source_ids(claim_rows))
  first_date <- suppressWarnings(min(as.Date(claim_rows$event_date), na.rm = TRUE))
  last_date <- suppressWarnings(max(as.Date(claim_rows$event_date), na.rm = TRUE))
  date_span <- if (!is.infinite(first_date) && !is.infinite(last_date) && !is.na(first_date) && !is.na(last_date)) {
    paste0("entre ", format_public_date(first_date), " y ", format_public_date(last_date))
  } else {
    "en las fuentes disponibles"
  }

  paste0(
    '<p class="narrative-paragraph"><strong>Lectura integrada.</strong> En ',
    escape_html(tolower(section$topic_label)),
    ", el corpus reúne ",
    nrow(claim_rows),
    " ",
    ifelse(nrow(claim_rows) == 1, "afirmación trazable", "afirmaciones trazables"),
    " de ",
    escape_html(candidate_name),
    " en ",
    source_count,
    " fuente",
    ifelse(source_count == 1, "", "s"),
    " ",
    escape_html(date_span),
    ". Leídas en conjunto, esas fuentes no sólo registran menciones aisladas: permiten reconstruir prioridades, instrumentos tentativos y vacíos de implementación para evaluar la orientación de política pública.</p>"
  )
}

candidate_topic_source_note_html <- function(section, source_analysis_notes, sources, source_numbers, claim_rows = NULL, limit = 8) {
  if (!is.data.frame(source_analysis_notes) || nrow(source_analysis_notes) == 0) {
    return("")
  }

  section_source_ids <- if (is.data.frame(claim_rows) && nrow(claim_rows) > 0) claim_source_ids(claim_rows) else character()
  matching <- vapply(seq_len(nrow(source_analysis_notes)), function(index) {
    note_row <- source_analysis_notes[index, , drop = FALSE]
    source_note_matches_topic(note_row, section$topic_id) || note_row$source_id[[1]] %in% section_source_ids
  }, logical(1))

  notes <- source_analysis_notes[matching, , drop = FALSE] |>
    dplyr::distinct(.data$source_id, .keep_all = TRUE) |>
    dplyr::slice_head(n = limit)

  if (nrow(notes) == 0) {
    return("")
  }

  paragraphs <- vapply(seq_len(nrow(notes)), function(index) {
    note <- notes[index, , drop = FALSE]
    paste0(
      '<p class="narrative-paragraph"><strong>Fuente analizada.</strong> ',
      escape_html(normalize_sentence(note$substantive_summary[[1]] %||% note$policy_positions[[1]] %||% "")),
      " ",
      source_note_reference_html(note$source_id[[1]], sources, source_numbers),
      "</p>"
    )
  }, character(1))

  paste(paragraphs, collapse = "")
}

comparison_topic_overview_html <- function(topic) {
  sections <- topic$candidate_sections %||% list()
  documented <- purrr::keep(sections, \(section) is.data.frame(section$claim_rows) && nrow(section$claim_rows) > 0)
  empty <- purrr::keep(sections, \(section) !is.data.frame(section$claim_rows) || nrow(section$claim_rows) == 0)
  documented_names <- vapply(documented, \(section) section$candidate_name %||% section$candidate_id, character(1))
  empty_names <- vapply(empty, \(section) section$candidate_name %||% section$candidate_id, character(1))
  source_count <- length(unique(unlist(lapply(documented, \(section) claim_source_ids(section$claim_rows)))))
  claim_count <- sum(vapply(documented, \(section) nrow(section$claim_rows), integer(1)))

  documented_text <- if (length(documented_names) == 0) {
    "ninguna candidatura de la watchlist"
  } else {
    paste(documented_names, collapse = ", ")
  }
  empty_text <- if (length(empty_names) == 0) {
    "ninguna candidatura queda sin señal en este eje"
  } else {
    paste(empty_names, collapse = ", ")
  }

  paste0(
    '<div class="prose-card prose-card--comparison">',
    '<p class="narrative-paragraph"><strong>Mapa del debate.</strong> En ',
    escape_html(tolower(topic$topic_label)),
    ", el corpus público contiene ",
    claim_count,
    " ",
    ifelse(claim_count == 1, "afirmación trazable", "afirmaciones trazables"),
    " distribuida",
    ifelse(claim_count == 1, "", "s"),
    " en ",
    source_count,
    " fuente",
    ifelse(source_count == 1, "", "s"),
    ". La comparación sustantiva se apoya hoy en ",
    escape_html(documented_text),
    ". Queda con señal pendiente o más débil: ",
    escape_html(empty_text),
    ".</p>",
    '<p class="narrative-paragraph"><strong>Cómo leer la diferencia.</strong> El contraste no se limita a contar menciones: compara prioridades, instrumentos, orientación de Estado, grado de coerción o redistribución, y nivel de detalle operativo. Cuando una candidatura tiene menos fuentes, se dice explícitamente para no presentar simetría artificial.</p>',
    "</div>"
  )
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
    ". ",
    claim_reference_html(claim_row, sources),
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
  ideology_label <- dossier_row$ideology_label[[1]] %||% "Señales programáticas pendientes"
  ideology_class <- ideology_family_class(ideology_label)
  last_event <- dossier_row$last_event_date[[1]] %||% NA
  total_claims <- dossier_row$total_claims[[1]] %||% 0
  total_sources <- dossier_row$total_sources[[1]] %||% 0
  total_notes <- dossier_row$total_analysis_notes[[1]] %||% 0

  cat(
    paste0(
      '<div class="profile-hero">',
      if (!is.na(candidate_meta$photo_url[[1]] %||% NA_character_) && candidate_meta$photo_url[[1]] %||% "" != "") {
        paste0(
          '<figure class="profile-hero__photo">',
          '<img src="', escape_html(candidate_meta$photo_url[[1]]), '" alt="', escape_html(candidate_meta$photo_alt[[1]] %||% candidate_meta$president_name[[1]]), '">',
          if (!is.na(candidate_meta$photo_credit[[1]] %||% NA_character_) && candidate_meta$photo_credit[[1]] %||% "" != "") {
            paste0(
              '<figcaption>',
              if (!is.na(candidate_meta$photo_source_url[[1]] %||% NA_character_) && candidate_meta$photo_source_url[[1]] %||% "" != "") {
                html_link(candidate_meta$photo_credit[[1]], candidate_meta$photo_source_url[[1]])
              } else {
                escape_html(candidate_meta$photo_credit[[1]])
              },
              '</figcaption>'
            )
          } else {
            ""
          },
          '</figure>'
        )
      } else {
        ""
      },
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
  ideology_label <- dossier_row$ideology_label[[1]] %||% "Señales programáticas pendientes"
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

emit_candidate_policy_sections <- function(candidate_policy, candidate_sources = NULL, taxonomy = NULL, source_analysis_notes = NULL) {
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

  sources <- candidate_policy$source_library %||% tibble::tibble()
  source_numbers <- source_number_lookup(sources)
  all_sections <- c(
    candidate_policy$comparable_sections %||% list(),
    candidate_policy$documented_sections %||% list()
  )

  if (length(all_sections) == 0) {
    emit_callout("Todavía no hay propuestas públicas trazables para este candidato.", "note")
    return(invisible(NULL))
  }

  rendered_sections <- vapply(all_sections, function(section) {
    claim_rows <- section$claim_rows |>
      dplyr::arrange(dplyr::desc(.data$claim_type == "policy_proposal"), dplyr::desc(.data$specificity_score), dplyr::desc(.data$event_date)) |>
      dplyr::distinct(.data$summary_text, .keep_all = TRUE)

    displayed_claims <- claim_rows |>
      dplyr::slice_head(n = 12)

    claim_paragraphs <- paste(vapply(seq_len(nrow(displayed_claims)), function(index) {
        paste0(
          '<p class="narrative-paragraph">',
          escape_html(claim_policy_sentence(displayed_claims[index, , drop = FALSE], candidate_policy$candidate_name)),
          " ",
          claim_reference_html(displayed_claims[index, , drop = FALSE], sources, source_numbers),
          "</p>"
        )
      }, character(1)), collapse = "")

    source_note_paragraphs <- candidate_topic_source_note_html(
      section = section,
      source_analysis_notes = source_analysis_notes,
      sources = sources,
      source_numbers = source_numbers,
      claim_rows = claim_rows,
      limit = 8
    )

    paste0(
      '<section class="topic-section topic-section--narrative" data-topic-id="', escape_html(section$topic_id), '" data-topic-state="', escape_html(section$state), '">',
      '<h3>', escape_html(section$topic_label), '</h3>',
      if (!is.na(section$topic_description) && section$topic_description != "") {
        paste0('<p class="topic-section__intro">', escape_html(section$topic_description), "</p>")
      } else {
        ""
      },
      '<div class="prose-card prose-card--policy">',
      candidate_topic_overview_html(section, claim_rows, candidate_policy$candidate_name, sources, source_numbers),
      '<p class="narrative-paragraph"><strong>Qué está proponiendo.</strong> Estas son las señales sustantivas que aparecen en las fuentes revisadas, ordenadas por especificidad y fecha.</p>',
      claim_paragraphs,
      source_note_paragraphs,
      '<p class="narrative-paragraph"><strong>Lectura política.</strong> ',
      escape_html(topic_policy_reading(section$topic_id, claim_rows, candidate_policy$candidate_name)),
      "</p>",
      '<p class="narrative-paragraph"><strong>Implicaciones de política pública.</strong> ',
      escape_html(topic_implication_sentence(section$topic_id, claim_rows, candidate_policy$candidate_name)),
      "</p>",
      '<p class="narrative-paragraph"><strong>Cautela analítica.</strong> ',
      escape_html(topic_uncertainty_sentence(claim_rows)),
      "</p>",
      "</div>",
      "</section>"
    )
  }, character(1))

  cat(paste(rendered_sections, collapse = "\n"))
}

emit_candidate_analysis <- function(candidate_analysis, candidate_sources, candidate_analysis_artifact = NULL) {
  artifact_paragraphs <- character()

  if (!is.null(candidate_analysis_artifact) && length(candidate_analysis_artifact) > 0) {
    artifact_paragraphs <- c(
      if (!is.na(candidate_analysis_artifact$political_philosophy %||% NA_character_)) {
        paste0('<p class="narrative-paragraph"><strong>Filosofía política inferida.</strong> ', escape_html(candidate_analysis_artifact$political_philosophy), "</p>")
      },
      if (!is.na(candidate_analysis_artifact$internal_coherence %||% NA_character_)) {
        paste0('<p class="narrative-paragraph"><strong>Coherencia interna.</strong> ', escape_html(candidate_analysis_artifact$internal_coherence), "</p>")
      },
      if (!is.na(candidate_analysis_artifact$mainstream_distance %||% NA_character_)) {
        paste0('<p class="narrative-paragraph"><strong>Distancia frente al consenso político.</strong> ', escape_html(candidate_analysis_artifact$mainstream_distance), "</p>")
      },
      if (length(candidate_analysis_artifact$strengths %||% character()) > 0) {
        paste0('<p class="narrative-paragraph"><strong>Fortalezas visibles.</strong> ', escape_html(paste(candidate_analysis_artifact$strengths, collapse = " ")), "</p>")
      },
      if (length(candidate_analysis_artifact$weaknesses %||% character()) > 0) {
        paste0('<p class="narrative-paragraph"><strong>Límites y tensiones.</strong> ', escape_html(paste(candidate_analysis_artifact$weaknesses, collapse = " ")), "</p>")
      }
    )
  }

  note_paragraphs <- character()
  if (is.data.frame(candidate_analysis) && nrow(candidate_analysis) > 0) {
    notes <- candidate_analysis |>
      dplyr::arrange(dplyr::desc(.data$confidence))

    note_paragraphs <- vapply(seq_len(nrow(notes)), function(index) {
      analysis_paragraph_html(notes[index, , drop = FALSE], candidate_sources)
    }, character(1))
  }

  paragraphs <- c(artifact_paragraphs, note_paragraphs)
  paragraphs <- paragraphs[paragraphs != ""]

  if (length(paragraphs) == 0) {
    emit_callout("Aún no hay notas analíticas públicas para esta candidatura.", "note")
    return(invisible(NULL))
  }

  cat(paste0('<div class="prose-card prose-card--analysis">', paste(paragraphs, collapse = ""), "</div>"))
}

program_document_href <- function(path_or_url, project_dir = ".", relative_prefix = "") {
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
    relative_path <- sub(paste0("^", stringr::fixed(project_root), "/?"), "", resolved)
    if (!is.na(relative_prefix) && relative_prefix != "" && !grepl("^([.][.]/|/)", relative_path, perl = TRUE)) {
      return(paste0(relative_prefix, relative_path))
    }

    return(relative_path)
  }

  resolved
}

program_document_link_html <- function(path_or_url, label, project_dir = ".", relative_prefix = "") {
  value <- path_or_url %||% ""
  if (is.na(value) || identical(value, "")) {
    return("")
  }

  href <- program_document_href(value, project_dir = project_dir, relative_prefix = relative_prefix)

  if (is.na(href) || identical(href, "")) {
    return("")
  }

  paste0('<a class="card-link" href="', escape_html(href), '">', escape_html(label), "</a>")
}

emit_candidate_program_documents <- function(candidate_program_documents, project_dir = ".", relative_prefix = "") {
  if (nrow(candidate_program_documents) == 0) {
    emit_callout("Todavía no hay documento oficial del programa cargado para esta candidatura.", "note")
    return(invisible(NULL))
  }

  rows <- candidate_program_documents |>
    dplyr::distinct(.data$document_id, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(.data$is_primary), dplyr::desc(.data$published_at))

  cards <- vapply(seq_len(nrow(rows)), function(index) {
    row <- rows[index, , drop = FALSE]
    row_value <- function(name) {
      if (!name %in% names(row)) {
        return(NA_character_)
      }
      row[[name]][[1]]
    }
    actions <- c(
      program_document_link_html(row$pdf_path[[1]], "PDF local", project_dir = project_dir, relative_prefix = relative_prefix),
      program_document_link_html(row$markdown_path[[1]], "Markdown", project_dir = project_dir, relative_prefix = relative_prefix),
      program_document_link_html(row_value("public_url") %||% row$download_url[[1]] %||% row$official_page_url[[1]], "Fuente oficial", project_dir = project_dir, relative_prefix = relative_prefix)
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
      '<p class="comparison-card__meta">Documento usado como fuente programática trazable para esta ficha. Los enlaces permiten revisar el original y la conversión auditable.</p>',
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
      sources <- comparison_model$sources %||% tibble::tibble()
      source_numbers <- source_number_lookup(sources)
      candidate_blocks <- vapply(topic$candidate_sections, function(section) {
        claim_rows <- section$claim_rows
        if (!is.data.frame(claim_rows) || nrow(claim_rows) == 0) {
          return(paste0(
            '<article class="comparison-narrative-card comparison-narrative-card--empty">',
            '<h3>', escape_html(section$candidate_name), '</h3>',
            '<p class="narrative-paragraph">En el corpus publicado no aparece todavía una propuesta específica de esta candidatura sobre este tema. La comparación no rellena ese vacío con inferencias externas.</p>',
            '</article>'
          ))
        }

        selected <- claim_rows |>
          dplyr::slice_head(n = 8)
        claim_paragraphs <- paste(vapply(seq_len(nrow(selected)), function(index) {
          paste0(
            '<p class="narrative-paragraph">',
            escape_html(claim_policy_sentence(selected[index, , drop = FALSE], section$candidate_name)),
            " ",
            external_claim_reference_html(selected[index, , drop = FALSE], sources, source_numbers),
            "</p>"
          )
        }, character(1)), collapse = "")

        paste0(
          '<article class="comparison-narrative-card">',
          '<h3><a href="', escape_html(section$href), '">', escape_html(section$candidate_name), '</a></h3>',
          '<p class="comparison-card__meta">Posiciones trazables en este eje: ', nrow(claim_rows), ' en ', length(claim_source_ids(claim_rows)), ' fuente', ifelse(length(claim_source_ids(claim_rows)) == 1, "", "s"), '.</p>',
          '<p class="narrative-paragraph"><strong>Resumen interpretativo.</strong> ',
          escape_html(topic_policy_reading(topic$topic_id, claim_rows, section$candidate_name)),
          "</p>",
          claim_paragraphs,
          '<p class="narrative-paragraph"><strong>Implicación comparativa.</strong> ',
          escape_html(topic_implication_sentence(topic$topic_id, claim_rows, section$candidate_name)),
          "</p>",
          '<p class="narrative-paragraph"><strong>Punto débil de la evidencia.</strong> ',
          escape_html(topic_uncertainty_sentence(claim_rows)),
          "</p>",
          '</article>'
        )
      }, character(1))

      paste0(
        '<section class="topic-section" data-topic-id="', escape_html(topic$topic_id), '">',
        "<h2>", escape_html(topic$topic_label), "</h2>",
        if (!is.na(topic$topic_description) && topic$topic_description != "") {
          paste0('<p class="topic-section__intro">', escape_html(topic$topic_description), "</p>")
        } else {
          ""
        },
        '<p class="narrative-paragraph">', escape_html(topic$summary), "</p>",
        comparison_topic_overview_html(topic),
        '<div class="comparison-narrative-grid">', paste(candidate_blocks, collapse = ""), "</div>",
        "</section>"
      )
    }, character(1))

    cat(paste(rendered_topics, collapse = "\n"))
    return(invisible(NULL))
  }

  if (is.null(comparison_model) || length(comparison_model) == 0) {
    emit_callout("Todavía no hay material público trazable para comparar programas entre candidatos.", "note")
  } else {
    emit_callout("El comparador público usa el modelo narrativo por temas. Este artefacto no trae secciones narrativas para renderizar.", "note")
  }

  invisible(NULL)
}
