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
      "topic_id",
      "topic_label",
      "root_topic_id",
      "root_label",
      "root_description",
      "root_sort_order"
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
    dplyr::distinct(.data$source_id, .keep_all = TRUE)
  if (!"published_at" %in% names(source_items)) {
    source_items$published_at <- as.POSIXct(NA)
  }
  if (!"source_name" %in% names(source_items)) {
    source_items$source_name <- ""
  }

  source_items <- source_items |>
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

clean_policy_claim_text <- function(claim_row) {
  value <- claim_row$position_text[[1]] %||% claim_row$summary_text[[1]] %||% ""
  value <- normalize_sentence(value)

  if (value == "" || grepl("fuente incorporada desde data|pdf oficial de propuestas hospedado", normalize_public_match_text(value), perl = TRUE)) {
    value <- normalize_sentence(claim_row$summary_text[[1]] %||% "")
  }

  value <- gsub("^La fuente registra un hecho público asociado a [^:]+:\\s*", "", value, perl = TRUE)
  value <- gsub("^(Infobae|EL PAÍS|El País|El Tiempo|Forbes|Caracol Radio|RTVC|Semana|Noticias RCN|Canal 1)\\s+(reporta|resume|registra)\\s+que\\s+", "", value, ignore.case = TRUE, perl = TRUE)
  value <- gsub("^(Infobae|EL PAÍS|El País|El Tiempo|Forbes|Caracol Radio|RTVC|Semana|Noticias RCN|Canal 1)\\s+(reporta|resume|registra)\\s+", "", value, ignore.case = TRUE, perl = TRUE)
  value <- stringr::str_squish(value)

  if (value == "") {
    return("La candidatura dejó una señal pública relevante sobre este tema")
  }

  sentence_case_text(value)
}

policy_claim_sentence_html <- function(claim_row, sources, source_numbers = NULL, external = FALSE) {
  reference <- if (external) {
    external_claim_reference_html(claim_row, sources, source_numbers)
  } else {
    claim_reference_html(claim_row, sources, source_numbers)
  }

  paste0(
    escape_html(clean_policy_claim_text(claim_row)),
    " ",
    reference
  )
}

policy_claims_narrative_html <- function(claim_rows, sources, source_numbers = NULL, limit = 7, external = FALSE) {
  if (!is.data.frame(claim_rows) || nrow(claim_rows) == 0) {
    return("")
  }

  selected <- claim_rows |>
    dplyr::distinct(.data$summary_text, .data$position_text, .keep_all = TRUE) |>
    dplyr::slice_head(n = limit)

  sentences <- vapply(seq_len(nrow(selected)), function(index) {
    paste0(policy_claim_sentence_html(selected[index, , drop = FALSE], sources, source_numbers, external = external), ".")
  }, character(1))

  paste(sentences, collapse = " ")
}

topic_candidate_lead_sentence <- function(topic_id, claim_rows, candidate_name) {
  topic <- normalize_public_match_text(topic_id %||% "")
  text <- normalize_public_match_text(paste(claim_rows$summary_text, claim_rows$position_text, claim_rows$mechanism_text, collapse = " "))

  if (grepl("fiscal|economia|productividad|empleo|emprendimiento", topic, perl = TRUE)) {
    if (grepl("recortar|40 %|impuesto|cuatro por mil|gasolina|inversion extranjera|hidrocarburo|mineria", text, perl = TRUE)) {
      return(paste0("La apuesta económica de ", candidate_name, " apunta a un Estado más pequeño, menor carga tributaria y reactivación de sectores productivos intensivos en inversión."))
    }
    return(paste0("La propuesta económica de ", candidate_name, " combina crecimiento, manejo fiscal y promesas de activación productiva."))
  }

  if (grepl("salud", topic, perl = TRUE)) {
    if (grepl("90 dias|10 billones|emergencia|choque", text, perl = TRUE)) {
      return(paste0("En salud, ", candidate_name, " presenta la crisis del sistema como un problema que requiere un choque inmediato de financiación y gestión."))
    }
    return(paste0("En salud, ", candidate_name, " concentra su mensaje en corregir fallas de acceso, financiación o prestación del sistema."))
  }

  if (grepl("seguridad|defensa|justicia", topic, perl = TRUE)) {
    if (grepl("megacarcel|carcel|drones|fumigacion|bombardeo|pie de fuerza|ordenes de captura", text, perl = TRUE)) {
      return(paste0("En seguridad y justicia, ", candidate_name, " plantea una agenda de autoridad fuerte: más coerción estatal, expansión penitenciaria y uso intensivo de fuerza pública."))
    }
    return(paste0("En seguridad y justicia, ", candidate_name, " propone recuperar capacidad estatal frente al crimen y al desorden institucional."))
  }

  if (grepl("paz|relaciones-exteriores", topic, perl = TRUE)) {
    if (grepl("jep|estados unidos|israel|bombardeo|fumigacion", text, perl = TRUE)) {
      return(paste0("En paz y política internacional, ", candidate_name, " marca distancia con la arquitectura de justicia transicional y privilegia alianzas externas asociadas a seguridad dura."))
    }
    return(paste0("En paz y política internacional, ", candidate_name, " deja ver cómo conectaría seguridad, diplomacia y orden institucional."))
  }

  if (grepl("reforma|instituciones|anticorrupcion", topic, perl = TRUE)) {
    return(paste0("En instituciones, ", candidate_name, " usa esta agenda para definir qué tipo de reglas políticas considera necesarias para gobernar y competir."))
  }

  if (grepl("ambiente|energia|transicion|agua|tierras|transporte|vivienda", topic, perl = TRUE)) {
    return(paste0("En este frente territorial y productivo, ", candidate_name, " conecta infraestructura, recursos y capacidad estatal con promesas de desarrollo."))
  }

  paste0("En este tema, ", candidate_name, " deja una señal programática que permite leer prioridades de gobierno y tensiones de implementación.")
}

candidate_topic_essay_html <- function(section, claim_rows, candidate_name, sources, source_numbers) {
  topic_label <- tolower(section$topic_label %||% section$topic_id %||% "este tema")
  claim_text <- policy_claims_narrative_html(claim_rows, sources, source_numbers, limit = 8)
  if (claim_text == "") {
    claim_text <- "Todavía no hay una formulación suficientemente clara para describir esta agenda con precisión."
  }

  paste0(
    '<div class="prose-card prose-card--policy">',
    '<p class="narrative-paragraph">',
    escape_html(topic_candidate_lead_sentence(section$topic_id, claim_rows, candidate_name)),
    " En ",
    escape_html(topic_label),
    ", los instrumentos visibles son estos: ",
    claim_text,
    "</p>",
    '<p class="narrative-paragraph">',
    escape_html(topic_policy_reading(section$topic_id, claim_rows, candidate_name)),
    " ",
    escape_html(topic_implication_sentence(section$topic_id, claim_rows, candidate_name)),
    "</p>",
    '<p class="narrative-paragraph">La lectura política todavía debe tomarse con cautela. ',
    escape_html(topic_uncertainty_sentence(claim_rows)),
    " Esa cautela no elimina la señal programática: indica qué tendría que aclarar la campaña para que la promesa pueda evaluarse como política pública completa.</p>",
    "</div>"
  )
}

comparison_candidate_paragraph_html <- function(section, topic_id, sources, source_numbers) {
  claim_rows <- section$claim_rows
  candidate_name <- section$candidate_name %||% section$candidate_id

  if (!is.data.frame(claim_rows) || nrow(claim_rows) == 0) {
    return(paste0(
      '<p class="narrative-paragraph"><strong>',
      escape_html(candidate_name),
      ".</strong> En las fuentes publicadas todavía no aparece una propuesta específica sobre este tema. Por ahora, cualquier comparación tendría que esperar una formulación más clara de la campaña.</p>"
    ))
  }

  paste0(
    '<p class="narrative-paragraph"><strong>',
    escape_html(candidate_name),
    ".</strong> ",
    policy_claims_narrative_html(claim_rows, sources, source_numbers, limit = 6, external = TRUE),
    " ",
    escape_html(topic_policy_reading(topic_id, claim_rows, candidate_name)),
    " ",
    escape_html(topic_implication_sentence(topic_id, claim_rows, candidate_name)),
    "</p>"
  )
}

comparison_topic_essay_html <- function(topic, sources, source_numbers) {
  candidate_paragraphs <- vapply(topic$candidate_sections, function(section) {
    comparison_candidate_paragraph_html(section, topic$topic_id, sources, source_numbers)
  }, character(1))

  paste0(
    '<div class="prose-card prose-card--comparison">',
    '<p class="narrative-paragraph">',
    escape_html(topic$summary %||% ""),
    "</p>",
    paste(candidate_paragraphs, collapse = ""),
    "</div>"
  )
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
    return("Todavía no hay una posición pública suficientemente clara sobre este tema.")
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

replace_editorial_source_tokens <- function(text, sources, source_numbers = NULL, external = FALSE) {
  if (is.null(source_numbers)) {
    source_numbers <- source_number_lookup(sources)
  }

  rendered <- escape_html(text %||% "")
  matches <- gregexpr("\\{src:([^}]+)\\}", text %||% "", perl = TRUE)
  tokens <- regmatches(text %||% "", matches)[[1]]
  if (length(tokens) == 0 || identical(tokens, character(0)) || tokens[[1]] == "-1") {
    return(rendered)
  }

  for (token in unique(tokens)) {
    source_id <- gsub("^\\{src:|\\}$", "", token)
    rendered <- gsub(
      escape_html(token),
      source_note_reference_html(source_id, sources, source_numbers, external = external),
      rendered,
      fixed = TRUE
    )
  }

  rendered
}

editorial_markdown_body_html <- function(body, sources, source_numbers = NULL, external = FALSE) {
  value <- body %||% ""
  if (is.na(value) || value == "") {
    return("")
  }

  paragraphs <- unlist(strsplit(value, "\\n[[:space:]]*\\n", perl = TRUE), use.names = FALSE)
  paragraphs <- paragraphs[stringr::str_squish(paragraphs) != ""]

  paste(vapply(paragraphs, function(paragraph) {
    paste0(
      '<p class="narrative-paragraph">',
      replace_editorial_source_tokens(stringr::str_squish(paragraph), sources, source_numbers, external = external),
      "</p>"
    )
  }, character(1)), collapse = "")
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
  top_topics <- dossier_row$top_topics[[1]] %||% "Agenda en construcción"

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
      '<p class="profile-hero__meta"><strong>Énfasis visibles:</strong> ', escape_html(top_topics), "</p>",
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

  cat(
    paste0(
      '<div class="prose-card">',
      '<p class="narrative-paragraph"><strong>Ubicación analítica provisional:</strong> ',
      escape_html(ideology_label),
      '. La etiqueta resume la orientación que sugieren sus propuestas públicas, no una autodefinición oficial ni una calificación cerrada.</p>',
      '<p class="narrative-paragraph">',
      escape_html(ideology_rationale),
      ' La lectura debe contrastarse con las secciones temáticas, donde se ve con más claridad qué instrumentos concretos empujan esa orientación.</p>',
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

  editorial_sections <- normalize_public_collection(candidate_policy$editorial_sections %||% list())
  if (length(editorial_sections) > 0) {
    rendered_sections <- vapply(editorial_sections, function(section) {
      paste0(
        '<section class="topic-section topic-section--narrative" data-topic-id="',
        escape_html(section$section_id %||% ""),
        '" data-topic-state="editorial_dossier">',
        '<h3>', escape_html(section$heading %||% section$section_id %||% "Tema"), '</h3>',
        '<div class="prose-card prose-card--policy">',
        editorial_markdown_body_html(section$body %||% "", sources, source_numbers),
        "</div>",
        "</section>"
      )
    }, character(1))

    cat(paste(rendered_sections, collapse = "\n"))
    return(invisible(NULL))
  }

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

    paste0(
      '<section class="topic-section topic-section--narrative" data-topic-id="', escape_html(section$topic_id), '" data-topic-state="', escape_html(section$state), '">',
      '<h3>', escape_html(section$topic_label), '</h3>',
      if (!is.na(section$topic_description) && section$topic_description != "") {
        paste0('<p class="topic-section__intro">', escape_html(section$topic_description), "</p>")
      } else {
        ""
      },
      candidate_topic_essay_html(section, claim_rows, candidate_policy$candidate_name, sources, source_numbers),
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

      paste0(
        '<section class="topic-section" data-topic-id="', escape_html(topic$topic_id), '">',
        "<h2>", escape_html(topic$topic_label), "</h2>",
        if (!is.na(topic$topic_description) && topic$topic_description != "") {
          paste0('<p class="topic-section__intro">', escape_html(topic$topic_description), "</p>")
        } else {
          ""
        },
        if (!is.null(topic$body) && (topic$body %||% "") != "") {
          paste0(
            '<div class="prose-card prose-card--comparison">',
            editorial_markdown_body_html(topic$body, sources, source_numbers, external = TRUE),
            "</div>"
          )
        } else {
          comparison_topic_essay_html(topic, sources, source_numbers)
        },
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
