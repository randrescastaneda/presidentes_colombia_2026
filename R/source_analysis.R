empty_source_analysis_notes_tibble <- function() {
  tibble::tibble(
    source_analysis_id = character(),
    source_id = character(),
    candidate_id = character(),
    topic_id = character(),
    substantive_summary = character(),
    policy_positions = character(),
    ideology_signals = character(),
    chronology_summary = character(),
    evidence_state = character(),
    source_ids = character(),
    claim_ids = character()
  )
}

normalize_public_match_text <- function(x) {
  value <- paste(stats::na.omit(as.character(x %||% "")), collapse = " ")
  value <- iconv(value, to = "ASCII//TRANSLIT")
  value[is.na(value)] <- ""
  tolower(value)
}

normalize_public_match_vector <- function(x) {
  values <- iconv(as.character(x %||% ""), to = "ASCII//TRANSLIT")
  values[is.na(values)] <- ""
  tolower(values)
}

is_blank_topic <- function(value, taxonomy = NULL) {
  if (length(value) == 0 || all(is.na(value))) {
    return(TRUE)
  }

  normalized <- as.character(value[[1]])
  if (is.na(normalized) || identical(normalized, "") || identical(tolower(normalized), "na")) {
    return(TRUE)
  }

  if (!is.null(taxonomy) && nrow(taxonomy) > 0 && !normalized %in% taxonomy$topic_id) {
    return(TRUE)
  }

  FALSE
}

topic_inference_rules <- function() {
  tibble::tribble(
    ~topic_id, ~pattern,
    "aseguramiento-salud", "salud|eps|hospital|medicamento|paciente|atencion represada|sistema sanitario",
    "educacion-superior", "educacion|universidad|sena|beca|matricula|colegio|maestro|jornada escolar|estudiar",
    "seguridad-ciudadana", "seguridad ciudadana|policia|hurto|extorsion|secuestro|gaula|crimen|criminalidad|orden publico|control territorial",
    "defensa", "militar|fuerza publica|defensa|frontera|drones|bombardeo|pie de fuerza|soldados",
    "justicia", "justicia|carcel|megacarcel|pena de muerte|impunidad|fiscalia|jueces|ordenes de captura|gestores de paz",
    "paz", "paz|jep|acuerdo final|sometimiento|dialogo|desarmar|victimas|conflicto armado|far[c|k]",
    "fiscal", "fiscal|impuesto|tribut|austeridad|deuda|deficit|regla fiscal|colpensiones|pensiones|ahorro ciudadano|recursos publicos",
    "productividad", "productividad|crecimiento|industria|empresa|inversion|innovacion|tecnologia|turismo|agroindustria|export",
    "emprendimiento", "emprendimiento|mipyme|formalizacion|regimen simple|empresa",
    "transicion-energetica", "energia|energetic|fracking|hidrocarburo|mineria|petroleo|gas|renovable|apagones|tarifa",
    "agua-ambiente", "ambiente|clima|biodiversidad|deforestacion|agua potable|saneamiento|reforestacion|naturaleza",
    "tierras", "tierra|campo|campesino|rural|reforma agraria|vias terciarias|agro",
    "transporte", "infraestructura|transporte|tren|vias|carretera|puerto|logistica|metro|regiotram",
    "vivienda", "vivienda|subsidio de vivienda|mi casa ya|arriendo|autoconstruccion",
    "proteccion-social", "proteccion social|subsidio|renta|cuidado|pension|colpensiones|ingreso minimo",
    "genero-diversidad", "genero|mujer|feminicidio|lgbti|diversidad|cuidado|violencias basadas",
    "anticorrupcion", "corrupcion|contratacion|transparencia|auditoria|robo de recursos",
    "reforma-politica", "constituyente|reforma politica|democracia|participacion|reglas electorales|debate",
    "relaciones-exteriores", "politica exterior|diplomacia|comercio exterior|cooperacion internacional|estados unidos|israel|venezuela",
    "desigualdad", "desigualdad|pobreza|region olvidada|brecha|movilidad social|oportunidad",
    "trayectoria-publica", "exalcalde|exgobernador|senador|ministro|canciller|trayectoria|perfil|biografia|renuncia|candidatura"
  )
}

infer_topic_id_for_claim <- function(claim_row, source_row = tibble::tibble(), taxonomy = tibble::tibble()) {
  existing <- claim_row$topic_id[[1]] %||% NA_character_
  if (!is_blank_topic(existing, taxonomy)) {
    return(existing)
  }

  text <- normalize_public_match_text(c(
    claim_row$policy_key[[1]] %||% "",
    claim_row$summary_text[[1]] %||% "",
    claim_row$position_text[[1]] %||% "",
    claim_row$mechanism_text[[1]] %||% "",
    claim_row$target_population[[1]] %||% "",
    claim_row$problem_diagnosed[[1]] %||% "",
    source_row$title[[1]] %||% "",
    source_row$quote_text[[1]] %||% "",
    source_row$source_type[[1]] %||% ""
  ))

  rules <- topic_inference_rules()
  matches <- rules |>
    dplyr::rowwise() |>
    dplyr::mutate(matched = grepl(.data$pattern, text, perl = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$matched)

  if (nrow(matches) > 0) {
    return(matches$topic_id[[1]])
  }

  if (identical(claim_row$claim_type[[1]] %||% "", "biography")) {
    return("trayectoria-publica")
  }

  "vida-publica"
}

summary_looks_like_placeholder <- function(text, source_title = "") {
  normalized <- normalize_public_match_text(text)
  title_norm <- normalize_public_match_text(source_title)

  normalized == "" ||
    normalized %in% c("programa de gobierno", "plan integrado de gobierno", "programa oficial", "fuente incorporada desde data") ||
    normalized == title_norm ||
    grepl("fuente incorporada desde data|pdf oficial descargable|tabla de contenido|captura parcial del sitio oficial|^#\\s", normalized, perl = TRUE) ||
    nchar(normalized) < 20
}

extract_meaningful_sentence <- function(text) {
  if (length(text) == 0 || all(is.na(text))) {
    return("")
  }

  raw <- gsub("\r", " ", as.character(text[[1]]))
  raw <- gsub("\\s+", " ", raw)
  pieces <- unlist(strsplit(raw, "(?<=[.!?])\\s+|\\s+-\\s+|\\n", perl = TRUE))
  pieces <- stringr::str_squish(pieces)
  pieces <- pieces[nchar(pieces) >= 35 & nchar(pieces) <= 260]
  pieces <- pieces[!grepl("tabla de contenido|fuente oficial|estado de acceso|whatsapp|contacto|pagina [0-9]|fuente incorporada desde data|validated_by_http|^https?://", normalize_public_match_vector(pieces), perl = TRUE)]

  if (length(pieces) == 0) {
    return("")
  }

  policy_like <- grepl(
    "propon|plante|crear|fortalec|reduc|aument|impuls|mantener|eliminar|garantiz|reactiv|constru|financ|ordenar|prioriz|defiend|promet",
    normalize_public_match_vector(pieces),
    perl = TRUE
  )

  selected <- if (any(policy_like)) pieces[which(policy_like)[[1]]] else pieces[[1]]
  gsub("[.]+$", "", selected)
}

title_based_public_summary <- function(source_title, candidate_name = "") {
  clean_title <- stringr::str_squish(source_title %||% "")
  if (summary_looks_like_placeholder(clean_title, "")) {
    return("")
  }

  clean_title <- gsub("\\s+\\|\\s+.*$", "", clean_title)
  clean_title <- gsub(
    "\\s+-\\s+(Infobae|Caracol Radio|360|El País|El Pais|El Tiempo|Semana|W Radio|La Silla Vacía|La Silla Vacia|Wikipedia).*$",
    "",
    clean_title,
    ignore.case = TRUE,
    perl = TRUE
  )
  clean_title <- stringr::str_squish(clean_title)

  if (clean_title == "") {
    return("")
  }

  prefix <- if (!is.na(candidate_name) && candidate_name != "") {
    paste0("La fuente registra un hecho público asociado a ", candidate_name, ": ")
  } else {
    "La fuente registra un hecho público: "
  }

  paste0(prefix, gsub("[.]+$", "", clean_title), ".")
}

compact_public_claim_summary <- function(claim_row, source_row = tibble::tibble(), candidate_name = "") {
  current <- claim_row$summary_text[[1]] %||% ""
  source_title <- source_row$title[[1]] %||% ""
  if (
    !summary_looks_like_placeholder(current, source_title) &&
      nchar(current) <= 320
  ) {
    return(stringr::str_squish(current))
  }

  candidate <- extract_meaningful_sentence(claim_row$position_text[[1]] %||% "")
  if (!is.na(candidate) && candidate != "" && !summary_looks_like_placeholder(candidate, source_title)) {
    return(stringr::str_trunc(candidate, 320))
  }

  quote <- extract_meaningful_sentence(source_row$quote_text[[1]] %||% "")
  if (!is.na(quote) && quote != "" && !summary_looks_like_placeholder(quote, source_title)) {
    return(stringr::str_trunc(quote, 320))
  }

  title_summary <- title_based_public_summary(source_title, candidate_name = candidate_name)
  if (!is.na(title_summary) && title_summary != "") {
    return(stringr::str_trunc(title_summary, 320))
  }

  "La fuente aporta información pública trazable sobre la candidatura."
}

repair_claims_for_publication <- function(claims, sources, taxonomy, candidates = tibble::tibble()) {
  if (nrow(claims) == 0) {
    return(claims)
  }

  source_lookup <- split(sources, sources$source_id)
  candidate_lookup <- if (!is.null(candidates) && nrow(candidates) > 0) {
    stats::setNames(candidates$president_name %||% candidates$candidate_id, candidates$candidate_id)
  } else {
    character()
  }

  repaired <- purrr::map_dfr(seq_len(nrow(claims)), function(index) {
    claim_row <- claims[index, , drop = FALSE]
    source_row <- source_lookup[[claim_row$source_id[[1]] %||% ""]] %||% tibble::tibble()
    if (nrow(source_row) > 0) {
      source_row <- source_row[1, , drop = FALSE]
    }

    inferred_topic <- infer_topic_id_for_claim(claim_row, source_row, taxonomy)
    candidate_name <- candidate_lookup[[claim_row$candidate_id[[1]] %||% ""]] %||% ""
    summary <- compact_public_claim_summary(claim_row, source_row, candidate_name = candidate_name)
    position <- claim_row$position_text[[1]] %||% ""

    if (summary_looks_like_placeholder(position, source_row$title[[1]] %||% "") || nchar(position) > 900) {
      position <- summary
    }

    claim_row$topic_id <- inferred_topic
    claim_row$summary_text <- summary
    claim_row$position_text <- stringr::str_squish(position)
    claim_row$insufficient_evidence_flag <- FALSE
    claim_row
  })

  repaired |>
    dplyr::filter(!is.na(.data$candidate_id), .data$candidate_id != "") |>
    dplyr::filter(!is.na(.data$source_id), .data$source_id != "") |>
    dplyr::distinct(.data$claim_id, .keep_all = TRUE)
}

source_policy_positions_text <- function(source_claims) {
  policy_claims <- source_claims |>
    dplyr::filter(.data$claim_type == "policy_proposal") |>
    dplyr::arrange(dplyr::desc(.data$specificity_score), .data$summary_text)

  selected <- if (nrow(policy_claims) > 0) policy_claims else source_claims
  selected <- selected |>
    dplyr::distinct(.data$summary_text, .keep_all = TRUE) |>
    dplyr::slice_head(n = 4)

  paste(selected$summary_text, collapse = " | ")
}

source_ideology_signals_text <- function(source_claims) {
  text <- normalize_public_match_text(paste(source_claims$summary_text, source_claims$position_text, collapse = " "))
  signals <- character()

  if (grepl("impuesto|regla fiscal|austeridad|estado|subsidio|redistrib|pobreza|desigualdad", text, perl = TRUE)) {
    signals <- c(signals, "rol económico del Estado")
  }
  if (grepl("empresa|mercado|inversion|baja de impuestos|formalizacion|productividad", text, perl = TRUE)) {
    signals <- c(signals, "relación Estado-mercado")
  }
  if (grepl("seguridad|policia|militar|carcel|jep|paz|dialogo|sometimiento", text, perl = TRUE)) {
    signals <- c(signals, "seguridad, justicia y paz")
  }
  if (grepl("genero|mujer|diversidad|aborto|familia|cuidado", text, perl = TRUE)) {
    signals <- c(signals, "derechos y agenda social")
  }
  if (grepl("ambiente|energia|fracking|transicion|biodiversidad", text, perl = TRUE)) {
    signals <- c(signals, "ambiente y modelo productivo")
  }

  if (length(signals) == 0) {
    return("La fuente aporta contexto público, pero no deja una señal ideológica fuerte por sí sola.")
  }

  paste(unique(signals), collapse = ", ")
}

build_source_analysis_notes <- function(claims, sources, taxonomy = tibble::tibble()) {
  if (nrow(claims) == 0 || nrow(sources) == 0) {
    return(empty_source_analysis_notes_tibble())
  }

  topic_lookup <- taxonomy |>
    dplyr::select(.data$topic_id, topic_label = .data$label_public)

  claims |>
    dplyr::left_join(topic_lookup, by = "topic_id") |>
    dplyr::group_split(.data$source_id, .data$candidate_id) |>
    purrr::map_dfr(function(source_claims) {
      source_id <- source_claims$source_id[[1]]
      source_row <- sources |>
        dplyr::filter(.data$source_id == source_id) |>
        dplyr::slice_head(n = 1)

      if (nrow(source_row) == 0) {
        return(tibble::tibble())
      }

      topic_ids <- unique(stats::na.omit(source_claims$topic_id))
      topic_labels <- unique(stats::na.omit(source_claims$topic_label))
      if (length(topic_labels) == 0) {
        topic_labels <- topic_ids
      }

      policy_positions <- source_policy_positions_text(source_claims)
      lead_summary <- source_claims |>
        dplyr::arrange(dplyr::desc(.data$claim_type == "policy_proposal"), dplyr::desc(.data$specificity_score)) |>
        dplyr::slice_head(n = 1) |>
        dplyr::pull(.data$summary_text)

      tibble::tibble(
        source_analysis_id = paste0("source-analysis-", source_id),
        source_id = source_id,
        candidate_id = source_claims$candidate_id[[1]],
        topic_id = paste(topic_ids, collapse = "|"),
        substantive_summary = paste0(
          "La fuente ",
          source_row$source_name[[1]] %||% "registrada",
          " aporta evidencia sobre ",
          paste(topic_labels, collapse = ", "),
          ". En términos sustantivos, registra: ",
          policy_positions,
          "."
        ),
        policy_positions = policy_positions,
        ideology_signals = source_ideology_signals_text(source_claims),
        chronology_summary = lead_summary[[1]] %||% policy_positions,
        evidence_state = if ((source_row$confidence[[1]] %||% 0) >= 0.75) "trazable_con_cautela" else "trazable_baja_confianza",
        source_ids = source_id,
        claim_ids = paste(unique(source_claims$claim_id), collapse = "|")
      )
    }) |>
    dplyr::distinct(.data$source_analysis_id, .keep_all = TRUE)
}
