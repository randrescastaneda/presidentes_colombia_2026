empty_candidate_analysis_tibble <- function() {
  tibble::tibble(
    analysis_id = character(),
    candidate_id = character(),
    source_count = integer(),
    claim_count = integer(),
    uncertainty_count = integer(),
    profile_overview = character(),
    political_philosophy = character(),
    internal_coherence = character(),
    mainstream_distance = character()
  )
}

load_analysis_axes <- function(project_dir = ".") {
  path <- file.path(project_dir, "config", "analysis_axes.csv")
  if (!file.exists(path)) {
    return(tibble::tibble(
      axis_id = character(),
      label_public = character(),
      description = character(),
      pole_a = character(),
      pole_b = character(),
      default_question = character(),
      sort_order = integer()
    ))
  }

  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("axis_id", "label_public", "description", "pole_a", "pole_b", "default_question")),
        as.character
      ),
      sort_order = as.integer(sort_order)
    ) |>
    dplyr::arrange(sort_order, axis_id)
}

load_analysis_axis_rules <- function(project_dir = ".") {
  path <- file.path(project_dir, "config", "analysis_axis_rules.csv")
  if (!file.exists(path)) {
    return(tibble::tibble(
      axis_id = character(),
      match_field = character(),
      pattern = character(),
      pole = character(),
      rule_weight = numeric(),
      rationale_fragment = character()
    ))
  }

  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("axis_id", "match_field", "pattern", "pole", "rationale_fragment")),
        as.character
      ),
      rule_weight = as.numeric(rule_weight)
    ) |>
    dplyr::arrange(axis_id, dplyr::desc(rule_weight))
}

normalize_analysis_text <- function(x) {
  normalized <- iconv(dplyr::coalesce(as.character(x), ""), to = "ASCII//TRANSLIT")
  normalized[is.na(normalized)] <- ""
  tolower(normalized)
}

enrich_claims_for_analysis <- function(claims, sources, taxonomy = NULL) {
  topic_lookup <- if (is.null(taxonomy) || nrow(taxonomy) == 0) {
    tibble::tibble(topic_id = character(), topic_label = character())
  } else {
    taxonomy |>
      dplyr::transmute(topic_id, topic_label = label_public)
  }

  required_columns <- c(
    "mechanism_text",
    "problem_diagnosed",
    "target_population",
    "specificity_score",
    "ambiguity_flag",
    "insufficient_evidence_flag",
    "claim_type_id"
  )

  normalized_claims <- claims
  for (column_name in required_columns) {
    if (!column_name %in% names(normalized_claims)) {
      normalized_claims[[column_name]] <- NA
    }
  }

  normalized_claims |>
    dplyr::left_join(
      sources |>
        dplyr::select(source_id, source_confidence = confidence),
      by = "source_id"
    ) |>
    dplyr::left_join(topic_lookup, by = "topic_id") |>
    dplyr::mutate(
      source_confidence = dplyr::coalesce(source_confidence, 0.6),
      specificity_score = dplyr::coalesce(as.numeric(specificity_score), 0),
      topic_label = dplyr::coalesce(topic_label, topic_id, "Sin tema claro"),
      combined_text = normalize_analysis_text(
        paste(
          topic_id,
          policy_key,
          summary_text,
          position_text,
          mechanism_text,
          problem_diagnosed,
          target_population,
          sep = " | "
        )
      ),
      policy_key_norm = normalize_analysis_text(policy_key),
      topic_id_norm = normalize_analysis_text(topic_id)
    )
}

build_axis_match_table <- function(claims, sources, axis_rules, taxonomy = NULL) {
  if (nrow(claims) == 0 || nrow(axis_rules) == 0) {
    return(tibble::tibble(
      candidate_id = character(),
      claim_id = character(),
      source_id = character(),
      axis_id = character(),
      pole = character(),
      weight = numeric(),
      rationale_fragment = character(),
      topic_id = character(),
      policy_key = character()
    ))
  }

  enriched_claims <- enrich_claims_for_analysis(claims, sources, taxonomy)

  purrr::map_dfr(seq_len(nrow(axis_rules)), function(i) {
    rule <- axis_rules[i, , drop = FALSE]
    field_name <- rule$match_field[[1]]

    if (!field_name %in% names(enriched_claims)) {
      return(tibble::tibble())
    }

    field_values <- normalize_analysis_text(enriched_claims[[field_name]])
    matched <- grepl(rule$pattern[[1]], field_values, perl = TRUE)

    if (!any(matched)) {
      return(tibble::tibble())
    }

    enriched_claims[matched, , drop = FALSE] |>
      dplyr::transmute(
        candidate_id,
        claim_id,
        source_id,
        axis_id = rule$axis_id[[1]],
        pole = rule$pole[[1]],
        weight = source_confidence * (1 + (specificity_score / 3)) * rule$rule_weight[[1]],
        rationale_fragment = rule$rationale_fragment[[1]],
        topic_id,
        policy_key
      )
  })
}

axis_confidence <- function(score_a, score_b) {
  total_score <- score_a + score_b
  if (total_score <= 0) {
    return(0.1)
  }

  separation <- abs(score_a - score_b) / total_score
  confidence <- 0.25 + min(total_score / 4, 0.35) + (separation * 0.35)
  round(min(0.95, confidence), 2)
}

score_candidate_axes <- function(candidate_key, axis_matches, analysis_axes) {
  if (nrow(analysis_axes) == 0) {
    return(list())
  }

  purrr::pmap(analysis_axes, function(axis_id, label_public, description, pole_a, pole_b, default_question, sort_order) {
    candidate_axis_matches <- axis_matches |>
      dplyr::filter(candidate_id == candidate_key, axis_id == !!axis_id)

    pole_a_score <- candidate_axis_matches |>
      dplyr::filter(pole == "pole_a") |>
      dplyr::summarise(score = sum(weight, na.rm = TRUE)) |>
      dplyr::pull(score)

    pole_b_score <- candidate_axis_matches |>
      dplyr::filter(pole == "pole_b") |>
      dplyr::summarise(score = sum(weight, na.rm = TRUE)) |>
      dplyr::pull(score)

    pole_a_score <- pole_a_score %||% 0
    pole_b_score <- pole_b_score %||% 0
    total_score <- pole_a_score + pole_b_score

    top_fragments <- candidate_axis_matches |>
      dplyr::arrange(dplyr::desc(weight)) |>
      dplyr::distinct(rationale_fragment, .keep_all = TRUE) |>
      dplyr::slice_head(n = 3) |>
      dplyr::pull(rationale_fragment)

    placement <- if (total_score < 0.45) {
      "Señales programáticas pendientes"
    } else if ((abs(pole_a_score - pole_b_score) / total_score) < 0.2) {
      "Mixto o tensionado"
    } else if (pole_a_score >= pole_b_score) {
      pole_a
    } else {
      pole_b
    }

    rationale <- if (placement == "Señales programáticas pendientes") {
      paste0("No aparecen señales programáticas suficientes para ubicar este eje con seguridad: ", default_question)
    } else if (placement == "Mixto o tensionado") {
      paste0(
        "La evidencia relevante apunta en ambos sentidos; conviven señales hacia ",
        pole_a,
        " y ",
        pole_b,
        if (length(top_fragments) > 0) paste0(". Señales observadas: ", paste(top_fragments, collapse = "; ")) else "."
      )
    } else {
      paste0(
        "Predominan señales compatibles con ",
        placement,
        if (length(top_fragments) > 0) paste0(": ", paste(top_fragments, collapse = "; ")) else "."
      )
    }

    list(
      axis_id = axis_id,
      placement = placement,
      confidence = if (placement == "Señales programáticas pendientes") round(min(0.35, total_score), 2) else axis_confidence(pole_a_score, pole_b_score),
      rationale = rationale
    )
  })
}

topic_specificity_label <- function(topic_claims) {
  avg_specificity <- mean(topic_claims$specificity_score %||% 0, na.rm = TRUE)
  if (is.nan(avg_specificity) || avg_specificity < 0.75) {
    return("baja")
  }
  if (avg_specificity < 1.5) {
    return("media")
  }
  "alta"
}

topic_instrument_label <- function(topic_claims) {
  mechanism_text <- if ("mechanism_text" %in% names(topic_claims)) topic_claims$mechanism_text else rep(NA_character_, nrow(topic_claims))
  claim_type_id <- if ("claim_type_id" %in% names(topic_claims)) topic_claims$claim_type_id else rep(NA_character_, nrow(topic_claims))

  mechanism_count <- sum(!is.na(mechanism_text) & mechanism_text != "")
  vague_count <- sum(claim_type_id %in% c("promesa_vaga", "slogan"), na.rm = TRUE)

  if (mechanism_count >= 1) {
    return("instrumentos o mecanismos parcialmente explicitados")
  }
  if (vague_count >= 1) {
    return("promesas o marcos generales sin mecanismo claro")
  }
  "posturas o prioridades generales"
}

topic_tradeoffs <- function(topic_id) {
  dplyr::case_when(
    grepl("salud|educacion|vivienda|agua", topic_id %||% "", perl = TRUE) ~
      list(c("Mayor cobertura o intervención puede exigir más recursos fiscales y capacidad de ejecución.")),
    grepl("seguridad|defensa|justicia", topic_id %||% "", perl = TRUE) ~
      list(c("Más coerción o control puede tensionar garantías y requerir coordinación institucional sostenida.")),
    grepl("fiscal|impuestos|productividad|empresa", topic_id %||% "", perl = TRUE) ~
      list(c("Alivios económicos o incentivos pueden competir con metas de recaudo o gasto social.")),
    grepl("paz|territ", topic_id %||% "", perl = TRUE) ~
      list(c("La negociación territorial o de paz suele intercambiar velocidad de implementación por estabilidad y legitimidad.")),
    TRUE ~ list(c("La evidencia disponible no permite precisar todos los costos de oportunidad del tema."))
  )
}

topic_uncertainties <- function(topic_claims, topic_notes) {
  uncertainties <- character()

  if (nrow(topic_claims) <= 1) {
    uncertainties <- c(uncertainties, "La evidencia para este tema sigue siendo limitada en número de registros.")
  }

  if (topic_specificity_label(topic_claims) == "baja") {
    uncertainties <- c(uncertainties, "El nivel de especificidad todavía es bajo y no permite reconstruir un diseño completo.")
  }

  if (any(topic_claims$ambiguity_flag %in% TRUE, na.rm = TRUE)) {
    uncertainties <- c(uncertainties, "Hay formulaciones ambiguas en la evidencia relevada.")
  }

  if (any(topic_claims$insufficient_evidence_flag %in% TRUE, na.rm = TRUE)) {
    uncertainties <- c(uncertainties, "Parte de la evidencia del tema fue marcada como insuficiente.")
  }

  if (nrow(topic_notes) > 0) {
    uncertainties <- c(
      uncertainties,
      paste0("El sistema detectó alertas analíticas en este tema: ", paste(unique(topic_notes$analysis_type), collapse = ", "), ".")
    )
  }

  unique(uncertainties)
}

topic_feasibility_block <- function(topic_claims, topic_notes) {
  specificity <- topic_specificity_label(topic_claims)
  mechanism_count <- sum(!is.na(topic_claims$mechanism_text) & topic_claims$mechanism_text != "")
  contradiction_flag <- any(topic_notes$analysis_type %in% c("contradiccion_interna", "cambio_de_postura"))

  political <- dplyr::case_when(
    contradiction_flag ~ "La viabilidad política es incierta porque hay señales de tensión o cambio de postura.",
    specificity == "alta" ~ "La viabilidad política puede discutirse con mayor precisión porque hay instrumentos más visibles en la evidencia.",
    TRUE ~ "La viabilidad política no puede estimarse con firmeza porque el compromiso sigue formulado en términos generales."
  )

  fiscal <- dplyr::case_when(
    grepl("fiscal|impuesto|subsid|salud|educacion|vivienda|energia", paste(topic_claims$topic_id, collapse = " "), perl = TRUE) &
      mechanism_count == 0 ~ "El impacto fiscal potencial existe, pero la fuente no ofrece costeo ni mecanismo suficiente para evaluarlo.",
    mechanism_count >= 1 ~ "Se observan mecanismos o instrumentos, aunque todavía faltan cifras y secuencia de financiamiento.",
    TRUE ~ "No hay base suficiente para medir la carga fiscal asociada."
  )

  institutional <- dplyr::case_when(
    mechanism_count >= 1 ~ "La evidencia permite inferir que haría falta coordinación institucional identificable, aunque no siempre detallada.",
    TRUE ~ "La arquitectura institucional requerida no está suficientemente explicitada."
  )

  administrative <- dplyr::case_when(
    specificity == "alta" ~ "La ejecución administrativa parece imaginable, pero requeriría capacidades operativas aún no cuantificadas.",
    specificity == "media" ~ "Se puede inferir el tipo de despliegue administrativo, aunque faltan detalles de implementación.",
    TRUE ~ "La factibilidad administrativa es incierta porque el mecanismo no está desarrollado."
  )

  list(
    political = political,
    fiscal = fiscal,
    institutional = institutional,
    administrative = administrative
  )
}

build_thematic_analysis <- function(candidate_claims, analysis_notes, taxonomy = NULL) {
  if (nrow(candidate_claims) == 0) {
    return(list())
  }

  topic_lookup <- if (is.null(taxonomy) || nrow(taxonomy) == 0) {
    tibble::tibble(topic_id = unique(candidate_claims$topic_id), label_public = unique(candidate_claims$topic_id))
  } else {
    taxonomy |>
      dplyr::select(topic_id, label_public)
  }

  candidate_claims |>
    dplyr::filter(!is.na(topic_id), topic_id != "") |>
    dplyr::count(topic_id, sort = TRUE) |>
    dplyr::pull(topic_id) |>
    purrr::map(function(topic_key) {
      topic_claims <- candidate_claims |>
        dplyr::filter(topic_id == topic_key)

      topic_label <- topic_lookup |>
        dplyr::filter(topic_id == topic_key) |>
        dplyr::pull(label_public)
      topic_label <- topic_label[[1]] %||% topic_key

      topic_notes <- analysis_notes |>
        dplyr::filter(candidate_id == topic_claims$candidate_id[[1]]) |>
        dplyr::filter(
          purrr::map_lgl(
            claim_ids,
            \(claim_ids_text) any(strsplit(claim_ids_text %||% "", "[|]")[[1]] %in% topic_claims$claim_id)
          )
        )

      description_bits <- topic_claims |>
        dplyr::arrange(dplyr::desc(specificity_score), dplyr::desc(!is.na(mechanism_text) & mechanism_text != "")) |>
        dplyr::distinct(summary_text, .keep_all = TRUE) |>
        dplyr::slice_head(n = 2) |>
        dplyr::pull(summary_text)

      description <- paste0(
        "En ",
        topic_label,
        ", la evidencia pública observada se concentra en ",
        topic_instrument_label(topic_claims),
        ". ",
        paste(description_bits, collapse = " ")
      )

      inference <- dplyr::case_when(
        topic_specificity_label(topic_claims) == "alta" ~
          "La evidencia sugiere una prioridad programática relativamente desarrollada para este tema.",
        topic_specificity_label(topic_claims) == "media" ~
          "La evidencia apunta a una prioridad temática real, pero todavía incompleta en diseño y secuencia.",
        TRUE ~
          "La evidencia permite inferir una orientación general, más que un programa plenamente desarrollado."
      )

      evaluation <- dplyr::case_when(
        nrow(topic_notes) > 0 ~
          paste0(
            "El análisis requiere cautela: hay alertas registradas sobre ",
            paste(unique(topic_notes$analysis_type), collapse = ", "),
            "."
          ),
        topic_specificity_label(topic_claims) == "alta" ~
          "Comparativamente, este es uno de los temas mejor especificados dentro de la evidencia disponible.",
        TRUE ~
          "El principal límite analítico del tema es la falta de detalle sobre mecanismo, costos o secuencia de implementación."
      )

      uncertainties <- topic_uncertainties(topic_claims, topic_notes)

      list(
        topic_id = topic_key,
        description = description,
        inference = inference,
        evaluation = evaluation,
        feasibility = topic_feasibility_block(topic_claims, topic_notes),
        tradeoffs = topic_tradeoffs(topic_key)[[1]],
        uncertainties = if (length(uncertainties) == 0) {
          c("No se identificaron incertidumbres específicas adicionales a las ya visibles en la evidencia.")
        } else {
          uncertainties
        }
      )
    })
}

compose_profile_overview <- function(candidate_row, candidate_claims, candidate_sources, taxonomy = NULL) {
  candidate_name <- candidate_row$president_name[[1]] %||% candidate_row$candidate_id[[1]]

  if (nrow(candidate_claims) == 0) {
    return(paste0(
      candidate_name,
      " todavía no tiene una agenda programática suficientemente visible en las fuentes revisadas; por ahora la ficha debe leerse como cobertura inicial."
    ))
  }

  topic_labels <- if (!is.null(taxonomy) && nrow(taxonomy) > 0) {
    candidate_claims |>
      dplyr::count(topic_id, sort = TRUE) |>
      dplyr::left_join(
        taxonomy |>
          dplyr::select(topic_id, label_public),
        by = "topic_id"
      ) |>
      dplyr::slice_head(n = 3) |>
      dplyr::pull(label_public)
  } else {
    unique(candidate_claims$topic_id)
  }

  paste0(
    candidate_name,
    " aparece públicamente con una agenda que gira principalmente alrededor de ",
    paste(topic_labels, collapse = ", "),
    ". Esa combinación permite leer la candidatura menos como una suma de noticias aisladas y más como una oferta política en construcción, con énfasis que deben contrastarse en las propuestas temáticas."
  )
}

compose_political_philosophy <- function(ideology_axes) {
  if (length(ideology_axes) == 0) {
    return("Aún no hay señales programáticas trazables para inferir una filosofía política subyacente.")
  }

  informative_axes <- purrr::keep(
    ideology_axes,
    \(axis) axis$placement != "Señales programáticas pendientes" && axis$confidence >= 0.45
  )

  if (length(informative_axes) == 0) {
    return("La filosofía política subyacente no puede inferirse con suficiente sustento a partir de la evidencia disponible.")
  }

  highlighted <- informative_axes[seq_len(min(3, length(informative_axes)))]
  highlighted_text <- paste(
    purrr::map_chr(highlighted, \(axis) paste0(axis$axis_id, ": ", axis$placement)),
    collapse = "; "
  )

  paste0(
    "La evidencia sugiere una filosofía política que puede resumirse, de forma tentativa, en estos ejes dominantes: ",
    highlighted_text,
    ". Esta lectura sigue siendo analítica y no debe confundirse con una autodefinición ideológica del candidato."
  )
}

compose_internal_coherence <- function(candidate_claims, candidate_notes) {
  if (nrow(candidate_claims) == 0) {
    return("No hay base suficiente para evaluar coherencia interna.")
  }

  contradiction_notes <- candidate_notes |>
    dplyr::filter(analysis_type %in% c("contradiccion_interna", "cambio_de_postura"))

  implementation_notes <- candidate_notes |>
    dplyr::filter(analysis_type == "vacio_de_implementacion")

  if (nrow(contradiction_notes) > 0) {
    return("Hay señales de tensión interna o cambio de postura en parte del programa. La coherencia no puede asumirse como estable.")
  }

  if (nrow(implementation_notes) > 0) {
    return("No aparecen contradicciones directas relevantes, pero la coherencia sustantiva sigue limitada por vacíos de implementación.")
  }

  "Con la evidencia disponible no aparecen contradicciones internas fuertes; aun así, la evaluación sigue condicionada al nivel de detalle público observado."
}

compose_mainstream_distance <- function(ideology_axes, candidate_claims) {
  if (nrow(candidate_claims) == 0 || length(ideology_axes) == 0) {
    return("Aún no hay señales programáticas trazables para ubicar la distancia frente al mainstream político.")
  }

  placement_lookup <- purrr::set_names(
    purrr::map_chr(ideology_axes, "placement"),
    purrr::map_chr(ideology_axes, "axis_id")
  )

  confidence_lookup <- purrr::set_names(
    purrr::map_dbl(ideology_axes, "confidence"),
    purrr::map_chr(ideology_axes, "axis_id")
  )

  lookup_value <- function(named_vector, key) {
    if (!key %in% names(named_vector)) {
      return(NA)
    }

    named_vector[[key]]
  }

  disruptive_flag <- identical(lookup_value(placement_lookup, "gradualismo_vs_disrupcion"), "Disrupcion") &&
    (lookup_value(confidence_lookup, "gradualismo_vs_disrupcion") %||% 0) >= 0.55
  populist_flag <- identical(lookup_value(placement_lookup, "tecnocracia_vs_populismo"), "Populismo") &&
    (lookup_value(confidence_lookup, "tecnocracia_vs_populismo") %||% 0) >= 0.55
  institutional_flag <- identical(lookup_value(placement_lookup, "institucionalismo_vs_personalismo"), "Institucionalismo") &&
    (lookup_value(confidence_lookup, "institucionalismo_vs_personalismo") %||% 0) >= 0.55
  gradual_flag <- identical(lookup_value(placement_lookup, "gradualismo_vs_disrupcion"), "Gradualismo") &&
    (lookup_value(confidence_lookup, "gradualismo_vs_disrupcion") %||% 0) >= 0.55

  if (disruptive_flag || populist_flag) {
    return("La evidencia sugiere una distancia relativamente alta frente al mainstream, sobre todo por el tono de ruptura o por apelaciones menos institucionales.")
  }

  if (institutional_flag && gradual_flag) {
    return("La evidencia disponible ubica al candidato relativamente cerca del mainstream político-programático, con énfasis más incremental e institucional.")
  }

  "La distancia frente al mainstream parece intermedia: hay rasgos de continuidad con algunos énfasis diferenciadores, pero todavía no una ruptura programática plenamente demostrada."
}

compose_strengths <- function(candidate_claims, ideology_axes, candidate_notes, taxonomy = NULL) {
  strengths <- character()

  if (nrow(candidate_claims) == 0) {
    return(c("No hay suficiente evidencia para identificar fortalezas programáticas con rigor."))
  }

  specific_topics <- candidate_claims |>
    dplyr::filter(specificity_score >= 1) |>
    dplyr::count(topic_id, sort = TRUE) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(topic_id)

  if (length(specific_topics) > 0) {
    strengths <- c(
      strengths,
      paste0("Hay mayor desarrollo relativo en temas como ", paste(specific_topics, collapse = ", "), ".")
    )
  }

  if (!any(candidate_notes$analysis_type %in% c("contradiccion_interna", "cambio_de_postura"))) {
    strengths <- c(strengths, "No se observan contradicciones internas fuertes en la evidencia disponible.")
  }

  informative_axes <- purrr::keep(
    ideology_axes,
    \(axis) axis$placement != "Señales programáticas pendientes" && axis$confidence >= 0.55
  )
  if (length(informative_axes) >= 3) {
    strengths <- c(strengths, "La orientación programática ya muestra cierta consistencia en varios ejes analíticos.")
  }

  unique(strengths)
}

compose_weaknesses <- function(candidate_claims, candidate_notes) {
  weaknesses <- character()

  if (nrow(candidate_claims) == 0) {
    return(c("La principal debilidad es la ausencia de evidencia programática suficiente."))
  }

  if (sum(candidate_claims$claim_type_id %in% c("promesa_vaga", "slogan"), na.rm = TRUE) > 0) {
    weaknesses <- c(weaknesses, "Persisten piezas de baja especificidad que no permiten reconstruir instrumentos de política pública.")
  }

  if (mean(candidate_claims$specificity_score %||% 0, na.rm = TRUE) < 1) {
    weaknesses <- c(weaknesses, "El nivel general de especificidad todavía es bajo para una evaluación programática robusta.")
  }

  if (any(candidate_notes$analysis_type %in% c("contradiccion_interna", "cambio_de_postura"))) {
    weaknesses <- c(weaknesses, "Hay señales de tensión interna o cambios de postura que debilitan la coherencia del programa.")
  }

  if (dplyr::n_distinct(candidate_claims$source_id) <= 1) {
    weaknesses <- c(weaknesses, "La base de evidencia pública sigue siendo estrecha y eso limita la comparación.")
  }

  unique(weaknesses)
}

compose_uncertainties <- function(candidate_claims, ideology_axes, thematic_analysis) {
  uncertainties <- character()

  if (nrow(candidate_claims) == 0) {
    return(c("Aún no hay material público trazable suficiente para producir un análisis programático sólido."))
  }

  insufficient_axes <- purrr::keep(ideology_axes, \(axis) axis$placement == "Señales programáticas pendientes")
  if (length(insufficient_axes) > 0) {
    uncertainties <- c(
      uncertainties,
      paste0(
        "Faltan señales programáticas comparables en varios ejes: ",
        paste(purrr::map_chr(insufficient_axes, "axis_id"), collapse = ", "),
        "."
      )
    )
  }

  topic_uncertainty_count <- sum(purrr::map_int(thematic_analysis, \(topic) length(topic$uncertainties %||% character())))
  if (topic_uncertainty_count > 0) {
    uncertainties <- c(uncertainties, "Varios temas ya permiten describir prioridades, pero aún falta detalle público sobre mecanismos, costos o secuencia de implementación.")
  }

  unique(uncertainties)
}

build_candidate_analysis_artifacts <- function(
  candidates,
  claims,
  sources,
  analysis_notes,
  taxonomy,
  analysis_axes,
  axis_rules,
  report_date = Sys.Date()
) {
  if (nrow(candidates) == 0) {
    return(list())
  }

  axis_matches <- build_axis_match_table(claims, sources, axis_rules, taxonomy)

  artifacts <- purrr::pmap(candidates, function(...) {
    candidate_row <- tibble::as_tibble(list(...))
    candidate_key <- candidate_row$candidate_id[[1]]
    candidate_claims <- claims |>
      dplyr::filter(candidate_id == candidate_key)
    candidate_sources <- sources |>
      dplyr::filter(candidate_id == candidate_key)
    candidate_notes <- analysis_notes |>
      dplyr::filter(candidate_id == candidate_key)

    ideology_axes <- score_candidate_axes(candidate_key, axis_matches, analysis_axes)
    thematic_analysis <- build_thematic_analysis(candidate_claims, candidate_notes, taxonomy)

    list(
      analysis_id = paste0("candidate-analysis-", candidate_key, "-", as.character(report_date)),
      candidate_id = candidate_key,
      source_ids = unique(candidate_sources$source_id),
      claim_ids = unique(candidate_claims$claim_id),
      profile_overview = compose_profile_overview(candidate_row, candidate_claims, candidate_sources, taxonomy),
      political_philosophy = compose_political_philosophy(ideology_axes),
      ideology_axes = ideology_axes,
      thematic_analysis = thematic_analysis,
      internal_coherence = compose_internal_coherence(candidate_claims, candidate_notes),
      mainstream_distance = compose_mainstream_distance(ideology_axes, candidate_claims),
      strengths = compose_strengths(candidate_claims, ideology_axes, candidate_notes, taxonomy),
      weaknesses = compose_weaknesses(candidate_claims, candidate_notes),
      uncertainties = compose_uncertainties(candidate_claims, ideology_axes, thematic_analysis)
    )
  })

  stats::setNames(artifacts, vapply(artifacts, `[[`, character(1), "candidate_id"))
}

write_candidate_analysis_artifacts <- function(analysis_artifacts, project_dir = ".", report_date = Sys.Date()) {
  if (length(analysis_artifacts) == 0) {
    return(character())
  }

  output_dir <- file.path(project_dir, "data", "staging", "analysis", as.character(report_date))

  paths <- purrr::imap_chr(analysis_artifacts, function(artifact, candidate_id) {
    path <- file.path(output_dir, paste0(candidate_id, ".json"))
    write_contract_json(artifact, path)
    path
  })

  unname(paths)
}

load_candidate_analysis_artifacts <- function(project_dir = ".") {
  analysis_dir <- file.path(project_dir, "data", "staging", "analysis")
  if (!dir.exists(analysis_dir)) {
    return(list())
  }

  files <- list.files(analysis_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
}

candidate_analysis_summary_tibble <- function(analysis_artifacts) {
  if (length(analysis_artifacts) == 0) {
    return(empty_candidate_analysis_tibble())
  }

  purrr::map_dfr(analysis_artifacts, function(artifact) {
    tibble::tibble(
      analysis_id = artifact$analysis_id %||% NA_character_,
      candidate_id = artifact$candidate_id %||% NA_character_,
      source_count = length(artifact$source_ids %||% character()),
      claim_count = length(artifact$claim_ids %||% character()),
      uncertainty_count = length(artifact$uncertainties %||% character()),
      profile_overview = artifact$profile_overview %||% NA_character_,
      political_philosophy = artifact$political_philosophy %||% NA_character_,
      internal_coherence = artifact$internal_coherence %||% NA_character_,
      mainstream_distance = artifact$mainstream_distance %||% NA_character_
    )
  })
}
