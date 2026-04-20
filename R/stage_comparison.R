normalize_nested_objects <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(list())
  }

  if (inherits(x, "data.frame")) {
    return(split(x, seq_len(nrow(x))))
  }

  if (is.list(x) && !is.null(names(x)) && !all(names(x) == "")) {
    return(list(x))
  }

  x
}

select_comparison_candidates <- function(candidates) {
  if (nrow(candidates) == 0) {
    return(candidates)
  }

  if ("watchlist_active" %in% names(candidates) && any(candidates$watchlist_active %in% TRUE, na.rm = TRUE)) {
    return(
      candidates |>
        dplyr::filter(.data$watchlist_active %in% TRUE) |>
        dplyr::arrange(.data$watchlist_priority, .data$ballot_position, .data$candidate_id)
    )
  }

  candidates |>
    dplyr::arrange(.data$ballot_position, .data$candidate_id)
}

default_axis_position <- function(axis_id, candidate_id) {
  list(
    axis_id = axis_id,
    candidate_id = candidate_id,
    placement = "Evidencia insuficiente",
    confidence = 0.1
  )
}

candidate_axis_lookup <- function(artifact) {
  axes <- normalize_nested_objects(artifact$ideology_axes)
  stats::setNames(axes, purrr::map_chr(axes, \(axis) axis$axis_id %||% NA_character_))
}

find_thematic_row <- function(artifact, topic_id) {
  themes <- normalize_nested_objects(artifact$thematic_analysis)
  matches <- purrr::keep(themes, \(theme) identical(theme$topic_id %||% NA_character_, topic_id))
  matches[[1]] %||% NULL
}

topic_priority_label <- function(topic_claims) {
  if (nrow(topic_claims) == 0) {
    return("sin evidencia suficiente")
  }

  if (nrow(topic_claims) >= 2 || mean(topic_claims$specificity_score %||% 0, na.rm = TRUE) >= 1.5) {
    return("alta")
  }

  if (nrow(topic_claims) >= 1) {
    return("media")
  }

  "baja"
}

topic_feasibility_summary <- function(theme_row) {
  if (is.null(theme_row)) {
    return("sin base suficiente")
  }

  feasibility <- theme_row$feasibility
  feasibility_text <- paste(
    feasibility$political %||% "",
    feasibility$fiscal %||% "",
    feasibility$institutional %||% "",
    feasibility$administrative %||% ""
  )
  feasibility_text <- normalize_analysis_text(feasibility_text)

  dplyr::case_when(
    grepl("no hay base suficiente|incierta|no puede", feasibility_text, perl = TRUE) ~ "incierta",
    grepl("parcial|faltan detalles|faltan cifras", feasibility_text, perl = TRUE) ~ "parcialmente evaluable",
    TRUE ~ "relativamente evaluable"
  )
}

topic_coherence_summary <- function(theme_row, artifact) {
  if (is.null(theme_row)) {
    return("sin evidencia suficiente")
  }

  evaluation_text <- normalize_analysis_text(theme_row$evaluation %||% "")
  coherence_text <- normalize_analysis_text(artifact$internal_coherence %||% "")

  dplyr::case_when(
    grepl("tension|cambio de postura|contradic", paste(evaluation_text, coherence_text), perl = TRUE) ~ "con tensiones visibles",
    grepl("vacio|falt", paste(evaluation_text, coherence_text), perl = TRUE) ~ "limitada por baja especificidad",
    TRUE ~ "sin tensiones fuertes visibles"
  )
}

build_axes_comparison <- function(candidate_analysis, candidate_ids, analysis_axes) {
  if (nrow(analysis_axes) == 0) {
    return(list())
  }

  purrr::pmap(analysis_axes, function(axis_id, label_public, description, pole_a, pole_b, default_question, sort_order) {
    candidate_positions <- purrr::map(candidate_ids, function(candidate_id) {
      artifact <- candidate_analysis[[candidate_id]]
      axis_row <- if (is.null(artifact)) NULL else candidate_axis_lookup(artifact)[[axis_id]]

      if (is.null(axis_row)) {
        return(default_axis_position(axis_id, candidate_id))
      }

      list(
        candidate_id = candidate_id,
        placement = axis_row$placement %||% "Evidencia insuficiente",
        confidence = axis_row$confidence %||% 0.1
      )
    })

    non_empty_positions <- purrr::keep(candidate_positions, \(row) row$placement != "Evidencia insuficiente")
    unique_placements <- unique(purrr::map_chr(non_empty_positions, "placement"))

    summary <- if (length(unique_placements) == 0) {
      "La comparación todavía no tiene evidencia suficiente para diferenciar a los candidatos en este eje."
    } else if (length(unique_placements) == 1) {
      paste0("La evidencia disponible acerca a los candidatos comparados a una misma zona del eje: ", unique_placements[[1]], ".")
    } else {
      paste0("La comparación muestra diferencias sustantivas en este eje: ", paste(unique_placements, collapse = " vs. "), ".")
    }

    list(
      axis_id = axis_id,
      candidate_positions = candidate_positions,
      summary = summary
    )
  })
}

build_topic_comparison <- function(candidate_analysis, candidate_ids, claims, taxonomy = NULL) {
  if (length(candidate_analysis) == 0 && nrow(claims) == 0) {
    return(list())
  }

  topic_ids <- unique(c(
    claims$topic_id,
    unlist(
      purrr::map(candidate_analysis, function(artifact) {
        purrr::map_chr(normalize_nested_objects(artifact$thematic_analysis), \(theme) theme$topic_id %||% NA_character_)
      })
    )
  ))
  topic_ids <- topic_ids[!is.na(topic_ids) & topic_ids != ""]

  purrr::map(topic_ids, function(topic_id) {
    candidate_rows <- purrr::map(candidate_ids, function(candidate_id) {
      artifact <- candidate_analysis[[candidate_id]]
      topic_claims <- claims |>
        dplyr::filter(candidate_id == !!candidate_id, topic_id == !!topic_id)
      theme_row <- if (is.null(artifact)) NULL else find_thematic_row(artifact, topic_id)

      list(
        candidate_id = candidate_id,
        priority = topic_priority_label(topic_claims),
        instrument = if (nrow(topic_claims) == 0) "sin evidencia suficiente" else topic_instrument_label(topic_claims),
        specificity = if (nrow(topic_claims) == 0) "sin evidencia suficiente" else topic_specificity_label(topic_claims),
        coherence = if (is.null(artifact)) "sin evidencia suficiente" else topic_coherence_summary(theme_row, artifact),
        feasibility = topic_feasibility_summary(theme_row)
      )
    })

    high_priority <- purrr::keep(candidate_rows, \(row) row$priority == "alta")
    summary <- if (length(high_priority) == 0) {
      "Ningún candidato comparado tiene todavía suficiente desarrollo público para priorizar con claridad este tema."
    } else {
      paste0(
        "El tema permite comparación útil porque destacan ",
        paste(purrr::map_chr(high_priority, "candidate_id"), collapse = ", "),
        " con mayor prioridad relativa."
      )
    }

    list(
      topic_id = topic_id,
      candidate_rows = candidate_rows,
      summary = summary
    )
  })
}

build_comparison_convergences <- function(axes_comparison, topic_comparison) {
  axis_convergences <- purrr::keep(axes_comparison, function(axis_row) {
    placements <- unique(purrr::map_chr(axis_row$candidate_positions, "placement"))
    length(placements) == 1 && placements[[1]] != "Evidencia insuficiente"
  })

  topic_convergences <- purrr::keep(topic_comparison, function(topic_row) {
    priorities <- unique(purrr::map_chr(topic_row$candidate_rows, "priority"))
    length(priorities) == 1 && priorities[[1]] != "sin evidencia suficiente"
  })

  c(
    purrr::map_chr(axis_convergences, \(axis_row) paste0("Convergencia en ", axis_row$axis_id, ": ", axis_row$summary)),
    purrr::map_chr(topic_convergences, \(topic_row) paste0("Convergencia temática en ", topic_row$topic_id, ": ", topic_row$summary))
  )
}

build_comparison_divergences <- function(axes_comparison, topic_comparison) {
  axis_divergences <- purrr::keep(axes_comparison, function(axis_row) {
    placements <- unique(purrr::map_chr(axis_row$candidate_positions, "placement"))
    length(setdiff(placements, "Evidencia insuficiente")) >= 2
  })

  topic_divergences <- purrr::keep(topic_comparison, function(topic_row) {
    priorities <- unique(purrr::map_chr(topic_row$candidate_rows, "priority"))
    length(setdiff(priorities, "sin evidencia suficiente")) >= 2
  })

  c(
    purrr::map_chr(axis_divergences, \(axis_row) paste0("Diferencia en ", axis_row$axis_id, ": ", axis_row$summary)),
    purrr::map_chr(topic_divergences, \(topic_row) paste0("Diferencia temática en ", topic_row$topic_id, ": ", topic_row$summary))
  )
}

build_comparison_uncertainties <- function(candidate_analysis, axes_comparison, topic_comparison) {
  uncertainties <- purrr::map_chr(candidate_analysis, function(artifact) {
    candidate_uncertainties <- artifact$uncertainties %||% character()
    if (length(candidate_uncertainties) == 0) {
      return(NA_character_)
    }

    paste0(artifact$candidate_id %||% "candidato", ": ", candidate_uncertainties[[1]])
  })

  axis_uncertainties <- purrr::map_chr(
    purrr::keep(axes_comparison, \(axis_row) any(purrr::map_chr(axis_row$candidate_positions, "placement") == "Evidencia insuficiente")),
    \(axis_row) paste0("Persisten vacíos de evidencia en el eje ", axis_row$axis_id, ".")
  )

  topic_uncertainties <- purrr::map_chr(
    purrr::keep(topic_comparison, \(topic_row) any(purrr::map_chr(topic_row$candidate_rows, "priority") == "sin evidencia suficiente")),
    \(topic_row) paste0("No todos los candidatos tienen evidencia suficiente en el tema ", topic_row$topic_id, ".")
  )

  unique(stats::na.omit(c(uncertainties, axis_uncertainties, topic_uncertainties)))
}

build_comparison_report <- function(candidate_analysis, candidates, claims, analysis_axes, report_date = Sys.Date()) {
  selected_candidates <- select_comparison_candidates(candidates)
  candidate_ids <- selected_candidates$candidate_id

  if (length(candidate_ids) == 0) {
    return(NULL)
  }

  filtered_analysis <- candidate_analysis[candidate_ids]
  axes_comparison <- build_axes_comparison(filtered_analysis, candidate_ids, analysis_axes)
  topic_comparison <- build_topic_comparison(filtered_analysis, candidate_ids, claims)
  source_ids <- unique(unlist(purrr::map(filtered_analysis, \(artifact) artifact$source_ids %||% character())))
  claim_ids <- unique(unlist(purrr::map(filtered_analysis, \(artifact) artifact$claim_ids %||% character())))

  list(
    report_id = paste0("comparison-watchlist-", as.character(report_date)),
    scope_label = "watchlist_activa",
    candidate_ids = candidate_ids,
    source_ids = source_ids,
    claim_ids = claim_ids,
    axes_comparison = axes_comparison,
    topic_comparison = topic_comparison,
    convergences = build_comparison_convergences(axes_comparison, topic_comparison),
    divergences = build_comparison_divergences(axes_comparison, topic_comparison),
    uncertainties = build_comparison_uncertainties(filtered_analysis, axes_comparison, topic_comparison)
  )
}

write_comparison_report <- function(comparison_report, project_dir = ".", report_date = Sys.Date()) {
  if (is.null(comparison_report)) {
    return(character())
  }

  path <- file.path(
    project_dir,
    "data",
    "staging",
    "comparison",
    as.character(report_date),
    paste0(comparison_report$report_id, ".json")
  )
  write_contract_json(comparison_report, path)
  path
}

load_comparison_reports <- function(project_dir = ".") {
  comparison_dir <- file.path(project_dir, "data", "staging", "comparison")
  if (!dir.exists(comparison_dir)) {
    return(list())
  }

  files <- list.files(comparison_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
}

comparison_report_summary_tibble <- function(comparison_report) {
  if (is.null(comparison_report)) {
    return(tibble::tibble())
  }

  tibble::tibble(
    report_id = comparison_report$report_id %||% NA_character_,
    scope_label = comparison_report$scope_label %||% NA_character_,
    candidate_count = length(comparison_report$candidate_ids %||% character()),
    axis_count = length(comparison_report$axes_comparison %||% list()),
    topic_count = length(comparison_report$topic_comparison %||% list()),
    convergence_count = length(comparison_report$convergences %||% character()),
    divergence_count = length(comparison_report$divergences %||% character()),
    uncertainty_count = length(comparison_report$uncertainties %||% character())
  )
}
