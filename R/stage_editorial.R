build_editorial_section <- function(section_id, heading, content_type, body) {
  list(
    section_id = section_id,
    heading = heading,
    content_type = content_type,
    body = body
  )
}

collapse_theme_summaries <- function(thematic_analysis, field, limit = 3) {
  themes <- normalize_nested_objects(thematic_analysis)
  if (length(themes) == 0) {
    return("No hay evidencia temática suficiente.")
  }

  values <- purrr::map_chr(
    themes[seq_len(min(limit, length(themes)))],
    \(theme) paste0(theme$topic_id %||% "tema", ": ", theme[[field]] %||% "sin detalle")
  )

  paste(values, collapse = " ")
}

build_candidate_profile_package <- function(artifact, candidates, report_date = Sys.Date()) {
  candidate_row <- candidates |>
    dplyr::filter(candidate_id == artifact$candidate_id) |>
    dplyr::slice_head(n = 1)

  candidate_name <- candidate_row$president_name[[1]] %||% artifact$candidate_id

  list(
    artifact_id = paste0("candidate-profile-", artifact$candidate_id, "-", as.character(report_date)),
    artifact_type = "candidate_profile",
    title = paste0(candidate_name, ": perfil politico-programatico"),
    dek = paste0("Lectura estructurada y no partidista de la evidencia disponible sobre ", candidate_name, "."),
    candidate_ids = c(artifact$candidate_id),
    source_ids = artifact$source_ids %||% character(),
    claim_ids = artifact$claim_ids %||% character(),
    sections = list(
      build_editorial_section("profile_overview", "Perfil general", "description", artifact$profile_overview %||% "Sin evidencia suficiente."),
      build_editorial_section("political_philosophy", "Filosofía política", "inference", artifact$political_philosophy %||% "Sin evidencia suficiente."),
      build_editorial_section("thematic_proposals", "Propuestas por áreas", "mixed", collapse_theme_summaries(artifact$thematic_analysis, "description")),
      build_editorial_section("internal_coherence", "Coherencia interna", "evaluation", artifact$internal_coherence %||% "Sin evidencia suficiente."),
      build_editorial_section("multidimensional_ideology", "Ubicación ideológica multidimensional", "inference", collapse_theme_summaries(artifact$ideology_axes, "rationale")),
      build_editorial_section("mainstream_distance", "Distancia frente al mainstream", "evaluation", artifact$mainstream_distance %||% "Sin evidencia suficiente."),
      build_editorial_section("strengths", "Fortalezas programáticas", "evaluation", paste(artifact$strengths %||% "Sin fortalezas identificables todavía.", collapse = " ")),
      build_editorial_section("weaknesses_and_uncertainties", "Debilidades e incertidumbres", "evaluation", paste(c(artifact$weaknesses, artifact$uncertainties) %||% "Sin detalle suficiente.", collapse = " "))
    )
  )
}

build_comparison_editorial_package <- function(comparison_report, candidates, report_date = Sys.Date()) {
  if (is.null(comparison_report)) {
    return(NULL)
  }

  candidate_names <- candidates |>
    dplyr::filter(candidate_id %in% comparison_report$candidate_ids) |>
    dplyr::pull(president_name)

  list(
    artifact_id = paste0("comparison-report-", as.character(report_date)),
    artifact_type = "comparison_report",
    title = paste0("Comparativa transversal: ", paste(candidate_names, collapse = ", ")),
    dek = "Comparación simétrica de prioridades, instrumentos, especificidad, factibilidad y ejes ideológicos.",
    candidate_ids = comparison_report$candidate_ids,
    source_ids = comparison_report$source_ids %||% character(),
    claim_ids = comparison_report$claim_ids %||% character(),
    sections = list(
      build_editorial_section("scope_note", "Alcance", "description", "La comparación usa la misma estructura y los mismos ejes para todos los candidatos incluidos en la watchlist activa."),
      build_editorial_section("priorities_comparison", "Prioridades temáticas", "description", paste(purrr::map_chr(comparison_report$topic_comparison, "summary"), collapse = " ")),
      build_editorial_section("instruments_comparison", "Instrumentos de política", "inference", paste(purrr::map_chr(comparison_report$topic_comparison, function(topic_row) {
        paste0(topic_row$topic_id, ": ", paste(purrr::map_chr(topic_row$candidate_rows, \(row) paste0(row$candidate_id, "=", row$instrument)), collapse = "; "))
      }), collapse = " ")),
      build_editorial_section("specificity_comparison", "Grado de especificidad", "evaluation", paste(purrr::map_chr(comparison_report$topic_comparison, function(topic_row) {
        paste0(topic_row$topic_id, ": ", paste(purrr::map_chr(topic_row$candidate_rows, \(row) paste0(row$candidate_id, "=", row$specificity)), collapse = "; "))
      }), collapse = " ")),
      build_editorial_section("feasibility_comparison", "Factibilidad", "evaluation", paste(purrr::map_chr(comparison_report$topic_comparison, function(topic_row) {
        paste0(topic_row$topic_id, ": ", paste(purrr::map_chr(topic_row$candidate_rows, \(row) paste0(row$candidate_id, "=", row$feasibility)), collapse = "; "))
      }), collapse = " ")),
      build_editorial_section("ideology_axes_comparison", "Ejes ideológicos", "inference", paste(purrr::map_chr(comparison_report$axes_comparison, function(axis_row) {
        paste0(axis_row$axis_id, ": ", paste(purrr::map_chr(axis_row$candidate_positions, \(row) paste0(row$candidate_id, "=", row$placement)), collapse = "; "))
      }), collapse = " ")),
      build_editorial_section("convergences_and_divergences", "Convergencias y divergencias", "evaluation", paste(c(comparison_report$convergences, comparison_report$divergences), collapse = " ")),
      build_editorial_section("unresolved_uncertainties", "Incertidumbres abiertas", "evaluation", paste(comparison_report$uncertainties %||% "Sin incertidumbres registradas.", collapse = " "))
    )
  )
}

build_daily_update_package <- function(claims, candidates, report_date = Sys.Date()) {
  claims_of_day <- claims |>
    dplyr::filter(event_date == as.Date(report_date))

  top_candidates <- claims_of_day |>
    dplyr::count(candidate_id, sort = TRUE) |>
    dplyr::slice_head(n = 3) |>
    dplyr::left_join(
      candidates |>
        dplyr::select(candidate_id, president_name),
      by = "candidate_id"
    )

  list(
    artifact_id = paste0("daily-update-", as.character(report_date)),
    artifact_type = "daily_update",
    title = paste0("Actualizacion diaria: ", as.character(report_date)),
    dek = "Resumen breve de cambios, nuevas propuestas y preguntas abiertas del día.",
    candidate_ids = unique(claims_of_day$candidate_id),
    source_ids = unique(claims_of_day$source_id),
    claim_ids = unique(claims_of_day$claim_id),
    sections = list(
      build_editorial_section("what_changed", "Qué cambió", "description", if (nrow(claims_of_day) == 0) "No hubo nuevos registros públicos procesados para la fecha." else paste0("Se procesaron ", nrow(claims_of_day), " afirmaciones nuevas o actualizadas.")),
      build_editorial_section("new_proposals", "Nuevas propuestas", "description", if (nrow(top_candidates) == 0) "No hubo nuevas propuestas comparables." else paste0("Los mayores movimientos del día se concentraron en ", paste(top_candidates$president_name, collapse = ", "), ".")),
      build_editorial_section(
        "notable_reframing",
        "Reencuadres o matices",
        "inference",
        if (nrow(claims_of_day) == 0) {
          "Sin reencuadres detectables."
        } else {
          "La revisión diaria debe leer estos cambios como actualización incremental, no como reescritura completa del programa."
        }
      ),
      build_editorial_section("open_questions", "Preguntas abiertas", "evaluation", "La publicación diaria debe seguir marcando explícitamente qué temas siguen con evidencia insuficiente o baja especificidad.")
    )
  )
}

build_homepage_brief_package <- function(daily_update_package, comparison_package, report_date = Sys.Date()) {
  comparison_note <- if (is.null(comparison_package)) {
    "Todavía no hay una comparativa transversal suficiente."
  } else {
    comparison_package$sections[[6]]$body %||% "La comparativa sigue en construcción."
  }

  comparison_candidate_ids <- if (is.null(comparison_package)) {
    character()
  } else {
    comparison_package$candidate_ids %||% character()
  }

  list(
    artifact_id = paste0("homepage-brief-", as.character(report_date)),
    artifact_type = "homepage_brief",
    title = "Resumen ejecutivo",
    dek = "Resumen breve para portada con los cambios más relevantes del seguimiento diario.",
    candidate_ids = unique(c(daily_update_package$candidate_ids, comparison_candidate_ids)),
    source_ids = daily_update_package$source_ids %||% character(),
    claim_ids = daily_update_package$claim_ids %||% character(),
    sections = list(
      build_editorial_section("top_changes", "Cambios principales", "description", daily_update_package$sections[[1]]$body %||% "Sin cambios principales."),
      build_editorial_section("key_comparison_note", "Clave comparativa", "inference", comparison_note),
      build_editorial_section("caveats", "Caveats", "evaluation", "Este resumen no reemplaza los perfiles detallados ni la comparación completa; sintetiza solo lo más visible del día.")
    )
  )
}

build_editorial_packages <- function(candidate_analysis, comparison_report, claims, candidates, report_date = Sys.Date()) {
  candidate_packages <- purrr::map(
    candidate_analysis,
    build_candidate_profile_package,
    candidates = candidates,
    report_date = report_date
  )

  comparison_package <- build_comparison_editorial_package(comparison_report, candidates, report_date)
  daily_update_package <- build_daily_update_package(claims, candidates, report_date)
  homepage_brief_package <- build_homepage_brief_package(daily_update_package, comparison_package, report_date)

  packages <- c(
    candidate_packages,
    list(comparison_package, daily_update_package, homepage_brief_package)
  )
  packages[!vapply(packages, is.null, logical(1))]
}

write_editorial_packages <- function(editorial_packages, project_dir = ".", report_date = Sys.Date()) {
  if (length(editorial_packages) == 0) {
    return(character())
  }

  output_dir <- file.path(project_dir, "data", "staging", "editorial", as.character(report_date))

  paths <- purrr::map_chr(editorial_packages, function(artifact) {
    path <- file.path(output_dir, paste0(artifact$artifact_id, ".json"))
    write_contract_json(artifact, path)
    path
  })

  unname(paths)
}

load_editorial_packages <- function(project_dir = ".") {
  editorial_dir <- file.path(project_dir, "data", "staging", "editorial")
  if (!dir.exists(editorial_dir)) {
    return(list())
  }

  files <- list.files(editorial_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
}

editorial_package_index_tibble <- function(editorial_packages) {
  if (length(editorial_packages) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(editorial_packages, function(artifact) {
    tibble::tibble(
      artifact_id = artifact$artifact_id %||% NA_character_,
      artifact_type = artifact$artifact_type %||% NA_character_,
      title = artifact$title %||% NA_character_,
      candidate_count = length(artifact$candidate_ids %||% character()),
      section_count = length(artifact$sections %||% list())
    )
  })
}
