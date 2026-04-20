load_validation_reports <- function(project_dir = ".") {
  validation_dir <- file.path(project_dir, "data", "staging", "validation")
  if (!dir.exists(validation_dir)) {
    return(list())
  }

  files <- list.files(validation_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
}

load_validation_rules <- function(project_dir = ".") {
  path <- file.path(project_dir, "config", "validation_rules.yml")
  if (!file.exists(path) || !requireNamespace("yaml", quietly = TRUE)) {
    return(list(default_behavior = list(), rules = list()))
  }

  yaml::read_yaml(path)
}

artifact_has_traceability <- function(source_ids, claim_ids) {
  length(source_ids %||% character()) > 0 && length(claim_ids %||% character()) > 0
}

has_description_inference_evaluation_split <- function(candidate_analysis, editorial_packages) {
  candidate_ok <- purrr::every(candidate_analysis, function(artifact) {
    thematic_rows <- normalize_nested_objects(artifact$thematic_analysis)
    all(vapply(thematic_rows, function(row) {
      all(
        !is.na(row$description %||% NA_character_) && row$description %||% "" != "",
        !is.na(row$inference %||% NA_character_) && row$inference %||% "" != "",
        !is.na(row$evaluation %||% NA_character_) && row$evaluation %||% "" != ""
      )
    }, logical(1)))
  })

  editorial_ok <- purrr::every(editorial_packages, function(artifact) {
    section_types <- unique(purrr::map_chr(artifact$sections %||% list(), "content_type"))
    if (artifact$artifact_type %in% c("candidate_profile", "comparison_report")) {
      all(c("description", "inference", "evaluation") %in% section_types)
    } else {
      length(section_types) >= 1
    }
  })

  candidate_ok && editorial_ok
}

has_symmetric_comparison <- function(comparison_report) {
  if (is.null(comparison_report)) {
    return(TRUE)
  }

  candidate_ids <- comparison_report$candidate_ids %||% character()

  axes_ok <- purrr::every(comparison_report$axes_comparison %||% list(), function(axis_row) {
    compared_ids <- sort(purrr::map_chr(axis_row$candidate_positions %||% list(), "candidate_id"))
    identical(compared_ids, sort(candidate_ids))
  })

  topics_ok <- purrr::every(comparison_report$topic_comparison %||% list(), function(topic_row) {
    compared_ids <- sort(purrr::map_chr(topic_row$candidate_rows %||% list(), "candidate_id"))
    identical(compared_ids, sort(candidate_ids))
  })

  axes_ok && topics_ok
}

artifact_contains_partisan_language <- function(text) {
  if (length(text) == 0 || all(is.na(text))) {
    return(FALSE)
  }

  grepl(
    "salvador|heroic|brillante|vergonzos|nefasta|traidor|castrochavista|redentor|mesias",
    normalize_analysis_text(text),
    perl = TRUE
  )
}

collect_artifact_text <- function(candidate_analysis, comparison_report, editorial_packages) {
  texts <- c(
    unlist(purrr::map(candidate_analysis, function(artifact) {
      c(
        artifact$profile_overview,
        artifact$political_philosophy,
        artifact$internal_coherence,
        artifact$mainstream_distance,
        artifact$strengths %||% character(),
        artifact$weaknesses %||% character(),
        artifact$uncertainties %||% character(),
        unlist(purrr::map(normalize_nested_objects(artifact$thematic_analysis), function(theme) {
          c(theme$description, theme$inference, theme$evaluation, theme$tradeoffs %||% character(), theme$uncertainties %||% character())
        }))
      )
    })),
    if (!is.null(comparison_report)) {
      c(
        comparison_report$convergences %||% character(),
        comparison_report$divergences %||% character(),
        comparison_report$uncertainties %||% character(),
        unlist(purrr::map(comparison_report$axes_comparison %||% list(), "summary")),
        unlist(purrr::map(comparison_report$topic_comparison %||% list(), "summary"))
      )
    } else {
      character()
    },
    unlist(purrr::map(editorial_packages, function(artifact) {
      c(artifact$title, artifact$dek, unlist(purrr::map(artifact$sections %||% list(), "body")))
    }))
  )

  stats::na.omit(texts)
}

has_explicit_uncertainty <- function(candidate_analysis, comparison_report) {
  candidate_ok <- purrr::every(candidate_analysis, function(artifact) {
    length(artifact$uncertainties %||% character()) > 0 ||
      length(purrr::keep(artifact$ideology_axes %||% list(), \(axis) identical(axis$placement %||% "", "Evidencia insuficiente"))) == 0
  })

  comparison_ok <- is.null(comparison_report) || length(comparison_report$uncertainties %||% character()) > 0

  candidate_ok && comparison_ok
}

has_feasibility_coverage <- function(candidate_analysis, comparison_report) {
  candidate_ok <- purrr::every(candidate_analysis, function(artifact) {
    thematic_rows <- normalize_nested_objects(artifact$thematic_analysis)
    all(vapply(thematic_rows, function(row) {
      feasibility <- row$feasibility
      all(vapply(
        c("political", "fiscal", "institutional", "administrative"),
        \(field) !is.na(feasibility[[field]] %||% NA_character_) && feasibility[[field]] %||% "" != "",
        logical(1)
      ))
    }, logical(1)))
  })

  comparison_ok <- is.null(comparison_report) || purrr::every(
    comparison_report$topic_comparison %||% list(),
    \(topic_row) all(vapply(topic_row$candidate_rows %||% list(), \(row) !is.na(row$feasibility %||% NA_character_) && row$feasibility %||% "" != "", logical(1)))
  )

  candidate_ok && comparison_ok
}

vague_promises_are_flagged <- function(claims, candidate_analysis) {
  vague_candidates <- claims |>
    dplyr::filter(claim_type_id %in% c("promesa_vaga", "slogan")) |>
    dplyr::distinct(candidate_id) |>
    dplyr::pull(candidate_id)

  if (length(vague_candidates) == 0) {
    return(TRUE)
  }

  purrr::every(vague_candidates, function(candidate_id) {
    artifact <- candidate_analysis[[candidate_id]]
    if (is.null(artifact)) {
      return(FALSE)
    }

    text <- paste(c(artifact$weaknesses, artifact$uncertainties), collapse = " ")
    grepl("especificidad|vaga|vago|mecanismo", normalize_analysis_text(text), perl = TRUE)
  })
}

build_validation_check <- function(rule_id, level, status, artifact_ref, message) {
  list(
    rule_id = rule_id,
    level = level,
    status = status,
    artifact_ref = artifact_ref,
    message = message
  )
}

validation_status_from_checks <- function(checks) {
  has_block_fail <- any(vapply(checks, \(check) check$level == "block" && check$status == "fail", logical(1)))
  has_warning <- any(vapply(checks, \(check) check$status == "warn", logical(1)))

  if (has_block_fail) {
    return("block")
  }
  if (has_warning) {
    return("pass_with_warnings")
  }
  "pass"
}

build_validation_report <- function(
  claims,
  candidate_analysis,
  comparison_report,
  editorial_packages,
  report_date = Sys.Date(),
  project_dir = "."
) {
  rules <- load_validation_rules(project_dir)

  candidate_traceable <- purrr::every(candidate_analysis, \(artifact) artifact_has_traceability(artifact$source_ids, artifact$claim_ids))
  comparison_traceable <- is.null(comparison_report) || artifact_has_traceability(comparison_report$source_ids, comparison_report$claim_ids)
  editorial_traceable <- purrr::every(editorial_packages, \(artifact) artifact_has_traceability(artifact$source_ids, artifact$claim_ids))

  checks <- list(
    build_validation_check(
      "traceability_required",
      "block",
      if (candidate_traceable && comparison_traceable && editorial_traceable) "pass" else "fail",
      "candidate_analysis|comparison_report|editorial_packages",
      if (candidate_traceable && comparison_traceable && editorial_traceable) {
        "Todos los artefactos analíticos conservan source_ids y claim_ids trazables."
      } else {
        "Al menos un artefacto analítico carece de trazabilidad suficiente."
      }
    ),
    build_validation_check(
      "description_inference_evaluation_split",
      "block",
      if (has_description_inference_evaluation_split(candidate_analysis, editorial_packages)) "pass" else "fail",
      "candidate_analysis|editorial_packages",
      if (has_description_inference_evaluation_split(candidate_analysis, editorial_packages)) {
        "Los artefactos distinguen descripción, inferencia y evaluación."
      } else {
        "La separación entre descripción, inferencia y evaluación no es consistente."
      }
    ),
    build_validation_check(
      "symmetric_comparison",
      "block",
      if (has_symmetric_comparison(comparison_report)) "pass" else "fail",
      "comparison_report",
      if (has_symmetric_comparison(comparison_report)) {
        "La comparación usa la misma estructura para todos los candidatos incluidos."
      } else {
        "La comparación no mantiene filas y ejes simétricos para todos los candidatos."
      }
    ),
    build_validation_check(
      "explicit_uncertainty",
      "warn",
      if (has_explicit_uncertainty(candidate_analysis, comparison_report)) "pass" else "warn",
      "candidate_analysis|comparison_report",
      if (has_explicit_uncertainty(candidate_analysis, comparison_report)) {
        "Las incertidumbres relevantes están explicitadas."
      } else {
        "Persisten artefactos analíticos sin incertidumbre explícita donde la evidencia lo exigiría."
      }
    ),
    build_validation_check(
      "no_partisan_language",
      "block",
      if (any(vapply(collect_artifact_text(candidate_analysis, comparison_report, editorial_packages), artifact_contains_partisan_language, logical(1)))) "fail" else "pass",
      "candidate_analysis|comparison_report|editorial_packages",
      if (any(vapply(collect_artifact_text(candidate_analysis, comparison_report, editorial_packages), artifact_contains_partisan_language, logical(1)))) {
        "Se detectó lenguaje cargado o partidista en artefactos publicables."
      } else {
        "No se detectó lenguaje partidista evidente en los artefactos publicables."
      }
    ),
    build_validation_check(
      "feasibility_required_for_programmatic_analysis",
      "warn",
      if (has_feasibility_coverage(candidate_analysis, comparison_report)) "pass" else "warn",
      "candidate_analysis|comparison_report",
      if (has_feasibility_coverage(candidate_analysis, comparison_report)) {
        "La factibilidad está cubierta en las salidas programáticas."
      } else {
        "Falta cobertura suficiente de factibilidad en uno o más artefactos programáticos."
      }
    ),
    build_validation_check(
      "vague_promises_not_overstated",
      "warn",
      if (vague_promises_are_flagged(claims, candidate_analysis)) "pass" else "warn",
      "candidate_analysis",
      if (vague_promises_are_flagged(claims, candidate_analysis)) {
        "Las promesas vagas quedaron marcadas como limitación analítica."
      } else {
        "Hay promesas vagas sin una advertencia analítica suficientemente explícita."
      }
    )
  )

  status <- validation_status_from_checks(checks)
  summary <- dplyr::case_when(
    status == "block" ~ "Validation blocked publication: at least one mandatory methodological rule failed.",
    status == "pass_with_warnings" ~ "Validation passed with warnings: publication is possible, but analytical caveats remain visible.",
    TRUE ~ "Validation passed with no blocking issues."
  )

  list(
    report_id = paste0("validation-", as.character(report_date)),
    artifact_ids = c("claim_records", "candidate_analysis", "comparison_report", "editorial_packages", "site_metadata"),
    status = status,
    checks = checks,
    summary = summary
  )
}

public_publish_allowed <- function(validation_report) {
  !identical(validation_report$status %||% "block", "block")
}

build_legacy_validation_report <- function(claims, analysis_notes, source_text_files = tibble::tibble(), report_date = Sys.Date()) {
  checks <- list(
    list(
      rule_id = "legacy_claims_have_source_id",
      level = "block",
      status = if (all(!is.na(claims$source_id %||% character()) & claims$source_id %||% character() != "")) "pass" else "fail",
      artifact_ref = "claim_records",
      message = if (nrow(claims) == 0) {
        "No public claims were produced in this run."
      } else if (all(!is.na(claims$source_id) & claims$source_id != "")) {
        "Every public claim points to a traceable source_id."
      } else {
        "At least one public claim is missing a source_id."
      }
    ),
    list(
      rule_id = "legacy_analysis_has_support_links",
      level = "block",
      status = if (nrow(analysis_notes) == 0 || all(!is.na(analysis_notes$source_ids) & analysis_notes$source_ids != "")) "pass" else "fail",
      artifact_ref = "analysis_notes",
      message = if (nrow(analysis_notes) == 0) {
        "No analysis notes were published in this run."
      } else if (all(!is.na(analysis_notes$source_ids) & analysis_notes$source_ids != "")) {
        "Every analysis note preserves source linkage."
      } else {
        "At least one analysis note is missing support links."
      }
    ),
    list(
      rule_id = "structured_extraction_pending",
      level = "warn",
      status = "warn",
      artifact_ref = "pipeline_mode",
      message = if (nrow(source_text_files) > 0) {
        "Source texts are present, but the public pipeline still depends on manual claims.csv during this transition."
      } else {
        "The contract layer exists, but structured extraction is not yet wired into the public pipeline."
      }
    )
  )

  check_statuses <- vapply(checks, `[[`, character(1), "status")
  has_fail <- any(check_statuses == "fail")
  has_warn <- any(check_statuses == "warn")

  status <- if (has_fail) {
    "block"
  } else if (has_warn) {
    "pass_with_warnings"
  } else {
    "pass"
  }

  summary <- dplyr::case_when(
    status == "block" ~ "Validation failed: the legacy pipeline produced artifacts with broken traceability.",
    status == "pass_with_warnings" ~ "Validation passed with warnings: legacy compatibility remains active while structured extraction is pending.",
    TRUE ~ "Validation passed with no warnings."
  )

  list(
    report_id = paste0("validation-", as.character(report_date)),
    artifact_ids = c("claim_records", "analysis_notes", "site_metadata"),
    status = status,
    checks = checks,
    summary = summary
  )
}

latest_validation_status <- function(project_dir = ".") {
  reports <- load_validation_reports(project_dir)
  if (length(reports) == 0) {
    return(tibble::tibble(
      report_id = character(),
      status = character(),
      summary = character()
    ))
  }

  purrr::map_dfr(reports, function(report) {
    tibble::tibble(
      report_id = report$report_id %||% NA_character_,
      status = report$status %||% NA_character_,
      summary = report$summary %||% NA_character_
    )
  }) |>
    dplyr::slice_tail(n = 1)
}
