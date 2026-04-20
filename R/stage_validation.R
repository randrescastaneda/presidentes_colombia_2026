load_validation_reports <- function(project_dir = ".") {
  validation_dir <- file.path(project_dir, "data", "staging", "validation")
  if (!dir.exists(validation_dir)) {
    return(list())
  }

  files <- list.files(validation_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
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
