load_validation_reports <- function(project_dir = ".") {
  validation_dir <- file.path(project_dir, "data", "staging", "validation")
  if (!dir.exists(validation_dir)) {
    return(list())
  }

  files <- list.files(validation_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
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
