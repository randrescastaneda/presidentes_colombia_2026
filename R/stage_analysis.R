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
    return(tibble::tibble())
  }

  purrr::map_dfr(analysis_artifacts, function(artifact) {
    tibble::tibble(
      analysis_id = artifact$analysis_id %||% NA_character_,
      candidate_id = artifact$candidate_id %||% NA_character_,
      source_count = length(artifact$source_ids %||% character()),
      claim_count = length(artifact$claim_ids %||% character()),
      uncertainty_count = length(artifact$uncertainties %||% character())
    )
  })
}
