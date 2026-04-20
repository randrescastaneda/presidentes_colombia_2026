load_comparison_reports <- function(project_dir = ".") {
  comparison_dir <- file.path(project_dir, "data", "staging", "comparison")
  if (!dir.exists(comparison_dir)) {
    return(list())
  }

  files <- list.files(comparison_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
}
