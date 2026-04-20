load_editorial_packages <- function(project_dir = ".") {
  editorial_dir <- file.path(project_dir, "data", "staging", "editorial")
  if (!dir.exists(editorial_dir)) {
    return(list())
  }

  files <- list.files(editorial_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
}
