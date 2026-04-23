contract_path <- function(project_dir = ".", area, filename = NULL) {
  base_dir <- file.path(project_dir, area)
  if (is.null(filename)) {
    return(base_dir)
  }

  file.path(base_dir, filename)
}

read_contract_json <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  jsonlite::read_json(path, simplifyVector = TRUE)
}

write_contract_json <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(data, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  invisible(path)
}

read_json_schema <- function(project_dir = ".", schema_filename) {
  read_contract_json(file.path(project_dir, "schemas", schema_filename))
}

contract_directories <- function(project_dir = ".") {
  c(
    file.path(project_dir, "prompts"),
    file.path(project_dir, "schemas"),
    file.path(project_dir, "examples"),
    file.path(project_dir, "data", "added_manually"),
    file.path(project_dir, "data", "program_documents"),
    file.path(project_dir, "data", "program_documents", "files"),
    file.path(project_dir, "data", "staging", "source_packets"),
    file.path(project_dir, "data", "staging", "extraction"),
    file.path(project_dir, "data", "staging", "analysis"),
    file.path(project_dir, "data", "staging", "comparison"),
    file.path(project_dir, "data", "staging", "editorial"),
    file.path(project_dir, "data", "staging", "validation"),
    file.path(project_dir, "data", "state")
  )
}

ensure_contract_layout <- function(project_dir = ".") {
  purrr::walk(contract_directories(project_dir), dir.create, recursive = TRUE, showWarnings = FALSE)
  ensure_program_document_registry(project_dir)
  invisible(contract_directories(project_dir))
}
