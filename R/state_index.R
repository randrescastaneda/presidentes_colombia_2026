empty_source_registry <- function() {
  tibble::tibble(
    source_id = character(),
    source_url = character(),
    content_hash = character(),
    batch_date = as.Date(character()),
    processed_at = as.POSIXct(character(), tz = "UTC")
  )
}

empty_candidate_state <- function() {
  tibble::tibble(
    candidate_id = character(),
    last_evidence_hash = character(),
    last_analysis_at = as.POSIXct(character(), tz = "UTC"),
    last_batch_date = as.Date(character()),
    dirty = logical()
  )
}

ensure_state_tables <- function(project_dir = ".") {
  source_registry_path <- file.path(project_dir, "data", "state", "source_registry.csv")
  candidate_state_path <- file.path(project_dir, "data", "state", "candidate_state.csv")

  dir.create(dirname(source_registry_path), recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(source_registry_path)) {
    readr::write_csv(empty_source_registry(), source_registry_path)
  }

  if (!file.exists(candidate_state_path)) {
    readr::write_csv(empty_candidate_state(), candidate_state_path)
  }

  invisible(c(source_registry_path, candidate_state_path))
}

load_source_registry <- function(project_dir = ".") {
  ensure_state_tables(project_dir)
  readr::read_csv(file.path(project_dir, "data", "state", "source_registry.csv"), show_col_types = FALSE) |>
    dplyr::mutate(
      batch_date = as.Date(.data$batch_date),
      processed_at = as.POSIXct(.data$processed_at, tz = "UTC")
    )
}

load_candidate_state <- function(project_dir = ".") {
  ensure_state_tables(project_dir)
  readr::read_csv(file.path(project_dir, "data", "state", "candidate_state.csv"), show_col_types = FALSE) |>
    dplyr::mutate(
      last_batch_date = as.Date(.data$last_batch_date),
      last_analysis_at = as.POSIXct(.data$last_analysis_at, tz = "UTC"),
      dirty = as.logical(.data$dirty)
    )
}
