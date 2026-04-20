load_extraction_results <- function(project_dir = ".") {
  extraction_dir <- file.path(project_dir, "data", "staging", "extraction")
  if (!dir.exists(extraction_dir)) {
    return(list())
  }

  files <- list.files(extraction_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
}

flatten_extraction_claims <- function(extraction_results) {
  if (length(extraction_results) == 0) {
    return(tibble::tibble())
  }

  purrr::imap_dfr(extraction_results, function(result, name) {
    claims <- result$claims
    if (is.null(claims) || length(claims) == 0) {
      return(tibble::tibble())
    }

    tibble::as_tibble(claims) |>
      dplyr::mutate(
        source_id = result$source_id %||% tools::file_path_sans_ext(name),
        batch_date = as.Date(result$batch_date %||% NA_character_)
      )
  })
}
