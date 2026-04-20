list_inbox_batches_v2 <- function(project_dir = ".") {
  inbox_dir <- file.path(project_dir, "data", "inbox")
  if (!dir.exists(inbox_dir)) {
    return(character())
  }

  list.dirs(inbox_dir, recursive = FALSE, full.names = TRUE) |>
    sort()
}

list_source_text_files <- function(project_dir = ".", batch_date = NULL) {
  batch_dirs <- list_inbox_batches_v2(project_dir)

  if (!is.null(batch_date)) {
    batch_dirs <- batch_dirs[basename(batch_dirs) %in% batch_date]
  }

  if (length(batch_dirs) == 0) {
    return(tibble::tibble(
      batch_date = as.Date(character()),
      source_id = character(),
      path = character()
    ))
  }

  purrr::map_dfr(batch_dirs, function(batch_dir) {
    text_dir <- file.path(batch_dir, "source_texts")
    if (!dir.exists(text_dir)) {
      return(tibble::tibble())
    }

    files <- list.files(text_dir, full.names = TRUE)
    if (length(files) == 0) {
      return(tibble::tibble())
    }

    tibble::tibble(
      batch_date = as.Date(basename(batch_dir)),
      source_id = tools::file_path_sans_ext(basename(files)),
      path = files
    )
  })
}
