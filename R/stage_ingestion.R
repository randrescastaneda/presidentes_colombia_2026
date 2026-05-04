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

read_source_text_content <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

build_source_packets <- function(sources, source_text_files = tibble::tibble()) {
  if (nrow(sources) == 0) {
    return(list())
  }

  source_text_lookup <- if (nrow(source_text_files) == 0) {
    tibble::tibble(source_id = character(), text_path = character(), batch_date = as.Date(character()))
  } else {
    source_text_files |>
      dplyr::transmute(
        source_id = .data$source_id,
        text_path = .data$path,
        batch_date = .data$batch_date
      )
  }

  enriched_sources <- sources |>
    dplyr::left_join(source_text_lookup, by = "source_id")

  packets <- purrr::pmap(enriched_sources, function(...) {
    row <- tibble::as_tibble(list(...))
    row_value <- function(name, default = NA_character_) {
      if (!name %in% names(row)) {
        return(default)
      }

      value <- row[[name]][[1]]
      if (length(value) == 0 || all(is.na(value))) {
        return(default)
      }

      value
    }

    raw_text <- if (!is.na(row$text_path[[1]] %||% NA_character_)) read_source_text_content(row$text_path[[1]]) else row$quote_text[[1]] %||% ""
    note_metadata <- parse_source_note_metadata(raw_text)
    hinted_candidates <- unique(stats::na.omit(c(
      row$candidate_id[[1]],
      note_metadata$candidate_hint %||% NA_character_
    )))

    list(
      source_id = row$source_id[[1]],
      batch_date = row$inbox_batch[[1]] %||% as.character(row$batch_date[[1]] %||% Sys.Date()),
      source_name = row$source_name[[1]],
      source_type = row$source_type[[1]],
      source_tier = row$source_tier[[1]],
      source_url = row$url[[1]],
      published_at = format(as.POSIXct(row$published_at[[1]], tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      title = row$title[[1]] %||% "Sin título visible",
      document_id = row_value("document_id"),
      document_role = row_value("document_role"),
      is_primary_document = as.logical(row_value("is_primary", FALSE)),
      official_page_url = row_value("official_page_url"),
      download_url = row_value("download_url"),
      discovery_method = row_value("discovery_method"),
      download_status = row_value("download_status"),
      conversion_status = row_value("conversion_status"),
      pdf_path = row_value("pdf_path"),
      markdown_path = row_value("markdown_path"),
      candidate_hints = hinted_candidates,
      capture_method = note_metadata$capture_method %||% if (!is.na(row$text_path[[1]] %||% NA_character_)) "source_text_file" else "quote_only",
      text_content = raw_text,
      captured_excerpt = row$quote_text[[1]] %||% "",
      notes = note_metadata$notes %||% NA_character_
    )
  })

  stats::setNames(packets, vapply(packets, `[[`, character(1), "source_id"))
}

write_source_packets <- function(source_packets, project_dir = ".") {
  if (length(source_packets) == 0) {
    return(character())
  }

  output_dir <- file.path(project_dir, "data", "staging", "source_packets")

  paths <- purrr::imap_chr(source_packets, function(packet, source_id) {
    batch_date <- packet$batch_date %||% "undated"
    path <- file.path(output_dir, batch_date, paste0(source_id, ".json"))
    write_contract_json(packet, path)
    path
  })

  unname(paths)
}

load_source_packets <- function(project_dir = ".") {
  packet_dir <- file.path(project_dir, "data", "staging", "source_packets")
  if (!dir.exists(packet_dir)) {
    return(list())
  }

  files <- list.files(packet_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
}
