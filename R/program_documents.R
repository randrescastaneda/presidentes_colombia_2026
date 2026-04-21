empty_program_documents_tibble <- function() {
  tibble::tibble(
    document_id = character(),
    source_id = character(),
    candidate_id = character(),
    document_role = character(),
    is_primary = logical(),
    official_page_url = character(),
    download_url = character(),
    source_name = character(),
    title = character(),
    published_at = as.POSIXct(character(), tz = "UTC"),
    discovery_method = character(),
    download_status = character(),
    conversion_status = character(),
    pdf_path = character(),
    markdown_path = character(),
    notes = character()
  )
}

program_document_registry_path <- function(project_dir = ".") {
  file.path(project_dir, "data", "program_documents", "program_documents.csv")
}

ensure_program_document_registry <- function(project_dir = ".") {
  registry_path <- program_document_registry_path(project_dir)
  dir.create(dirname(registry_path), recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(registry_path)) {
    readr::write_csv(empty_program_documents_tibble(), registry_path)
  }

  invisible(registry_path)
}

resolve_program_document_path <- function(project_dir = ".", value) {
  if (length(value) == 0 || all(is.na(value)) || identical(value[[1]], "")) {
    return(NA_character_)
  }

  path <- as.character(value[[1]])
  if (grepl("^(https?|data):", path, perl = TRUE)) {
    return(path)
  }

  if (grepl("^/", path, perl = TRUE)) {
    return(path)
  }

  normalizePath(file.path(project_dir, path), winslash = "/", mustWork = FALSE)
}

relative_program_document_path <- function(project_dir = ".", value) {
  if (length(value) == 0 || all(is.na(value)) || identical(value[[1]], "")) {
    return(NA_character_)
  }

  path <- as.character(value[[1]])
  if (grepl("^(https?|data):", path, perl = TRUE)) {
    return(path)
  }

  if (!grepl("^/", path, perl = TRUE)) {
    return(path)
  }

  project_root <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (startsWith(normalized, paste0(project_root, "/"))) {
    return(sub(paste0("^", stringr::fixed(project_root), "/?"), "", normalized))
  }

  normalized
}

load_program_documents <- function(project_dir = ".") {
  registry_path <- ensure_program_document_registry(project_dir)

  documents <- readr::read_csv(registry_path, show_col_types = FALSE)
  if (nrow(documents) == 0) {
    return(empty_program_documents_tibble())
  }

  documents |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(
          c(
            "document_id",
            "source_id",
            "candidate_id",
            "document_role",
            "official_page_url",
            "download_url",
            "source_name",
            "title",
            "discovery_method",
            "download_status",
            "conversion_status",
            "pdf_path",
            "markdown_path",
            "notes"
          )
        ),
        as.character
      ),
      dplyr::across(dplyr::any_of("is_primary"), as.logical),
      published_at = as.POSIXct(.data$published_at, tz = "UTC"),
      pdf_path = vapply(.data$pdf_path, relative_program_document_path, character(1), project_dir = project_dir),
      markdown_path = vapply(.data$markdown_path, relative_program_document_path, character(1), project_dir = project_dir)
    ) |>
    dplyr::distinct(.data$document_id, .keep_all = TRUE)
}

program_documents_initialized <- function(program_documents) {
  nrow(program_documents) > 0
}

build_program_document_sources <- function(program_documents) {
  if (nrow(program_documents) == 0) {
    return(tibble::tibble(
      source_id = character(),
      candidate_id = character(),
      published_at = as.POSIXct(character(), tz = "UTC"),
      source_tier = character(),
      source_type = character(),
      source_name = character(),
      url = character(),
      title = character(),
      quote_text = character(),
      confidence = numeric(),
      inbox_batch = character(),
      document_id = character(),
      document_role = character(),
      is_primary = logical(),
      official_page_url = character(),
      download_url = character(),
      discovery_method = character(),
      download_status = character(),
      conversion_status = character(),
      pdf_path = character(),
      markdown_path = character(),
      notes = character()
    ))
  }

  program_documents |>
    dplyr::filter(!is.na(.data$source_id), .data$source_id != "", !is.na(.data$candidate_id), .data$candidate_id != "") |>
    dplyr::transmute(
      source_id = .data$source_id,
      candidate_id = .data$candidate_id,
      published_at = .data$published_at,
      source_tier = "official",
      source_type = "program",
      source_name = dplyr::coalesce(.data$source_name, "Documento oficial"),
      url = dplyr::coalesce(dplyr::na_if(.data$download_url, ""), dplyr::na_if(.data$official_page_url, "")),
      title = dplyr::coalesce(dplyr::na_if(.data$title, ""), .data$document_id),
      quote_text = dplyr::coalesce(dplyr::na_if(.data$notes, ""), dplyr::na_if(.data$title, "")),
      confidence = 0.95,
      inbox_batch = as.character(as.Date(dplyr::coalesce(.data$published_at, as.POSIXct(Sys.Date(), tz = "UTC")))),
      document_id = .data$document_id,
      document_role = .data$document_role,
      is_primary = .data$is_primary,
      official_page_url = .data$official_page_url,
      download_url = .data$download_url,
      discovery_method = .data$discovery_method,
      download_status = .data$download_status,
      conversion_status = .data$conversion_status,
      pdf_path = .data$pdf_path,
      markdown_path = .data$markdown_path,
      notes = .data$notes
    ) |>
    dplyr::distinct(.data$source_id, .keep_all = TRUE)
}

list_program_document_text_files <- function(project_dir = ".", program_documents = load_program_documents(project_dir)) {
  if (nrow(program_documents) == 0) {
    return(tibble::tibble(
      batch_date = as.Date(character()),
      source_id = character(),
      path = character()
    ))
  }

  markdown_rows <- program_documents |>
    dplyr::filter(!is.na(.data$markdown_path), .data$markdown_path != "")

  if (nrow(markdown_rows) == 0) {
    return(tibble::tibble(
      batch_date = as.Date(character()),
      source_id = character(),
      path = character()
    ))
  }

  markdown_rows |>
    dplyr::transmute(
      batch_date = as.Date(dplyr::coalesce(.data$published_at, as.POSIXct(Sys.Date(), tz = "UTC"))),
      source_id = .data$source_id,
      path = vapply(.data$markdown_path, resolve_program_document_path, character(1), project_dir = project_dir)
    ) |>
    dplyr::filter(file.exists(.data$path)) |>
    dplyr::distinct(.data$source_id, .keep_all = TRUE)
}

build_program_documents_public <- function(
  program_documents,
  public_sources = tibble::tibble(),
  candidate_analysis = list(),
  comparison_report = NULL
) {
  if (nrow(program_documents) == 0) {
    return(tibble::tibble())
  }

  analysis_source_ids <- unique(unlist(purrr::map(candidate_analysis, \(artifact) artifact$source_ids %||% character())))
  comparison_source_ids <- unique(comparison_report$source_ids %||% character())
  public_source_ids <- unique(public_sources$source_id %||% character())

  program_documents |>
    dplyr::mutate(
      public_url = dplyr::coalesce(dplyr::na_if(.data$download_url, ""), dplyr::na_if(.data$official_page_url, "")),
      has_pdf = !is.na(.data$pdf_path) & .data$pdf_path != "",
      has_markdown = !is.na(.data$markdown_path) & .data$markdown_path != "",
      included_in_public_sources = .data$source_id %in% public_source_ids,
      used_in_candidate_analysis = .data$source_id %in% analysis_source_ids,
      used_in_comparison = .data$source_id %in% comparison_source_ids,
      coverage_status = dplyr::case_when(
        .data$used_in_comparison ~ "comparison_ready",
        .data$used_in_candidate_analysis ~ "analysis_ready",
        .data$included_in_public_sources ~ "ingested",
        .data$has_markdown ~ "markdown_ready",
        TRUE ~ "registered_only"
      ),
      coverage_label = dplyr::case_when(
        .data$coverage_status == "comparison_ready" ~ "Comparación activa",
        .data$coverage_status == "analysis_ready" ~ "Análisis por candidato activo",
        .data$coverage_status == "ingested" ~ "Integrado al pipeline",
        .data$coverage_status == "markdown_ready" ~ "Markdown listo",
        TRUE ~ "Solo registrado"
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$is_primary), .data$candidate_id, dplyr::desc(.data$published_at))
}

publish_program_document_files <- function(project_dir = ".", program_documents = load_program_documents(project_dir)) {
  if (nrow(program_documents) == 0) {
    return(invisible(character()))
  }

  file_rows <- program_documents |>
    dplyr::transmute(
      pdf_path = .data$pdf_path,
      markdown_path = .data$markdown_path
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "path_type",
      values_to = "relative_path"
    ) |>
    dplyr::filter(!is.na(.data$relative_path), .data$relative_path != "") |>
    dplyr::distinct(.data$relative_path)

  if (nrow(file_rows) == 0) {
    return(invisible(character()))
  }

  published <- character()
  destination_roots <- c(
    file.path(project_dir, "docs"),
    file.path(project_dir, "docs", "candidatos")
  )

  for (relative_path in file_rows$relative_path) {
    source_path <- file.path(project_dir, relative_path)
    if (!file.exists(source_path)) {
      next
    }

    for (destination_root in destination_roots) {
      destination_path <- file.path(destination_root, relative_path)
      dir.create(dirname(destination_path), recursive = TRUE, showWarnings = FALSE)
      copied <- file.copy(source_path, destination_path, overwrite = TRUE)
      if (isTRUE(copied)) {
        published <- c(published, destination_path)
      }
    }
  }

  invisible(unique(published))
}
