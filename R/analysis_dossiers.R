extract_source_tokens <- function(text) {
  if (length(text) == 0 || all(is.na(text))) {
    return(character())
  }

  matches <- gregexpr("\\{src:([^}]+)\\}", text, perl = TRUE)
  tokens <- regmatches(text, matches)[[1]]
  if (length(tokens) == 0 || identical(tokens, character(0)) || tokens[[1]] == "-1") {
    return(character())
  }

  unique(gsub("^\\{src:|\\}$", "", tokens))
}

parse_analysis_sections <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  heading_indexes <- grep("^##[[:space:]]+", lines)
  if (length(heading_indexes) == 0) {
    return(list())
  }

  purrr::map(seq_along(heading_indexes), function(index) {
    start <- heading_indexes[[index]]
    end <- if (index < length(heading_indexes)) heading_indexes[[index + 1]] - 1 else length(lines)
    heading_line <- lines[[start]]
    heading <- stringr::str_squish(gsub("^##[[:space:]]+", "", heading_line))
    section_id <- stringr::str_match(heading, "\\{#([^}]+)\\}")[, 2]
    heading <- stringr::str_squish(gsub("[[:space:]]*\\{#[^}]+\\}[[:space:]]*$", "", heading))
    body_lines <- lines[(start + 1):end]
    body <- stringr::str_trim(paste(body_lines, collapse = "\n"))

    list(
      section_id = section_id %||% gsub("[^a-z0-9]+", "-", tolower(iconv(heading, to = "ASCII//TRANSLIT"))),
      heading = heading,
      body = body,
      source_ids = extract_source_tokens(body)
    )
  })
}

parse_candidate_policy_dossier <- function(path) {
  candidate_id <- tools::file_path_sans_ext(basename(path))
  sections <- parse_analysis_sections(path)

  list(
    candidate_id = candidate_id,
    path = path,
    source_ids = unique(unlist(purrr::map(sections, "source_ids"), use.names = FALSE)),
    sections = sections
  )
}

load_candidate_policy_dossiers <- function(project_dir = ".") {
  dossier_dir <- file.path(project_dir, "data", "analysis", "candidate_policy_dossiers")
  if (!dir.exists(dossier_dir)) {
    return(list())
  }

  files <- list.files(dossier_dir, pattern = "[.]md$", full.names = TRUE)
  dossiers <- lapply(files, parse_candidate_policy_dossier)
  stats::setNames(dossiers, purrr::map_chr(dossiers, "candidate_id"))
}

parse_comparison_essay <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  title_line <- lines[grep("^#[[:space:]]+", lines)[1]] %||% paste0("# ", tools::file_path_sans_ext(basename(path)))
  title <- stringr::str_squish(gsub("^#[[:space:]]+", "", title_line))
  body_lines <- lines[!grepl("^#[[:space:]]+", lines)]
  body <- stringr::str_trim(paste(body_lines, collapse = "\n"))
  topic_id <- tools::file_path_sans_ext(basename(path))

  list(
    topic_id = topic_id,
    title = title,
    path = path,
    body = body,
    source_ids = extract_source_tokens(body)
  )
}

load_comparison_essays <- function(project_dir = ".") {
  essay_dir <- file.path(project_dir, "data", "analysis", "comparison_essays")
  if (!dir.exists(essay_dir)) {
    return(list())
  }

  files <- list.files(essay_dir, pattern = "[.]md$", full.names = TRUE)
  essays <- lapply(files, parse_comparison_essay)
  stats::setNames(essays, purrr::map_chr(essays, "topic_id"))
}

candidate_policy_dossiers_tibble <- function(dossiers) {
  if (length(dossiers) == 0) {
    return(tibble::tibble(
      candidate_id = character(),
      section_count = integer(),
      source_ids = character(),
      path = character()
    ))
  }

  purrr::map_dfr(dossiers, function(dossier) {
    tibble::tibble(
      candidate_id = dossier$candidate_id %||% NA_character_,
      section_count = length(dossier$sections %||% list()),
      source_ids = paste(dossier$source_ids %||% character(), collapse = "|"),
      path = dossier$path %||% NA_character_
    )
  })
}

comparison_essays_tibble <- function(essays) {
  if (length(essays) == 0) {
    return(tibble::tibble(
      topic_id = character(),
      title = character(),
      source_ids = character(),
      path = character()
    ))
  }

  purrr::map_dfr(essays, function(essay) {
    tibble::tibble(
      topic_id = essay$topic_id %||% NA_character_,
      title = essay$title %||% NA_character_,
      source_ids = paste(essay$source_ids %||% character(), collapse = "|"),
      path = essay$path %||% NA_character_
    )
  })
}
