normalize_source_note_optional_text <- function(x) {
  value <- stringr::str_squish(as.character(x %||% ""))
  if (identical(value, "") || identical(tolower(value), "na") || identical(tolower(value), "null")) {
    return(NA_character_)
  }

  value
}

parse_markdown_key_values <- function(lines) {
  matches <- regmatches(
    lines,
    regexec("^\\s*-\\s+`?([A-Za-z0-9_]+)`?\\s*:\\s*(.*)$", lines, perl = TRUE)
  )

  values <- purrr::map(matches, function(match) {
    if (length(match) < 3) {
      return(NULL)
    }

    key <- trimws(match[[2]])
    value <- normalize_source_note_optional_text(match[[3]])
    stats::setNames(list(value), key)
  })

  purrr::compact(values) |>
    purrr::flatten()
}

extract_markdown_section_lines <- function(lines, heading_pattern) {
  heading_idx <- grep(heading_pattern, lines, perl = TRUE)
  if (length(heading_idx) == 0) {
    return(character())
  }

  start_idx <- heading_idx[[1]] + 1
  if (start_idx > length(lines)) {
    return(character())
  }

  next_heading <- grep("^##\\s+", lines, perl = TRUE)
  next_heading <- next_heading[next_heading > heading_idx[[1]]]
  end_idx <- if (length(next_heading) == 0) length(lines) else next_heading[[1]] - 1

  if (end_idx < start_idx) {
    return(character())
  }

  lines[start_idx:end_idx]
}

parse_source_note_metadata <- function(text) {
  if (is.na(text) || identical(text, "")) {
    return(list())
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  first_section <- grep("^##\\s+", lines, perl = TRUE)
  metadata_lines <- if (length(first_section) == 0) {
    lines
  } else {
    lines[seq_len(first_section[[1]] - 1)]
  }

  parse_markdown_key_values(metadata_lines)
}

extract_structured_claim_blocks <- function(text) {
  if (is.na(text) || identical(text, "")) {
    return(list())
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  section_lines <- extract_markdown_section_lines(lines, "^##\\s+Structured claims\\s*$")
  if (length(section_lines) == 0) {
    return(list())
  }

  block_markers <- grep("^###\\s+", section_lines, perl = TRUE)
  if (length(block_markers) == 0) {
    parsed <- parse_markdown_key_values(section_lines)
    return(if (length(parsed) == 0) list() else list(parsed))
  }

  purrr::map(seq_along(block_markers), function(i) {
    start_idx <- block_markers[[i]] + 1
    end_idx <- if (i == length(block_markers)) length(section_lines) else block_markers[[i + 1]] - 1
    parse_markdown_key_values(section_lines[start_idx:end_idx])
  }) |>
    purrr::keep(\(block) length(block) > 0)
}

extract_source_text_body <- function(text) {
  if (is.na(text) || identical(text, "")) {
    return("")
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  body_lines <- extract_markdown_section_lines(lines, "^##\\s+Source text or cleaned transcript\\s*$")

  if (length(body_lines) == 0) {
    body_lines <- lines[!grepl("^\\s*-\\s+`?[A-Za-z0-9_]+`?\\s*:", lines, perl = TRUE)]
    body_lines <- body_lines[!grepl("^##\\s+Structured claims\\s*$", body_lines, perl = TRUE)]
    body_lines <- body_lines[!grepl("^###\\s+", body_lines, perl = TRUE)]
  }

  stringr::str_squish(paste(body_lines, collapse = "\n"))
}

normalize_source_note_comparison_text <- function(x) {
  normalize_source_note_optional_text(x)
}

normalize_source_note_claim_type <- function(value, allowed_types = character(), default = NA_character_) {
  normalized <- iconv(as.character(value %||% ""), from = "", to = "ASCII//TRANSLIT")
  normalized <- stringr::str_to_lower(normalized)
  normalized <- stringr::str_squish(normalized)
  if (!nzchar(normalized)) {
    return(default)
  }

  guess <- dplyr::case_when(
    grepl("propuesta", normalized, perl = TRUE) ~ "propuesta_concreta",
    grepl("postura|posicion", normalized, perl = TRUE) ~ "postura_general",
    grepl("diagnost", normalized, perl = TRUE) ~ "diagnostico_problema",
    grepl("slogan|consigna", normalized, perl = TRUE) ~ "slogan",
    grepl("critica|adversar", normalized, perl = TRUE) ~ "critica_adversario",
    grepl("promesa", normalized, perl = TRUE) ~ "promesa_vaga",
    grepl("dato|contextual", normalized, perl = TRUE) ~ "dato_contextual",
    TRUE ~ default
  )

  if (length(allowed_types) > 0 && !is.na(guess) && !guess %in% allowed_types) {
    return(default)
  }

  guess
}
