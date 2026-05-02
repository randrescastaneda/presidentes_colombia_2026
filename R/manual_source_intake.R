if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (length(x) == 0 || all(is.na(x)) || identical(x, "")) {
      return(y)
    }

    x
  }
}

manual_source_input_dir <- function(project_dir = ".") {
  file.path(project_dir, "data", "added_manually")
}

manual_source_state_path <- function(project_dir = ".") {
  file.path(project_dir, "data", "state", "manual_source_registry.csv")
}

manual_source_public_status_levels <- c(
  "promoted",
  "pending_classification",
  "discarded_unreachable",
  "discarded_invalid"
)

empty_manual_source_registry <- function() {
  tibble::tibble(
    entry_id = character(),
    raw_url = character(),
    normalized_url = character(),
    final_url = character(),
    source_files = character(),
    discovery_count = integer(),
    context_hint = character(),
    source_name = character(),
    source_tier = character(),
    source_type = character(),
    candidate_id = character(),
    candidate_confidence = numeric(),
    candidate_match_method = character(),
    title = character(),
    published_at = as.POSIXct(character(), tz = "UTC"),
    validation_status = character(),
    public_status = character(),
    status_reason = character(),
    http_status = integer(),
    reachable = logical(),
    discovery_method = character(),
    processed_at = as.POSIXct(character(), tz = "UTC")
  )
}

load_manual_source_registry <- function(project_dir = ".") {
  path <- manual_source_state_path(project_dir)

  if (!file.exists(path)) {
    return(empty_manual_source_registry())
  }

  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      discovery_count = as.integer(.data$discovery_count),
      candidate_confidence = as.numeric(.data$candidate_confidence),
      published_at = as.POSIXct(.data$published_at, tz = "UTC"),
      http_status = as.integer(.data$http_status),
      reachable = as.logical(.data$reachable),
      processed_at = as.POSIXct(.data$processed_at, tz = "UTC")
    )
}

list_manual_source_files <- function(project_dir = ".") {
  input_dir <- manual_source_input_dir(project_dir)

  if (!dir.exists(input_dir)) {
    return(character())
  }

  list.files(input_dir, recursive = TRUE, full.names = TRUE) |>
    (\(paths) paths[file.info(paths)$isdir %in% FALSE])() |>
    (\(paths) paths[!tolower(basename(paths)) %in% c("readme.md", "readme.txt")])() |>
    sort()
}

relative_repo_path <- function(path, project_dir = ".") {
  normalized_project <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  normalized_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(normalized_project, "/")

  if (startsWith(normalized_path, prefix)) {
    return(sub(prefix, "", normalized_path, fixed = TRUE))
  }

  normalized_path
}

clean_manual_url_candidate <- function(url) {
  cleaned <- stringr::str_trim(url %||% "")
  cleaned <- sub("[[:punct:]]+$", "", cleaned)
  cleaned <- sub("[)\\]>]+$", "", cleaned)
  cleaned <- sub("[\"']+$", "", cleaned)

  if (!grepl("^https?://", cleaned, ignore.case = TRUE)) {
    return(NA_character_)
  }

  cleaned
}

extract_urls_from_text <- function(text) {
  if (length(text) == 0 || all(is.na(text))) {
    return(character())
  }

  matches <- stringr::str_extract_all(
    text,
    "https?://[^\\s<>()\\[\\]{}\"']+"
  )[[1]]

  cleaned <- unique(stats::na.omit(vapply(matches, clean_manual_url_candidate, character(1))))
  cleaned[nzchar(cleaned)]
}

normalize_manual_source_url <- function(url) {
  candidate <- clean_manual_url_candidate(url)
  if (is.na(candidate)) {
    return(NA_character_)
  }

  parsed <- suppressWarnings(utils::URLdecode(candidate))
  parsed <- sub("#.*$", "", parsed)

  pieces <- strsplit(parsed, "://", fixed = TRUE)[[1]]
  if (length(pieces) != 2) {
    return(NA_character_)
  }

  scheme <- tolower(pieces[[1]])
  rest <- pieces[[2]]
  host_path <- strsplit(rest, "/", fixed = TRUE)[[1]]
  host <- tolower(host_path[[1]])
  host <- sub(":80$", "", host)
  host <- sub(":443$", "", host)
  host <- sub("^www\\.", "", host)

  path_and_query <- sub("^[^/]*", "", rest)
  if (!nzchar(path_and_query)) {
    path_and_query <- "/"
  }

  path <- sub("\\?.*$", "", path_and_query)
  query <- sub("^[^?]*\\??", "", path_and_query)
  query <- if (identical(query, path_and_query)) "" else query

  path <- gsub("/{2,}", "/", path)
  if (!identical(path, "/")) {
    path <- sub("/$", "", path)
  }

  if (nzchar(query)) {
    query_parts <- strsplit(query, "&", fixed = TRUE)[[1]]
    query_parts <- query_parts[nzchar(query_parts)]
    if (length(query_parts) > 0) {
      keys <- sub("=.*$", "", query_parts)
      keep <- !(tolower(keys) %in% c("utm_source", "utm_medium", "utm_campaign", "utm_term", "utm_content", "fbclid", "gclid", "outputtype"))
      query_parts <- query_parts[keep]
      query_parts <- sort(unique(query_parts))
    }
    query <- paste(query_parts, collapse = "&")
  }

  paste0(
    scheme,
    "://",
    host,
    path,
    if (nzchar(query)) paste0("?", query) else ""
  )
}

manual_source_hash <- function(text) {
  values <- utf8ToInt(enc2utf8(text %||% ""))
  if (length(values) == 0) {
    return("00000000")
  }

  accumulator <- 0
  modulus <- 2147483647
  for (value in values) {
    accumulator <- (accumulator * 131 + value) %% modulus
  }

  sprintf("%08x", accumulator)
}

empty_manual_source_occurrences <- function() {
  tibble::tibble(
    source_file = character(),
    context_hint = character(),
    raw_url = character(),
    normalized_url = character()
  )
}

manual_source_occurrences <- function(project_dir = ".") {
  files <- list_manual_source_files(project_dir)

  if (length(files) == 0) {
    return(empty_manual_source_occurrences())
  }

  purrr::map_dfr(files, function(path) {
    lines <- tryCatch(
      suppressWarnings(readLines(path, warn = FALSE, encoding = "UTF-8")),
      error = function(...) character()
    )
    relative_path <- relative_repo_path(path, project_dir = project_dir)

    if (length(lines) == 0) {
      return(empty_manual_source_occurrences())
    }

    rows <- purrr::map_dfr(lines, function(line) {
      urls <- extract_urls_from_text(line)
      if (length(urls) == 0) {
        return(empty_manual_source_occurrences())
      }

      tibble::tibble(
        source_file = relative_path,
        context_hint = stringr::str_squish(line),
        raw_url = urls,
        normalized_url = vapply(urls, normalize_manual_source_url, character(1))
      )
    })

    rows
  }) |>
    dplyr::filter(!is.na(.data$normalized_url), nzchar(.data$normalized_url))
}

manual_candidate_aliases <- function(candidates) {
  if (!is.data.frame(candidates) || nrow(candidates) == 0) {
    return(list())
  }

  stopwords <- c("de", "del", "la", "las", "los", "y", "san", "santa", "maria", "jose", "oscar")

  alias_map <- purrr::map(seq_len(nrow(candidates)), function(index) {
    row <- candidates[index, , drop = FALSE]
    base_strings <- c(
      row$candidate_id[[1]] %||% "",
      row$slug[[1]] %||% "",
      row$president_name[[1]] %||% ""
    )

    normalized_strings <- base_strings |>
      iconv(to = "ASCII//TRANSLIT") |>
      tolower()

    tokens <- unique(unlist(strsplit(gsub("[^a-z0-9]+", " ", normalized_strings), "\\s+")))
    tokens <- tokens[nzchar(tokens)]
    tokens <- tokens[nchar(tokens) >= 3]
    tokens <- tokens[!tokens %in% stopwords]

    phrases <- unique(gsub("[^a-z0-9]+", "", normalized_strings))
    phrases <- phrases[nzchar(phrases)]

    list(
      candidate_id = row$candidate_id[[1]],
      tokens = tokens,
      phrases = phrases
    )
  })

  stats::setNames(alias_map, vapply(alias_map, `[[`, character(1), "candidate_id"))
}

infer_manual_source_candidate <- function(normalized_url, context_hint, candidates) {
  alias_map <- manual_candidate_aliases(candidates)
  if (length(alias_map) == 0) {
    return(list(
      candidate_id = NA_character_,
      candidate_confidence = 0,
      candidate_match_method = "no_candidates"
    ))
  }

  normalized_url_text <- iconv(normalized_url %||% "", to = "ASCII//TRANSLIT") |>
    tolower()
  normalized_context_text <- iconv(context_hint %||% "", to = "ASCII//TRANSLIT") |>
    tolower()
  corpus <- paste(normalized_url_text, normalized_context_text, sep = " ")

  score_candidate <- function(entry, text) {
    score <- 0
    compact_text <- gsub("[^a-z0-9]+", "", text)

    for (phrase in entry$phrases) {
      if (nzchar(phrase) && grepl(phrase, compact_text, fixed = TRUE)) {
        score <- score + 3
      }
    }

    for (token in entry$tokens) {
      if (nzchar(token) && grepl(token, text, fixed = TRUE)) {
        score <- score + 1
      }
    }

    score
  }

  scores <- vapply(alias_map, score_candidate, numeric(1), text = corpus)
  if (length(scores) == 0 || max(scores) <= 0) {
    return(list(
      candidate_id = NA_character_,
      candidate_confidence = 0,
      candidate_match_method = "no_match"
    ))
  }

  ordered <- sort(scores, decreasing = TRUE)
  top_candidate <- names(ordered)[[1]]
  top_score <- ordered[[1]]
  runner_up <- if (length(ordered) >= 2) ordered[[2]] else 0

  url_scores <- vapply(alias_map, score_candidate, numeric(1), text = normalized_url_text)
  context_scores <- vapply(alias_map, score_candidate, numeric(1), text = normalized_context_text)
  context_candidate_ids <- names(context_scores[context_scores >= 2])
  top_url_score <- url_scores[[top_candidate]] %||% 0

  if (length(context_candidate_ids) >= 2 && top_url_score < 2) {
    return(list(
      candidate_id = NA_character_,
      candidate_confidence = min(0.6, top_score / 5),
      candidate_match_method = "ambiguous_multi_candidate_context"
    ))
  }

  if (top_score < 2 || top_score <= runner_up) {
    return(list(
      candidate_id = NA_character_,
      candidate_confidence = min(0.6, top_score / 5),
      candidate_match_method = "ambiguous_match"
    ))
  }

  list(
    candidate_id = top_candidate,
    candidate_confidence = min(0.99, 0.5 + (top_score * 0.1)),
    candidate_match_method = "alias_score"
  )
}

manual_source_name_from_url <- function(url) {
  if (is.na(url) || !nzchar(url)) {
    return("Fuente manual")
  }

  host <- sub("^https?://", "", url)
  host <- sub("/.*$", "", host)
  host <- sub("^www\\.", "", host)

  lookup <- c(
    "x.com" = "X",
    "facebook.com" = "Facebook",
    "instagram.com" = "Instagram",
    "registraduria.gov.co" = "Registraduría Nacional",
    "cne.gov.co" = "Consejo Nacional Electoral",
    "moe.org.co" = "MOE"
  )

  if (host %in% names(lookup)) {
    return(unname(lookup[[host]]))
  }

  label <- gsub("[.-]", " ", host)
  label <- tools::toTitleCase(label)
  stringr::str_squish(label)
}

classify_manual_source_metadata <- function(url, candidate_id = NA_character_) {
  normalized_url <- normalize_manual_source_url(url)
  host <- sub("^https?://", "", normalized_url %||% "")
  host <- sub("/.*$", "", host)
  path <- sub("^https?://[^/]+", "", normalized_url %||% "")
  is_pdf <- grepl("\\.pdf($|\\?)", normalized_url %||% "", ignore.case = TRUE)
  is_social <- host %in% c("x.com", "facebook.com", "instagram.com")
  is_institutional <- grepl("(\\.gov\\.co$|moe\\.org\\.co$|transparenciacolombia\\.org\\.co$)", host)
  is_media <- grepl("(eltiempo|elespectador|semana|cambiocolombia|elpais|infobae|portafolio|larepublica|bluradio|caracol|wradio|rcn|noticiascaracol|canal1|cablenoticias|redmas|ntn24|teleantioquia|elcolombiano|elheraldo|vanguardia|eluniversal|laopinion|kienyke|pulzo|las2orillas|revistaraya|voragine|360radio|segurilatam)", host)

  source_tier <- dplyr::case_when(
    is_social ~ "social",
    is_institutional ~ "institutional",
    is_media ~ "established-media",
    !is.na(candidate_id) ~ "official",
    TRUE ~ "reference"
  )

  source_type <- dplyr::case_when(
    is_pdf ~ "document",
    is_social ~ "social-profile",
    is_institutional & grepl("encuestas|registro|calendario|resolucion", path) ~ "institutional-document",
    is_institutional ~ "institutional-page",
    is_media & grepl("/tags?/|/temas?/|/elecciones", path) ~ "topic-page",
    is_media ~ "article",
    !is.na(candidate_id) & grepl("programa|propuestas|plan", normalized_url %||% "", ignore.case = TRUE) ~ "program",
    !is.na(candidate_id) ~ "official-page",
    TRUE ~ "webpage"
  )

  list(
    source_name = manual_source_name_from_url(normalized_url),
    source_tier = source_tier,
    source_type = source_type
  )
}

extract_manual_published_at <- function(text, url = NA_character_, fallback = NA_character_) {
  url_date_parts <- stringr::str_match(url %||% "", "/(20[0-9]{2})/([01]?[0-9])/([0-3]?[0-9])")
  url_date <- if (nrow(url_date_parts) == 0 || all(is.na(url_date_parts[1, 2:4]))) {
    NA_character_
  } else {
    sprintf(
      "%04d-%02d-%02d",
      as.integer(url_date_parts[1, 2]),
      as.integer(url_date_parts[1, 3]),
      as.integer(url_date_parts[1, 4])
    )
  }

  candidates <- c(
    stringr::str_match(text %||% "", "(20[0-9]{2}-[01][0-9]-[0-3][0-9][T ][0-9]{2}:[0-9]{2}:[0-9]{2}Z?)")[, 2],
    stringr::str_match(text %||% "", "(20[0-9]{2}-[01][0-9]-[0-3][0-9])")[, 2],
    url_date,
    stringr::str_match(url %||% "", "(20[0-9]{2}-[01][0-9]-[0-3][0-9])")[, 2],
    stringr::str_match(url %||% "", "(20[0-9]{2}[01][0-9][0-3][0-9])")[, 2],
    fallback
  )

  candidates <- stats::na.omit(candidates)
  if (length(candidates) == 0) {
    return(as.POSIXct(NA, tz = "UTC"))
  }

  for (candidate in candidates) {
    if (grepl("^[0-9]{8}$", candidate)) {
      parsed <- suppressWarnings(as.POSIXct(strptime(candidate, "%Y%m%d", tz = "UTC")))
    } else {
      parsed <- suppressWarnings(as.POSIXct(candidate, tz = "UTC"))
      if (is.na(parsed)) {
        parsed <- suppressWarnings(as.POSIXct(strptime(candidate, "%Y-%m-%d", tz = "UTC")))
      }
    }

    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  as.POSIXct(NA, tz = "UTC")
}

extract_manual_title <- function(text, url = NA_character_) {
  title_match <- stringr::str_match(text %||% "", "<title[^>]*>(.*?)</title>")[, 2]
  title_match <- gsub("\\s+", " ", title_match %||% "")
  title_match <- stringr::str_trim(title_match)

  if (nzchar(title_match)) {
    return(title_match)
  }

  slug <- sub("^https?://", "", url %||% "")
  slug <- sub("^www\\.", "", slug)
  slug <- sub("\\?.*$", "", slug)
  slug <- sub("/$", "", slug)
  slug <- basename(slug)
  slug <- sub("\\.pdf$", "", slug, ignore.case = TRUE)
  slug <- gsub("[-_]+", " ", slug)
  slug <- stringr::str_squish(slug)

  if (!nzchar(slug)) {
    return(manual_source_name_from_url(url))
  }

  tools::toTitleCase(slug)
}

default_manual_source_url_validator <- function(url) {
  if (is.na(url) || !nzchar(url)) {
    return(list(
      reachable = FALSE,
      final_url = NA_character_,
      http_status = NA_integer_,
      title = NA_character_,
      published_at = as.POSIXct(NA, tz = "UTC"),
      validation_status = "invalid",
      status_reason = "empty_url"
    ))
  }

  body_file <- tempfile(fileext = ".tmp")
  header_file <- tempfile(fileext = ".headers")
  on.exit(unlink(c(body_file, header_file), force = TRUE), add = TRUE)

  timeout_seconds <- as.integer(getOption("manual_source_timeout_seconds", 4L))
  connect_timeout <- max(1L, min(timeout_seconds, 2L))

  args <- c(
    "-L",
    "--connect-timeout", as.character(connect_timeout),
    "--max-time", as.character(timeout_seconds),
    "--silent",
    "--show-error",
    "--output", body_file,
    "--dump-header", header_file,
    "--write-out", "status=%{http_code},url=%{url_effective},type=%{content_type}",
    url
  )

  result <- tryCatch(
    suppressWarnings(system2("curl", args = args, stdout = TRUE, stderr = TRUE)),
    error = function(error) structure(conditionMessage(error), status = 1L)
  )

  write_out <- result[[length(result)]] %||% ""
  status_code <- suppressWarnings(as.integer(sub(".*status=([0-9]{3}).*", "\\1", write_out)))
  final_url <- sub(".*url=([^,]*).*", "\\1", write_out)
  content_type <- sub(".*type=([^,]*).*", "\\1", write_out)

  if (identical(final_url, write_out)) {
    final_url <- url
  }

  if (identical(content_type, write_out)) {
    content_type <- ""
  }

  if (is.na(status_code)) {
    return(list(
      reachable = FALSE,
      final_url = url,
      http_status = NA_integer_,
      title = NA_character_,
      published_at = as.POSIXct(NA, tz = "UTC"),
      validation_status = "invalid",
      status_reason = "curl_failed"
    ))
  }

  body_text <- ""
  if (file.exists(body_file) && grepl("(html|text|json)", content_type, ignore.case = TRUE)) {
    body_lines <- readLines(body_file, warn = FALSE, encoding = "UTF-8")
    body_text <- paste(utils::head(body_lines, 400), collapse = "\n")
  }

  reachable <- status_code >= 200 && status_code < 400
  published_at <- extract_manual_published_at(body_text, url = final_url)

  list(
    reachable = reachable,
    final_url = final_url,
    http_status = status_code,
    title = extract_manual_title(body_text, url = final_url),
    published_at = published_at,
    validation_status = if (reachable) "reachable" else "unreachable",
    status_reason = if (reachable) "validated_by_http" else paste0("http_", status_code)
  )
}

manual_source_validator <- function() {
  getOption("manual_source_url_validator", default_manual_source_url_validator)
}

build_manual_source_registry <- function(project_dir = ".", candidates = NULL, validator = manual_source_validator()) {
  occurrences <- manual_source_occurrences(project_dir = project_dir)
  if (nrow(occurrences) == 0) {
    return(empty_manual_source_registry())
  }

  if (is.null(candidates)) {
    candidates <- load_candidate_registry(project_dir)
  }

  aggregated <- occurrences |>
    dplyr::group_by(.data$normalized_url) |>
    dplyr::summarise(
      raw_url = dplyr::first(.data$raw_url),
      source_files = paste(sort(unique(.data$source_file)), collapse = "|"),
      discovery_count = dplyr::n(),
      context_hint = paste(unique(stats::na.omit(.data$context_hint[nzchar(.data$context_hint)])), collapse = " | "),
      .groups = "drop"
    ) |>
    dplyr::mutate(entry_id = paste0("manual-", vapply(.data$normalized_url, manual_source_hash, character(1))))

  rows <- purrr::pmap_dfr(aggregated, function(normalized_url, raw_url, source_files, discovery_count, context_hint, entry_id) {
    candidate_guess <- infer_manual_source_candidate(normalized_url, context_hint, candidates)
    metadata <- classify_manual_source_metadata(normalized_url, candidate_id = candidate_guess$candidate_id)
    validation <- validator(normalized_url)
    final_url <- validation$final_url %||% normalized_url
    final_metadata <- classify_manual_source_metadata(final_url, candidate_id = candidate_guess$candidate_id)
    published_at <- validation$published_at %||% as.POSIXct(NA, tz = "UTC")

    public_status <- dplyr::case_when(
      identical(validation$validation_status, "invalid") ~ "discarded_invalid",
      !isTRUE(validation$reachable) ~ "discarded_unreachable",
      !is.na(candidate_guess$candidate_id) &&
        candidate_guess$candidate_confidence >= 0.75 &&
        !is.na(published_at) ~ "promoted",
      TRUE ~ "pending_classification"
    )
    status_reason <- paste(
      validation$status_reason %||% "unknown",
      candidate_guess$candidate_match_method %||% "unknown",
      sep = "|"
    )

    tibble::tibble(
      entry_id = entry_id,
      raw_url = raw_url,
      normalized_url = normalized_url,
      final_url = final_url,
      source_files = source_files,
      discovery_count = as.integer(discovery_count),
      context_hint = context_hint,
      source_name = final_metadata$source_name %||% metadata$source_name,
      source_tier = final_metadata$source_tier %||% metadata$source_tier,
      source_type = final_metadata$source_type %||% metadata$source_type,
      candidate_id = candidate_guess$candidate_id %||% NA_character_,
      candidate_confidence = candidate_guess$candidate_confidence %||% 0,
      candidate_match_method = candidate_guess$candidate_match_method %||% "unknown",
      title = validation$title %||% extract_manual_title("", url = final_url),
      published_at = as.POSIXct(published_at, tz = "UTC"),
      validation_status = validation$validation_status %||% "invalid",
      public_status = public_status,
      status_reason = status_reason,
      http_status = as.integer(validation$http_status %||% NA_integer_),
      reachable = isTRUE(validation$reachable),
      discovery_method = "manual_added_file",
      processed_at = Sys.time()
    )
  })

  rows |>
    dplyr::arrange(dplyr::desc(.data$reachable), dplyr::desc(.data$candidate_confidence), .data$normalized_url)
}

write_manual_source_registry <- function(registry, project_dir = ".") {
  path <- manual_source_state_path(project_dir)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(registry, path)
  invisible(path)
}

build_promoted_manual_sources <- function(registry, batch_label = as.character(Sys.Date())) {
  if (!is.data.frame(registry) || nrow(registry) == 0) {
    return(tibble::tibble())
  }

  registry |>
    dplyr::filter(.data$public_status == "promoted") |>
    dplyr::transmute(
      source_id = paste0("src-manual-", vapply(.data$normalized_url, manual_source_hash, character(1))),
      candidate_id = .data$candidate_id,
      published_at = .data$published_at,
      source_tier = .data$source_tier,
      source_type = .data$source_type,
      source_name = .data$source_name,
      url = .data$final_url,
      title = .data$title,
      quote_text = .data$title,
      confidence = pmax(.data$candidate_confidence, 0.75),
      inbox_batch = batch_label,
      discovery_method = .data$discovery_method,
      notes = paste0("origin_files=", .data$source_files)
    )
}

build_pending_manual_source_library <- function(registry) {
  if (!is.data.frame(registry) || nrow(registry) == 0) {
    return(tibble::tibble())
  }

  registry |>
    dplyr::filter(.data$public_status == "pending_classification", .data$reachable %in% TRUE) |>
    dplyr::transmute(
      entry_id = .data$entry_id,
      candidate_id = dplyr::na_if(.data$candidate_id, ""),
      source_name = .data$source_name,
      source_tier = .data$source_tier,
      source_type = .data$source_type,
      url = .data$final_url,
      title = .data$title,
      published_at = .data$published_at,
      status = .data$public_status,
      status_reason = .data$status_reason,
      candidate_confidence = .data$candidate_confidence,
      source_files = .data$source_files
    ) |>
    dplyr::arrange(dplyr::desc(.data$candidate_confidence), dplyr::desc(.data$published_at), .data$title)
}
