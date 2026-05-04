args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(purrr)
  library(readr)
  library(tibble)
  library(stringr)
})

option_value <- function(name, default = NULL) {
  prefix <- paste0(name, "=")
  match <- args[startsWith(args, prefix)]
  if (length(match) == 0) {
    return(default)
  }
  sub(prefix, "", match[[1]], fixed = TRUE)
}

has_flag <- function(name) {
  paste0("--", name) %in% args
}

`%||%` <- function(x, y) {
  if (length(x) == 0 || all(is.na(x)) || identical(x, "")) {
    return(y)
  }
  x
}

project_dir <- normalizePath(option_value("--project-dir", "."), winslash = "/", mustWork = TRUE)
review_date <- as.Date(option_value("--date", as.character(Sys.Date())))
notify <- has_flag("notify")
check_oracle <- has_flag("check-oracle")

source(file.path(project_dir, "R", "source_note_parsing.R"), local = FALSE)

read_csv_safe <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

read_json_safe <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

extract_review_structured_claim_blocks <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }

  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  extract_structured_claim_blocks(text)
}

run_command <- function(command, args = character(), timeout = 60) {
  command_line <- paste(c(shQuote(command), shQuote(args)), collapse = " ")
  tryCatch(
    system(paste(command_line, "2>&1"), intern = TRUE, timeout = timeout),
    error = function(error) structure(conditionMessage(error), status = 1L)
  )
}

check_result <- function(check_id, status, message, details = list()) {
  list(
    check_id = check_id,
    status = status,
    message = message,
    details = details
  )
}

relative_public_paths <- c("docs", "data/public", "data/processed")
banned_patterns <- c(
  "fuente incorporada desde data",
  "corpus reúne",
  "corpus reune",
  "afirmaciones trazables",
  "Fuente analizada",
  "Lectura integrada",
  "Mapa del debate"
)

claims <- read_csv_safe(file.path(project_dir, "data", "processed", "claim_records.csv"))
validation_report <- read_json_safe(file.path(project_dir, "data", "public", "validation_report.json"))
manual_registry <- read_csv_safe(file.path(project_dir, "data", "processed", "manual_source_registry.csv"))
candidate_registry <- read_csv_safe(file.path(project_dir, "config", "candidate_registry.csv"))
daily_review <- read_csv_safe(file.path(project_dir, "data", "analysis", "daily_source_reviews", paste0(review_date, ".csv")))
inbox_source_files <- list.files(file.path(project_dir, "data", "inbox"), pattern = "^sources[.]csv$", recursive = TRUE, full.names = TRUE)
daily_sources <- purrr::map_dfr(inbox_source_files, function(path) {
  read_csv_safe(path) |>
    dplyr::mutate(inbox_batch = basename(dirname(path)))
})
source_text_lookup <- tibble::tibble(
  source_id = tools::file_path_sans_ext(basename(list.files(file.path(project_dir, "data", "inbox"), pattern = "[.]md$", recursive = TRUE, full.names = TRUE))),
  source_text_path = list.files(file.path(project_dir, "data", "inbox"), pattern = "[.]md$", recursive = TRUE, full.names = TRUE)
) |>
  dplyr::filter(grepl("/source_texts/", .data$source_text_path, fixed = TRUE)) |>
  dplyr::distinct(source_id, .keep_all = TRUE)

checks <- list()

empty_topic_count <- if (nrow(claims) == 0 || !"topic_id" %in% names(claims)) {
  0L
} else {
  sum(is.na(claims$topic_id) | !nzchar(claims$topic_id))
}
checks <- append(checks, list(check_result(
  "public_claim_topic_ids",
  if (empty_topic_count == 0) "pass" else "block",
  if (empty_topic_count == 0) "No public claims have empty topic_id." else paste(empty_topic_count, "public claims have empty topic_id."),
  list(empty_topic_count = empty_topic_count)
)))

validation_status <- validation_report$status %||% "missing"
checks <- append(checks, list(check_result(
  "validation_report_status",
  if (identical(validation_status, "block") || identical(validation_status, "missing")) "block" else "pass",
  paste("validation_report status:", validation_status),
  list(summary = validation_report$summary %||% NA_character_)
)))

text_files <- unlist(lapply(relative_public_paths, function(relative_path) {
  path <- file.path(project_dir, relative_path)
  if (!dir.exists(path)) {
    return(character())
  }
  list.files(path, recursive = TRUE, full.names = TRUE)
}), use.names = FALSE)
text_files <- text_files[file.info(text_files)$isdir %in% FALSE]
text_files <- text_files[grepl("[.](html|json|csv|md|txt)$", text_files, ignore.case = TRUE)]

banned_hits <- purrr::map_dfr(text_files, function(path) {
  text <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(...) character())
  if (length(text) == 0) {
    return(tibble::tibble())
  }
  purrr::map_dfr(banned_patterns, function(pattern) {
    hit_lines <- grep(pattern, text, fixed = TRUE)
    if (length(hit_lines) == 0) {
      return(tibble::tibble())
    }
    tibble::tibble(
      path = sub(paste0("^", project_dir, "/"), "", path),
      pattern = pattern,
      line = hit_lines
    )
  })
})
checks <- append(checks, list(check_result(
  "public_internal_phrases",
  if (nrow(banned_hits) == 0) "pass" else "block",
  if (nrow(banned_hits) == 0) "No banned internal phrases were found in public artifacts." else paste(nrow(banned_hits), "banned internal phrase hits found."),
  list(hits = banned_hits)
)))

review_md <- file.path(project_dir, "data", "analysis", "daily_source_reviews", paste0(review_date, ".md"))
review_csv <- file.path(project_dir, "data", "analysis", "daily_source_reviews", paste0(review_date, ".csv"))
sources_page <- file.path(project_dir, "docs", "fuentes-evaluadas.html")
sources_page_text <- if (file.exists(sources_page)) {
  paste(readLines(sources_page, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
} else {
  ""
}
review_heading <- paste("Fuentes evaluadas -", review_date)
checks <- append(checks, list(check_result(
  "daily_source_review_rendered",
  if (file.exists(review_md) && file.exists(review_csv) && grepl(review_heading, sources_page_text, fixed = TRUE)) "pass" else "block",
  if (file.exists(review_md) && file.exists(review_csv) && grepl(review_heading, sources_page_text, fixed = TRUE)) {
    paste("Daily source review rendered for", review_date)
  } else {
    paste("Daily source review is missing or not rendered for", review_date)
  },
  list(
    review_md = file.exists(review_md),
    review_csv = file.exists(review_csv),
    fuentes_evaluadas_html = file.exists(sources_page),
    heading_present = grepl(review_heading, sources_page_text, fixed = TRUE)
  )
)))

ambiguous_promoted <- if (nrow(manual_registry) == 0 || !"candidate_match_method" %in% names(manual_registry)) {
  tibble::tibble()
} else {
  manual_registry |>
    dplyr::filter(
      .data$public_status == "promoted",
      .data$candidate_match_method %in% c("ambiguous_match", "ambiguous_multi_candidate_context", "no_match", "no_candidates")
    )
}
checks <- append(checks, list(check_result(
  "manual_sources_no_ambiguous_promotions",
  if (nrow(ambiguous_promoted) == 0) "pass" else "block",
  if (nrow(ambiguous_promoted) == 0) "No ambiguous manual sources were promoted." else paste(nrow(ambiguous_promoted), "ambiguous manual sources were promoted."),
  list(entries = ambiguous_promoted)
)))

allowed_editorial_actions <- c("incorporar", "incorporar_contexto", "reservar", "no_incorporar")
required_review_columns <- c("candidate_id", "source_name", "published_at", "title", "url", "editorial_action")
missing_review_columns <- setdiff(required_review_columns, names(daily_review))
invalid_review_actions <- if (nrow(daily_review) == 0 || !"editorial_action" %in% names(daily_review)) {
  tibble::tibble()
} else {
  daily_review |>
    dplyr::filter(!.data$editorial_action %in% allowed_editorial_actions)
}
known_candidate_ids <- candidate_registry$candidate_id %||% character()
valid_review_candidate_id <- function(value) {
  value <- normalize_source_note_optional_text(value)
  if (is.na(value) || identical(value, "multiple")) {
    return(!is.na(value))
  }

  candidate_ids <- unlist(strsplit(value, "\\|", perl = TRUE))
  candidate_ids <- stringr::str_squish(candidate_ids)
  length(candidate_ids) > 0 && all(candidate_ids %in% known_candidate_ids)
}
invalid_review_candidates <- if (nrow(daily_review) == 0 || !"candidate_id" %in% names(daily_review)) {
  tibble::tibble()
} else {
  daily_review |>
    dplyr::filter(!vapply(.data$candidate_id, valid_review_candidate_id, logical(1)))
}
blank_required_review_fields <- if (nrow(daily_review) == 0 || length(missing_review_columns) > 0) {
  tibble::tibble()
} else {
  daily_review |>
    dplyr::filter(if_any(dplyr::all_of(required_review_columns), \(value) is.na(value) | !nzchar(as.character(value))))
}
review_contract_failures <- list(
  missing_columns = missing_review_columns,
  invalid_actions = invalid_review_actions,
  invalid_candidates = invalid_review_candidates,
  blank_required_fields = blank_required_review_fields
)
review_contract_passes <- nrow(daily_review) > 0 &&
  length(missing_review_columns) == 0 &&
  nrow(invalid_review_actions) == 0 &&
  nrow(invalid_review_candidates) == 0 &&
  nrow(blank_required_review_fields) == 0
checks <- append(checks, list(check_result(
  "daily_source_review_contract",
  if (review_contract_passes) "pass" else "block",
  if (review_contract_passes) "Daily source review CSV uses valid actions, candidates, and required fields." else "Daily source review CSV has invalid actions, candidates, missing columns, or blank required fields.",
  review_contract_failures
)))

source_lookup <- if (nrow(daily_sources) == 0) {
  tibble::tibble(
    source_id = character(),
    inbox_candidate_id = character(),
    url = character(),
    inbox_batch = character(),
    source_text_path = character()
  )
} else {
  daily_sources |>
    dplyr::mutate(
      source_id = as.character(.data$source_id),
      url = as.character(.data$url),
      inbox_candidate_id = as.character(.data$candidate_id)
    ) |>
    dplyr::left_join(source_text_lookup, by = "source_id") |>
    dplyr::select("source_id", "inbox_candidate_id", "url", dplyr::any_of("inbox_batch"), dplyr::any_of("source_text_path")) |>
    dplyr::distinct(source_id, .keep_all = TRUE)
}

single_review_candidate_id <- function(value) {
  value <- normalize_source_note_optional_text(value)
  if (is.na(value) || identical(value, "multiple") || grepl("\\|", value, fixed = FALSE)) {
    return(NA_character_)
  }

  value
}

review_prepared <- if (review_contract_passes) {
  prepared <- daily_review
  if (!"source_id" %in% names(prepared)) {
    prepared$source_id <- NA_character_
  }

  prepared |>
    dplyr::mutate(
      review_row_id = dplyr::row_number(),
      review_source_id = normalize_source_note_optional_text(.data$source_id),
      url = as.character(.data$url),
      candidate_id = as.character(.data$candidate_id),
      single_candidate_id = vapply(.data$candidate_id, single_review_candidate_id, character(1))
    ) |>
    dplyr::select(-source_id)
} else {
  tibble::tibble()
}

resolve_review_source <- function(row) {
  review_source_id <- row$review_source_id[[1]]
  if (!is.na(review_source_id)) {
    matched <- source_lookup |>
      dplyr::filter(.data$source_id == !!review_source_id)

    return(row |>
      dplyr::mutate(
        source_id = if (nrow(matched) == 1) matched$source_id[[1]] else NA_character_,
        inbox_candidate_id = if (nrow(matched) == 1) matched$inbox_candidate_id[[1]] else NA_character_,
        inbox_batch = if (nrow(matched) == 1) matched$inbox_batch[[1]] else NA_character_,
        source_text_path = if (nrow(matched) == 1) matched$source_text_path[[1]] else NA_character_,
        source_match_count = nrow(matched),
        source_resolution_method = "source_id"
      ))
  }

  candidates <- source_lookup |>
    dplyr::filter(.data$url == row$url[[1]])
  single_candidate <- row$single_candidate_id[[1]]
  if (!is.na(single_candidate)) {
    candidates <- candidates |>
      dplyr::filter(.data$inbox_candidate_id == !!single_candidate)
  }

  row |>
    dplyr::mutate(
      source_id = if (nrow(candidates) == 1) candidates$source_id[[1]] else NA_character_,
      inbox_candidate_id = if (nrow(candidates) == 1) candidates$inbox_candidate_id[[1]] else NA_character_,
      inbox_batch = if (nrow(candidates) == 1) candidates$inbox_batch[[1]] else NA_character_,
      source_text_path = if (nrow(candidates) == 1) candidates$source_text_path[[1]] else NA_character_,
      source_match_count = nrow(candidates),
      source_resolution_method = "candidate_url"
    )
}

review_with_sources <- if (nrow(review_prepared) == 0) {
  tibble::tibble()
} else {
  purrr::map_dfr(seq_len(nrow(review_prepared)), \(i) resolve_review_source(review_prepared[i, , drop = FALSE]))
}

ambiguous_source_matches <- if (nrow(review_with_sources) == 0) {
  tibble::tibble()
} else {
  review_with_sources |>
    dplyr::filter(.data$source_match_count > 1)
}

provided_source_id_not_found <- if (nrow(review_with_sources) == 0) {
  tibble::tibble()
} else {
  review_with_sources |>
    dplyr::filter(!is.na(.data$review_source_id), .data$source_match_count == 0)
}

incorporate_without_source <- if (nrow(review_with_sources) == 0) {
  tibble::tibble()
} else {
  review_with_sources |>
    dplyr::filter(.data$editorial_action == "incorporar", is.na(.data$source_id) | !nzchar(.data$source_id))
}
non_incorporated_with_claims <- if (nrow(review_with_sources) == 0 || nrow(claims) == 0 || !"source_id" %in% names(claims)) {
  tibble::tibble()
} else {
  review_with_sources |>
    dplyr::filter(.data$editorial_action != "incorporar", !is.na(.data$source_id), nzchar(.data$source_id)) |>
    dplyr::inner_join(claims |> dplyr::select(source_id) |> dplyr::distinct(), by = "source_id")
}

claim_value <- function(data, field) {
  if (!field %in% names(data) || nrow(data) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  vapply(data[[field]], normalize_source_note_comparison_text, character(1))
}

expected_claims_from_blocks <- function(blocks, row, source_id, source_text_path) {
  purrr::imap_dfr(blocks, function(block, block_index) {
    tibble::tibble(
      source_id = source_id,
      block_index = block_index,
      review_row_id = row$review_row_id[[1]],
      review_url = row$url[[1]],
      review_candidate_id = row$candidate_id[[1]],
      source_text_path = source_text_path,
      candidate_id = normalize_source_note_optional_text(block$candidate_id %||% row$candidate_id[[1]]),
      claim_type_id = normalize_source_note_claim_type(block$claim_type %||% NA_character_, default = NA_character_),
      topic_id = normalize_source_note_optional_text(block$topic_id),
      subtopic_id = normalize_source_note_optional_text(block$subtopic_id),
      policy_key = normalize_source_note_optional_text(block$policy_key),
      evidence_excerpt = normalize_source_note_comparison_text(block$evidence_excerpt)
    )
  })
}

observed_claims_for_source <- function(source_claims, source_id, source_text_path, row) {
  if (nrow(source_claims) == 0) {
    return(tibble::tibble(
      source_id = character(),
      observed_claim_id = character(),
      review_row_id = integer(),
      review_url = character(),
      review_candidate_id = character(),
      source_text_path = character(),
      candidate_id = character(),
      claim_type_id = character(),
      topic_id = character(),
      subtopic_id = character(),
      policy_key = character(),
      evidence_excerpt = character()
    ))
  }

  tibble::tibble(
    source_id = source_id,
    observed_claim_id = source_claims$claim_id %||% NA_character_,
    review_row_id = row$review_row_id[[1]],
    review_url = row$url[[1]],
    review_candidate_id = row$candidate_id[[1]],
    source_text_path = source_text_path,
    candidate_id = claim_value(source_claims, "candidate_id"),
    claim_type_id = claim_value(source_claims, "claim_type_id"),
    topic_id = claim_value(source_claims, "topic_id"),
    subtopic_id = claim_value(source_claims, "subtopic_id"),
    policy_key = claim_value(source_claims, "policy_key"),
    evidence_excerpt = claim_value(source_claims, "evidence_excerpt")
  )
}

reconcile_curated_source <- function(row) {
  source_id <- row$source_id[[1]]
  source_text_path <- row$source_text_path[[1]]
  blocks <- extract_review_structured_claim_blocks(source_text_path)
  if (length(blocks) == 0) {
    return(tibble::tibble(
      mismatch_type = "missing_structured_claims",
      source_id = source_id,
      block_index = NA_integer_,
      review_row_id = row$review_row_id[[1]],
      review_url = row$url[[1]],
      review_candidate_id = row$candidate_id[[1]],
      source_text_path = source_text_path,
      expected_candidate_id = NA_character_,
      expected_claim_type_id = NA_character_,
      expected_topic_id = NA_character_,
      expected_subtopic_id = NA_character_,
      expected_policy_key = NA_character_,
      expected_evidence_excerpt = NA_character_,
      observed_claim_id = NA_character_,
      observed_candidate_id = NA_character_,
      observed_claim_type_id = NA_character_,
      observed_topic_id = NA_character_,
      observed_subtopic_id = NA_character_,
      observed_policy_key = NA_character_,
      observed_evidence_excerpt = NA_character_,
      observed_claim_count = sum(claims$source_id == source_id, na.rm = TRUE)
    ))
  }

  source_claims <- claims |>
    dplyr::filter(.data$source_id == !!source_id)
  expected <- expected_claims_from_blocks(blocks, row, source_id, source_text_path)
  observed <- observed_claims_for_source(source_claims, source_id, source_text_path, row)
  key_fields <- c("source_id", "candidate_id", "claim_type_id", "topic_id", "subtopic_id", "policy_key", "evidence_excerpt")

  invalid_expected <- expected |>
    dplyr::filter(is.na(.data$claim_type_id)) |>
    dplyr::transmute(
      mismatch_type = "invalid_structured_claim_type",
      source_id,
      block_index,
      review_row_id,
      review_url,
      review_candidate_id,
      source_text_path,
      expected_candidate_id = candidate_id,
      expected_claim_type_id = claim_type_id,
      expected_topic_id = topic_id,
      expected_subtopic_id = subtopic_id,
      expected_policy_key = policy_key,
      expected_evidence_excerpt = evidence_excerpt,
      observed_claim_id = NA_character_,
      observed_candidate_id = NA_character_,
      observed_claim_type_id = NA_character_,
      observed_topic_id = NA_character_,
      observed_subtopic_id = NA_character_,
      observed_policy_key = NA_character_,
      observed_evidence_excerpt = NA_character_,
      observed_claim_count = nrow(source_claims)
    )

  valid_expected <- expected |>
    dplyr::filter(!is.na(.data$claim_type_id))
  missing_expected <- valid_expected |>
    dplyr::anti_join(observed |> dplyr::select(dplyr::all_of(key_fields)), by = key_fields) |>
    dplyr::transmute(
      mismatch_type = "missing_expected_claim",
      source_id,
      block_index,
      review_row_id,
      review_url,
      review_candidate_id,
      source_text_path,
      expected_candidate_id = candidate_id,
      expected_claim_type_id = claim_type_id,
      expected_topic_id = topic_id,
      expected_subtopic_id = subtopic_id,
      expected_policy_key = policy_key,
      expected_evidence_excerpt = evidence_excerpt,
      observed_claim_id = NA_character_,
      observed_candidate_id = NA_character_,
      observed_claim_type_id = NA_character_,
      observed_topic_id = NA_character_,
      observed_subtopic_id = NA_character_,
      observed_policy_key = NA_character_,
      observed_evidence_excerpt = NA_character_,
      observed_claim_count = nrow(source_claims)
    )

  extra_observed <- observed |>
    dplyr::anti_join(valid_expected |> dplyr::select(dplyr::all_of(key_fields)), by = key_fields) |>
    dplyr::transmute(
      mismatch_type = "extra_observed_claim",
      source_id,
      block_index = NA_integer_,
      review_row_id,
      review_url,
      review_candidate_id,
      source_text_path,
      expected_candidate_id = NA_character_,
      expected_claim_type_id = NA_character_,
      expected_topic_id = NA_character_,
      expected_subtopic_id = NA_character_,
      expected_policy_key = NA_character_,
      expected_evidence_excerpt = NA_character_,
      observed_claim_id,
      observed_candidate_id = candidate_id,
      observed_claim_type_id = claim_type_id,
      observed_topic_id = topic_id,
      observed_subtopic_id = subtopic_id,
      observed_policy_key = policy_key,
      observed_evidence_excerpt = evidence_excerpt,
      observed_claim_count = nrow(source_claims)
    )

  dplyr::bind_rows(invalid_expected, missing_expected, extra_observed)
}

curated_mismatches <- if (nrow(review_with_sources) == 0 || nrow(claims) == 0) {
  tibble::tibble()
} else {
  review_with_sources |>
    dplyr::filter(.data$editorial_action == "incorporar", !is.na(.data$source_id), nzchar(.data$source_id)) |>
    purrr::pmap_dfr(function(...) reconcile_curated_source(tibble::as_tibble(list(...))))
}
curated_reconciliation_passes <- nrow(ambiguous_source_matches) == 0 &&
  nrow(provided_source_id_not_found) == 0 &&
  nrow(incorporate_without_source) == 0 &&
  nrow(non_incorporated_with_claims) == 0 &&
  nrow(curated_mismatches) == 0
checks <- append(checks, list(check_result(
  "daily_source_review_claim_reconciliation",
  if (curated_reconciliation_passes) "pass" else "block",
  if (curated_reconciliation_passes) "Incorporated curated sources reconcile with claim_records and non-incorporated review rows do not create claims." else "Daily source review decisions diverge from inbox sources or processed claims.",
  list(
    ambiguous_source_matches = ambiguous_source_matches,
    provided_source_id_not_found = provided_source_id_not_found,
    incorporate_without_source = incorporate_without_source,
    non_incorporated_with_claims = non_incorporated_with_claims,
    curated_mismatches = curated_mismatches
  )
)))

oracle_status <- "skipped"
oracle_output <- character()
if (isTRUE(check_oracle)) {
  if (!nzchar(Sys.which("oracle"))) {
    oracle_status <- "missing"
    oracle_output <- "oracle command not found"
  } else {
    oracle_output <- run_command(
      "oracle",
      c(
        "--engine", "browser",
        "--timeout", "90",
        "--slug", paste0("daily-automation-smoke-", format(review_date, "%Y%m%d")),
        "-p", "Smoke test only. Reply exactly: ORACLE_AUTOMATION_READY"
      ),
      timeout = 120
    )
    oracle_status <- if (any(grepl("ORACLE_AUTOMATION_READY", oracle_output, fixed = TRUE))) "pass" else "warn"
  }
}
checks <- append(checks, list(check_result(
  "oracle_browser_smoke",
  if (oracle_status %in% c("pass", "skipped")) "pass" else "warn",
  paste("Oracle browser smoke:", oracle_status),
  list(output = paste(utils::tail(oracle_output, 20), collapse = "\n"))
)))

block_count <- sum(vapply(checks, \(check) identical(check$status, "block"), logical(1)))
warn_count <- sum(vapply(checks, \(check) identical(check$status, "warn"), logical(1)))
overall_status <- if (block_count > 0) {
  "block"
} else if (warn_count > 0) {
  "pass_with_warnings"
} else {
  "pass"
}

report <- list(
  report_id = paste0("daily-automation-", review_date),
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  review_date = as.character(review_date),
  project_dir = project_dir,
  status = overall_status,
  summary = dplyr::case_when(
    overall_status == "block" ~ paste(block_count, "blocking automation checks failed."),
    overall_status == "pass_with_warnings" ~ paste(warn_count, "automation checks produced warnings."),
    TRUE ~ "All automation checks passed."
  ),
  checks = checks
)

report_dir <- file.path(project_dir, "data", "automation", "run_reports")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
json_path <- file.path(report_dir, paste0(review_date, ".json"))
md_path <- file.path(report_dir, paste0(review_date, ".md"))
jsonlite::write_json(report, json_path, auto_unbox = TRUE, pretty = TRUE, na = "null")

markdown_lines <- c(
  paste0("# Daily automation report - ", review_date),
  "",
  paste0("- Status: ", overall_status),
  paste0("- Generated at: ", report$generated_at),
  paste0("- Summary: ", report$summary),
  "",
  "## Checks",
  unlist(lapply(checks, function(check) {
    c(
      paste0("- `", check$check_id, "`: ", check$status),
      paste0("  ", check$message)
    )
  }))
)
writeLines(markdown_lines, md_path, useBytes = TRUE)

if (isTRUE(notify) && Sys.info()[["sysname"]] == "Darwin") {
  title <- paste("Colombia 2026 automation", overall_status)
  message <- report$summary
  system2(
    "osascript",
    c(
      "-e",
      sprintf(
        "display notification %s with title %s",
        shQuote(message),
        shQuote(title)
      )
    ),
    stdout = FALSE,
    stderr = FALSE
  )
}

cat(report$summary, "\n")
cat("Report:", json_path, "\n")

if (identical(overall_status, "block")) {
  quit(status = 1)
}
