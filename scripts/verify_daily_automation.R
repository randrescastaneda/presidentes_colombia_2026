args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(purrr)
  library(readr)
  library(tibble)
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

git_status <- run_command("git", c("-C", project_dir, "status", "--short", "--branch"), timeout = 15)
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
  checks = checks,
  git_status = paste(git_status, collapse = "\n")
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
  })),
  "",
  "## Git status",
  "",
  "```",
  paste(git_status, collapse = "\n"),
  "```"
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
