args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages({
  library(jsonlite)
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
  if (length(x) == 0 || is.null(x) || all(is.na(x)) || identical(x, "")) {
    return(y)
  }
  x
}

project_dir <- normalizePath(option_value("--project-dir", "."), winslash = "/", mustWork = TRUE)
check_date <- as.Date(option_value("--date", as.character(Sys.Date())))
max_age_hours <- as.numeric(option_value("--max-age-hours", "30"))
notify <- has_flag("notify")

fail <- function(message, details = character()) {
  output <- c(
    paste("Automation health check failed:", message),
    details
  )
  cat(paste(output, collapse = "\n"), "\n")

  if (isTRUE(notify) && Sys.info()[["sysname"]] == "Darwin") {
    system2(
      "osascript",
      c(
        "-e",
        sprintf(
          "display notification %s with title %s",
          shQuote(message),
          shQuote("Colombia 2026 automation health: block")
        )
      ),
      stdout = FALSE,
      stderr = FALSE
    )
  }

  quit(status = 1)
}

report_path <- file.path(project_dir, "data", "automation", "run_reports", paste0(check_date, ".json"))
review_md <- file.path(project_dir, "data", "analysis", "daily_source_reviews", paste0(check_date, ".md"))
review_csv <- file.path(project_dir, "data", "analysis", "daily_source_reviews", paste0(check_date, ".csv"))

if (!file.exists(report_path)) {
  fail(
    paste("missing run report for", check_date),
    c("Expected:", report_path)
  )
}

report <- tryCatch(
  jsonlite::fromJSON(report_path, simplifyVector = FALSE),
  error = function(error) fail("run report is not valid JSON", conditionMessage(error))
)

status <- report$status %||% "missing"
if (identical(status, "block") || identical(status, "missing")) {
  fail(
    paste("run report status is", status),
    paste("Report:", report_path)
  )
}

generated_at <- as.POSIXct(report$generated_at %||% NA_character_, tz = "UTC")
if (is.na(generated_at)) {
  fail("run report has no parseable generated_at", paste("Report:", report_path))
}

age_hours <- as.numeric(difftime(Sys.time(), generated_at, units = "hours"))
if (!is.na(max_age_hours) && age_hours > max_age_hours) {
  fail(
    paste("run report is stale:", round(age_hours, 1), "hours old"),
    paste("Report:", report_path)
  )
}

if (!file.exists(review_md) || !file.exists(review_csv)) {
  fail(
    paste("daily source review artifacts are missing for", check_date),
    c(
      paste("Markdown exists:", file.exists(review_md)),
      paste("CSV exists:", file.exists(review_csv))
    )
  )
}

blocking_checks <- Filter(function(check) identical(check$status %||% "", "block"), report$checks %||% list())
if (length(blocking_checks) > 0) {
  fail(
    paste(length(blocking_checks), "blocking checks found in run report"),
    vapply(blocking_checks, function(check) paste0(check$check_id %||% "unknown", ": ", check$message %||% ""), character(1))
  )
}

summary <- report$summary %||% "Automation report passed."
cat("Automation health check passed.\n")
cat("Date:", as.character(check_date), "\n")
cat("Status:", status, "\n")
cat("Report:", report_path, "\n")
cat("Summary:", summary, "\n")

if (isTRUE(notify) && Sys.info()[["sysname"]] == "Darwin") {
  system2(
    "osascript",
    c(
      "-e",
      sprintf(
        "display notification %s with title %s",
        shQuote(summary),
        shQuote("Colombia 2026 automation health: pass")
      )
    ),
    stdout = FALSE,
    stderr = FALSE
  )
}
