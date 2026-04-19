list_inbox_files <- function(project_dir, filename) {
  inbox_dir <- file.path(project_dir, "data", "inbox")

  if (!dir.exists(inbox_dir)) {
    return(character())
  }

  list.files(inbox_dir, pattern = paste0("^", filename, "$"), recursive = TRUE, full.names = TRUE)
}

load_candidate_registry <- function(project_dir) {
  readr::read_csv(
    file.path(project_dir, "config", "candidate_registry.csv"),
    show_col_types = FALSE
  ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("candidate_id", "slug", "president_name", "vicepresident_name", "coalition", "party_or_group", "source_url", "tracking_note")), as.character),
      dplyr::across(dplyr::any_of(c("ballot_position", "watchlist_priority")), as.integer),
      dplyr::across(dplyr::any_of("watchlist_active"), as.logical),
      dplyr::across(dplyr::any_of("source_date"), as.Date)
    )
}

load_inbox_sources <- function(project_dir) {
  source_files <- list_inbox_files(project_dir, "sources.csv")

  if (length(source_files) == 0) {
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
      inbox_batch = character()
    ))
  }

  purrr::map_dfr(source_files, \(path) {
    readr::read_csv(path, show_col_types = FALSE) |>
      dplyr::mutate(
        published_at = as.POSIXct(published_at, tz = "UTC"),
        inbox_batch = basename(dirname(path))
      )
  }) |>
    dplyr::distinct(source_id, .keep_all = TRUE)
}

load_inbox_claims <- function(project_dir) {
  claim_files <- list_inbox_files(project_dir, "claims.csv")

  if (length(claim_files) == 0) {
    return(tibble::tibble(
      claim_id = character(),
      candidate_id = character(),
      event_date = as.Date(character()),
      source_id = character(),
      claim_type = character(),
      policy_key = character(),
      topic_id = character(),
      summary_text = character(),
      position_text = character(),
      position_key = character(),
      stance_value = numeric(),
      implementation_detail = logical(),
      inbox_batch = character()
    ))
  }

  purrr::map_dfr(claim_files, \(path) {
    readr::read_csv(path, show_col_types = FALSE) |>
      dplyr::mutate(
        event_date = as.Date(event_date),
        inbox_batch = basename(dirname(path))
      )
  }) |>
    dplyr::distinct(claim_id, .keep_all = TRUE)
}

write_output_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path)
}

write_output_json <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(data, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
}

run_pipeline <- function(project_dir = ".") {
  taxonomy <- load_taxonomy(file.path(project_dir, "config", "taxonomy_v1.csv"))
  ideology_rules <- load_ideology_rules(file.path(project_dir, "config", "ideology_rules.csv"))
  candidates <- load_candidate_registry(project_dir)
  sources <- load_inbox_sources(project_dir)
  claims <- load_inbox_claims(project_dir)

  screened <- screen_public_records(claims, sources)
  analysis_notes <- detect_analysis_notes(screened$public_claims, screened$public_sources)
  dossiers <- build_candidate_dossiers(
    candidates = candidates,
    claims = screened$public_claims,
    analysis_notes = analysis_notes,
    sources = screened$public_sources,
    taxonomy = taxonomy,
    ideology_rules = ideology_rules
  )

  digest_dates <- screened$public_claims |>
    dplyr::distinct(event_date) |>
    dplyr::filter(!is.na(event_date)) |>
    dplyr::arrange(event_date) |>
    dplyr::pull(event_date)

  daily_digest <- if (length(digest_dates) == 0) {
    tibble::tibble(
      digest_date = as.Date(character()),
      total_claims = integer(),
      total_candidates = integer(),
      total_analysis_notes = integer()
    )
  } else {
    purrr::map_dfr(digest_dates, \(digest_date) {
      build_daily_digest(screened$public_claims, analysis_notes, digest_date)
    })
  }

  latest_event_date <- if (nrow(screened$public_claims) == 0) {
    as.Date(NA)
  } else {
    max(screened$public_claims$event_date, na.rm = TRUE)
  }

  site_metadata <- tibble::tibble(
    updated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    latest_event_date = as.character(latest_event_date),
    public_claim_count = nrow(screened$public_claims),
    public_source_count = nrow(screened$public_sources),
    public_analysis_note_count = nrow(analysis_notes)
  )

  processed_dir <- file.path(project_dir, "data", "processed")
  public_dir <- file.path(project_dir, "data", "public")

  write_output_csv(screened$public_sources, file.path(processed_dir, "source_records.csv"))
  write_output_csv(screened$public_claims, file.path(processed_dir, "claim_records.csv"))
  write_output_csv(screened$rejected_claims, file.path(processed_dir, "rejected_claims.csv"))
  write_output_csv(analysis_notes, file.path(processed_dir, "analysis_notes.csv"))
  write_output_csv(dossiers, file.path(processed_dir, "candidate_dossiers.csv"))
  write_output_csv(daily_digest, file.path(processed_dir, "daily_digest.csv"))
  write_output_csv(site_metadata, file.path(processed_dir, "site_metadata.csv"))
  write_output_csv(ideology_rules, file.path(processed_dir, "ideology_rules.csv"))

  write_output_json(candidates, file.path(public_dir, "candidate_registry.json"))
  write_output_json(taxonomy, file.path(public_dir, "taxonomy_v1.json"))
  write_output_json(screened$public_sources, file.path(public_dir, "source_records.json"))
  write_output_json(screened$public_claims, file.path(public_dir, "claim_records.json"))
  write_output_json(analysis_notes, file.path(public_dir, "analysis_notes.json"))
  write_output_json(dossiers, file.path(public_dir, "candidate_dossiers.json"))
  write_output_json(daily_digest, file.path(public_dir, "daily_digest.json"))
  write_output_json(site_metadata, file.path(public_dir, "site_metadata.json"))
  write_output_json(ideology_rules, file.path(public_dir, "ideology_rules.json"))

  list(
    taxonomy = taxonomy,
    ideology_rules = ideology_rules,
    candidates = candidates,
    sources = screened$public_sources,
    claims = screened$public_claims,
    rejected_claims = screened$rejected_claims,
    analysis_notes = analysis_notes,
    dossiers = dossiers,
    daily_digest = daily_digest,
    site_metadata = site_metadata
  )
}
