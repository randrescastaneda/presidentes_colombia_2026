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

write_output_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path)
}

write_output_json <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(data, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
}

run_pipeline <- function(project_dir = ".") {
  ensure_contract_layout(project_dir)
  ensure_state_tables(project_dir)

  taxonomy <- load_taxonomy(file.path(project_dir, "config", "taxonomy_v1.csv"))
  ideology_rules <- load_ideology_rules(file.path(project_dir, "config", "ideology_rules.csv"))
  analysis_axes <- load_analysis_axes(project_dir)
  analysis_axis_rules <- load_analysis_axis_rules(project_dir)
  candidates <- load_candidate_registry(project_dir)
  sources <- load_inbox_sources(project_dir)
  source_text_files <- list_source_text_files(project_dir)
  source_packets <- build_source_packets(sources, source_text_files)
  write_source_packets(source_packets, project_dir)
  extraction_results <- materialize_extraction_results(project_dir = project_dir, source_packets = source_packets)
  claims <- flatten_extraction_claims(extraction_results)

  screened <- screen_public_records(claims, sources)
  analysis_notes <- detect_analysis_notes(screened$public_claims, screened$public_sources)
  latest_event_date <- if (nrow(screened$public_claims) == 0) {
    as.Date(NA)
  } else {
    max(screened$public_claims$event_date, na.rm = TRUE)
  }
  report_date <- if (is.na(latest_event_date)) Sys.Date() else latest_event_date
  candidate_analysis <- build_candidate_analysis_artifacts(
    candidates = candidates,
    claims = screened$public_claims,
    sources = screened$public_sources,
    analysis_notes = analysis_notes,
    taxonomy = taxonomy,
    analysis_axes = analysis_axes,
    axis_rules = analysis_axis_rules,
    report_date = report_date
  )
  write_candidate_analysis_artifacts(candidate_analysis, project_dir = project_dir, report_date = report_date)
  candidate_analysis_summary <- candidate_analysis_summary_tibble(candidate_analysis)
  comparison_report <- build_comparison_report(
    candidate_analysis = candidate_analysis,
    candidates = candidates,
    claims = screened$public_claims,
    analysis_axes = analysis_axes,
    report_date = report_date
  )
  write_comparison_report(comparison_report, project_dir = project_dir, report_date = report_date)
  comparison_report_summary <- comparison_report_summary_tibble(comparison_report)
  editorial_packages <- build_editorial_packages(
    candidate_analysis = candidate_analysis,
    comparison_report = comparison_report,
    claims = screened$public_claims,
    candidates = candidates,
    report_date = report_date
  )
  write_editorial_packages(editorial_packages, project_dir = project_dir, report_date = report_date)
  editorial_package_index <- editorial_package_index_tibble(editorial_packages)
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

  validation_report <- build_validation_report(
    claims = screened$public_claims,
    candidate_analysis = candidate_analysis,
    comparison_report = comparison_report,
    editorial_packages = editorial_packages,
    report_date = report_date,
    project_dir = project_dir
  )

  validation_status <- tibble::tibble(
    report_id = validation_report$report_id,
    status = validation_report$status,
    summary = validation_report$summary
  )

  site_metadata <- tibble::tibble(
    updated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    latest_event_date = as.character(latest_event_date),
    public_claim_count = nrow(screened$public_claims),
    public_source_count = nrow(screened$public_sources),
    public_analysis_note_count = nrow(analysis_notes),
    candidate_analysis_count = length(candidate_analysis),
    comparison_report_count = if (is.null(comparison_report)) 0 else 1,
    editorial_package_count = length(editorial_packages),
    source_text_file_count = nrow(source_text_files),
    source_packet_count = length(source_packets),
    extraction_result_count = length(extraction_results),
    pipeline_mode = "structured_extraction_auto",
    validation_status = validation_report$status
  )

  publish_allowed <- public_publish_allowed(validation_report)

  processed_dir <- file.path(project_dir, "data", "processed")
  public_dir <- file.path(project_dir, "data", "public")
  validation_dir <- file.path(project_dir, "data", "staging", "validation")

  write_output_csv(screened$public_sources, file.path(processed_dir, "source_records.csv"))
  write_output_csv(screened$public_claims, file.path(processed_dir, "claim_records.csv"))
  write_output_csv(screened$rejected_claims, file.path(processed_dir, "rejected_claims.csv"))
  write_output_csv(analysis_notes, file.path(processed_dir, "analysis_notes.csv"))
  write_output_csv(candidate_analysis_summary, file.path(processed_dir, "candidate_analysis_summary.csv"))
  write_output_csv(comparison_report_summary, file.path(processed_dir, "comparison_report_summary.csv"))
  write_output_csv(dossiers, file.path(processed_dir, "candidate_dossiers.csv"))
  write_output_csv(daily_digest, file.path(processed_dir, "daily_digest.csv"))
  write_output_csv(editorial_package_index, file.path(processed_dir, "editorial_package_index.csv"))
  write_output_csv(site_metadata, file.path(processed_dir, "site_metadata.csv"))
  write_output_csv(validation_status, file.path(processed_dir, "validation_status.csv"))
  write_output_csv(ideology_rules, file.path(processed_dir, "ideology_rules.csv"))

  write_contract_json(validation_report, file.path(validation_dir, paste0(validation_report$report_id, ".json")))
  write_output_json(candidates, file.path(public_dir, "candidate_registry.json"))
  write_output_json(taxonomy, file.path(public_dir, "taxonomy_v1.json"))
  if (publish_allowed) {
    write_output_json(screened$public_sources, file.path(public_dir, "source_records.json"))
    write_output_json(screened$public_claims, file.path(public_dir, "claim_records.json"))
    write_output_json(analysis_notes, file.path(public_dir, "analysis_notes.json"))
    write_output_json(unname(candidate_analysis), file.path(public_dir, "candidate_analysis.json"))
    write_output_json(candidate_analysis_summary, file.path(public_dir, "candidate_analysis_summary.json"))
    write_output_json(comparison_report, file.path(public_dir, "comparison_report.json"))
    write_output_json(dossiers, file.path(public_dir, "candidate_dossiers.json"))
    write_output_json(daily_digest, file.path(public_dir, "daily_digest.json"))
    write_output_json(editorial_packages, file.path(public_dir, "editorial_packages.json"))
    write_output_json(editorial_package_index, file.path(public_dir, "editorial_package_index.json"))
    write_output_json(site_metadata, file.path(public_dir, "site_metadata.json"))
    write_output_json(ideology_rules, file.path(public_dir, "ideology_rules.json"))
  }
  write_output_json(validation_status, file.path(public_dir, "validation_status.json"))
  write_output_json(validation_report, file.path(public_dir, "validation_report.json"))

  list(
    taxonomy = taxonomy,
    ideology_rules = ideology_rules,
    candidates = candidates,
    sources = screened$public_sources,
    claims = screened$public_claims,
    rejected_claims = screened$rejected_claims,
    analysis_notes = analysis_notes,
    candidate_analysis = candidate_analysis,
    candidate_analysis_summary = candidate_analysis_summary,
    comparison_report = comparison_report,
    comparison_report_summary = comparison_report_summary,
    editorial_packages = editorial_packages,
    editorial_package_index = editorial_package_index,
    dossiers = dossiers,
    daily_digest = daily_digest,
    site_metadata = site_metadata,
    validation_report = validation_report,
    validation_status = validation_status,
    publish_allowed = publish_allowed
  )
}
