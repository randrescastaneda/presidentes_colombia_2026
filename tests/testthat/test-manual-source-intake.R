test_that("build_manual_source_registry extracts, normalizes, and aggregates manual URLs", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 1, TRUE, 1,
      "paloma-valencia", "paloma-valencia", "Paloma Valencia", "Juan Daniel Oviedo", 2, TRUE, 2
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  writeLines(
    c(
      "https://palomavalencia.com/propuestas.html",
      "https://palomavalencia.com/propuestas.html?utm_source=test",
      "https://example.com/general"
    ),
    file.path(project_dir, "data", "added_manually", "manual.md")
  )

  validator <- function(url) {
    if (grepl("example.com/general", url, fixed = TRUE)) {
      return(list(
        reachable = TRUE,
        final_url = url,
        http_status = 200L,
        title = "General source",
        published_at = as.POSIXct(NA, tz = "UTC"),
        validation_status = "reachable",
        status_reason = "validated_by_http"
      ))
    }

    list(
      reachable = TRUE,
      final_url = url,
      http_status = 200L,
      title = "Paloma Valencia Propuestas",
      published_at = as.POSIXct("2026-04-20 00:00:00", tz = "UTC"),
      validation_status = "reachable",
      status_reason = "validated_by_http"
    )
  }

  registry <- build_manual_source_registry(project_dir = project_dir, validator = validator)

  expect_equal(nrow(registry), 2)
  paloma_row <- registry |>
    dplyr::filter(.data$candidate_id == "paloma-valencia") |>
    dplyr::slice_head(n = 1)
  pending_row <- registry |>
    dplyr::filter(.data$title == "General source") |>
    dplyr::slice_head(n = 1)

  expect_equal(paloma_row$discovery_count[[1]], 2L)
  expect_equal(paloma_row$public_status[[1]], "promoted")
  expect_equal(pending_row$public_status[[1]], "pending_classification")
})

test_that("list_manual_source_files ignores directory readmes", {
  project_dir <- tempfile()
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)
  writeLines("https://example.com/a", file.path(project_dir, "data", "added_manually", "manual.md"))
  writeLines("https://example.com/b", file.path(project_dir, "data", "added_manually", "README.md"))

  files <- list_manual_source_files(project_dir)

  expect_equal(length(files), 1)
  expect_match(files[[1]], "manual.md")
})

test_that("manual_source_occurrences tolerates files without URLs or readable text", {
  project_dir <- tempfile()
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)
  writeLines("sin urls aqui", file.path(project_dir, "data", "added_manually", "notes.md"))
  writeBin(
    as.raw(c(0x25, 0x50, 0x44, 0x46, 0x2d, 0xff, 0x00, 0x61)),
    file.path(project_dir, "data", "added_manually", "sample.pdf")
  )

  occurrences <- manual_source_occurrences(project_dir)

  expect_s3_class(occurrences, "tbl_df")
  expect_equal(nrow(occurrences), 0)
  expect_true(all(c("source_file", "context_hint", "raw_url", "normalized_url") %in% names(occurrences)))
})

test_that("build_promoted_manual_sources and pending library split registry rows by public status", {
  registry <- tibble::tribble(
    ~entry_id, ~raw_url, ~normalized_url, ~final_url, ~source_files, ~discovery_count, ~context_hint, ~source_name, ~source_tier, ~source_type, ~candidate_id, ~candidate_confidence, ~candidate_match_method, ~title, ~published_at, ~validation_status, ~public_status, ~status_reason, ~http_status, ~reachable, ~discovery_method, ~processed_at,
    "manual-a", "https://example.com/a", "https://example.com/a", "https://example.com/a", "data/added_manually/a.md", 1L, "", "Fuente A", "official", "article", "ivan-cepeda", 0.95, "alias_score", "Fuente A", as.POSIXct("2026-04-20 00:00:00", tz = "UTC"), "reachable", "promoted", "validated_by_http", 200L, TRUE, "manual_added_file", as.POSIXct("2026-04-23 12:00:00", tz = "UTC"),
    "manual-b", "https://example.com/b", "https://example.com/b", "https://example.com/b", "data/added_manually/b.md", 1L, "", "Fuente B", "reference", "webpage", NA_character_, 0.2, "no_match", "Fuente B", as.POSIXct(NA, tz = "UTC"), "reachable", "pending_classification", "validated_by_http", 200L, TRUE, "manual_added_file", as.POSIXct("2026-04-23 12:00:00", tz = "UTC")
  )

  promoted <- build_promoted_manual_sources(registry, batch_label = "manual-curated")
  pending <- build_pending_manual_source_library(registry)

  expect_equal(nrow(promoted), 1)
  expect_equal(promoted$inbox_batch[[1]], "manual-curated")
  expect_equal(nrow(pending), 1)
  expect_equal(pending$status[[1]], "pending_classification")
})

test_that("run_pipeline promotes validated manual sources and publishes pending manual library", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "processed"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "salud", NA_character_, "Salud", "salud", "Sistema de salud", TRUE, 1
    ),
    file.path(project_dir, "config", "taxonomy_v1.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority, ~coalition, ~party_or_group,
      "paloma-valencia", "paloma-valencia", "Paloma Valencia", "Juan Daniel Oviedo", 1, TRUE, 1, "Coalición", "Partido",
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 2, TRUE, 2, "Coalición", "Partido"
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  writeLines(
    c(
      "https://palomavalencia.com/2026/04/20/propuestas-seguridad.html",
      "https://example.com/fuente-general"
    ),
    file.path(project_dir, "data", "added_manually", "manual.md")
  )

  validator <- function(url) {
    if (grepl("palomavalencia.com", url, fixed = TRUE)) {
      return(list(
        reachable = TRUE,
        final_url = url,
        http_status = 200L,
        title = "Propuestas de seguridad",
        published_at = as.POSIXct("2026-04-20 00:00:00", tz = "UTC"),
        validation_status = "reachable",
        status_reason = "validated_by_http"
      ))
    }

    list(
      reachable = TRUE,
      final_url = url,
      http_status = 200L,
      title = "Fuente general",
      published_at = as.POSIXct(NA, tz = "UTC"),
      validation_status = "reachable",
      status_reason = "validated_by_http"
    )
  }

  old_options <- options(manual_source_url_validator = validator)
  on.exit(options(old_options), add = TRUE)

  outputs <- run_pipeline(project_dir)

  expect_true(file.exists(file.path(project_dir, "data", "state", "manual_source_registry.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "processed", "manual_source_library.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "manual_source_library.json")))
  expect_true(any(outputs$sources$url == "https://palomavalencia.com/2026/04/20/propuestas-seguridad.html"))

  pending <- readr::read_csv(file.path(project_dir, "data", "processed", "manual_source_library.csv"), show_col_types = FALSE)
  expect_true(any(pending$url == "https://example.com/fuente-general"))
})

test_that("run_pipeline prefers inbox sources over duplicated promoted manual sources", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-23"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "processed"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "salud", NA_character_, "Salud", "salud", "Sistema de salud", TRUE, 1
    ),
    file.path(project_dir, "config", "taxonomy_v1.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority, ~coalition, ~party_or_group,
      "paloma-valencia", "paloma-valencia", "Paloma Valencia", "Juan Daniel Oviedo", 1, TRUE, 1, "Coalición", "Partido"
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence,
      "src-existing", "paloma-valencia", "2026-04-20T00:00:00Z", "official", "article", "Paloma Valencia", "https://palomavalencia.com/propuestas", "Propuestas", "Texto base", 0.95
    ),
    file.path(project_dir, "data", "inbox", "2026-04-23", "sources.csv")
  )

  writeLines(
    "https://palomavalencia.com/propuestas",
    file.path(project_dir, "data", "added_manually", "manual.md")
  )

  validator <- function(url) {
    list(
      reachable = TRUE,
      final_url = url,
      http_status = 200L,
      title = "Propuestas",
      published_at = as.POSIXct("2026-04-20 00:00:00", tz = "UTC"),
      validation_status = "reachable",
      status_reason = "validated_by_http"
    )
  }

  old_options <- options(manual_source_url_validator = validator)
  on.exit(options(old_options), add = TRUE)

  outputs <- run_pipeline(project_dir)

  matching_sources <- outputs$sources |>
    dplyr::filter(.data$candidate_id == "paloma-valencia", .data$url == "https://palomavalencia.com/propuestas")

  expect_equal(nrow(matching_sources), 1)
  expect_equal(matching_sources$source_id[[1]], "src-existing")
})
