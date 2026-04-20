test_that("contract layout can be created without touching the public pipeline", {
  project_dir <- tempfile()
  dir.create(project_dir)

  paths <- ensure_contract_layout(project_dir)

  expect_true(all(dir.exists(paths)))
})

test_that("state tables are created with expected headers", {
  project_dir <- tempfile()
  dir.create(project_dir)

  ensure_state_tables(project_dir)

  source_registry <- readr::read_csv(file.path(project_dir, "data", "state", "source_registry.csv"), show_col_types = FALSE)
  candidate_state <- readr::read_csv(file.path(project_dir, "data", "state", "candidate_state.csv"), show_col_types = FALSE)

  expect_true(all(c("source_id", "source_url", "content_hash", "batch_date", "processed_at") %in% names(source_registry)))
  expect_true(all(c("candidate_id", "last_evidence_hash", "last_analysis_at", "last_batch_date", "dirty") %in% names(candidate_state)))
})

test_that("list_source_text_files finds captured source text files by batch", {
  project_dir <- tempfile()
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-20", "source_texts"), recursive = TRUE)
  writeLines("texto", file.path(project_dir, "data", "inbox", "2026-04-20", "source_texts", "src-1.md"))

  files <- list_source_text_files(project_dir, batch_date = "2026-04-20")

  expect_equal(nrow(files), 1)
  expect_equal(files$source_id[[1]], "src-1")
})
