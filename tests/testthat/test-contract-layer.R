test_that("contract layout can be created without touching the public pipeline", {
  project_dir <- tempfile()
  dir.create(project_dir)

  paths <- ensure_contract_layout(project_dir)

  expect_true(all(dir.exists(paths)))
  expect_true(file.exists(file.path(project_dir, "data", "program_documents", "program_documents.csv")))
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

test_that("create_daily_batch scaffolds sources and source_texts without claims.csv", {
  project_dir <- tempfile()
  dir.create(file.path(project_dir, "data", "inbox"), recursive = TRUE)
  writeLines("source_id,candidate_id,published_at,source_tier,source_type,source_name,url,title,quote_text,confidence", file.path(project_dir, "data", "inbox", "template_sources.csv"))
  writeLines("- source_id:\n\n## Source text or cleaned transcript", file.path(project_dir, "data", "inbox", "template_source_note.md"))

  script_path <- file.path(project_root, "scripts", "create_daily_batch.R")
  status <- system2("Rscript", c(shQuote(script_path), shQuote(project_dir), "2026-04-21"))

  expect_equal(status, 0)
  expect_true(file.exists(file.path(project_dir, "data", "inbox", "2026-04-21", "sources.csv")))
  expect_true(dir.exists(file.path(project_dir, "data", "inbox", "2026-04-21", "source_texts")))
  expect_false(file.exists(file.path(project_dir, "data", "inbox", "2026-04-21", "claims.csv")))
})
