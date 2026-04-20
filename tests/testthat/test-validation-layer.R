test_that("build_legacy_validation_report returns warnings while structured extraction is pending", {
  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~source_id,
    "claim-1", "ivan-cepeda", "src-1"
  )

  analysis_notes <- tibble::tribble(
    ~analysis_id, ~candidate_id, ~source_ids,
    "an-1", "ivan-cepeda", "src-1"
  )

  report <- build_legacy_validation_report(
    claims = claims,
    analysis_notes = analysis_notes,
    source_text_files = tibble::tibble(),
    report_date = as.Date("2026-04-20")
  )

  expect_equal(report$status, "pass_with_warnings")
  expect_equal(report$report_id, "validation-2026-04-20")
  expect_equal(length(report$checks), 3)
})

test_that("run_pipeline materializes validation artifacts", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-20"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-20", "source_texts"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "salud", NA_character_, "Salud", "salud", "Sistema de salud", TRUE, 1
    ),
    file.path(project_dir, "config", "taxonomy_v1.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~policy_key, ~base_weight, ~label_hint, ~public_reasoning,
      "salud-preventiva", -0.2, "salud publica", "Tiende ligeramente hacia intervencion publica."
    ),
    file.path(project_dir, "config", "ideology_rules.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence,
      "src-1", "ivan-cepeda", "2026-04-20T11:00:00Z", "official", "program", "Programa", "https://example.com/programa", "Programa de salud", "La salud preventiva será central.", 0.95
    ),
    file.path(project_dir, "data", "inbox", "2026-04-20", "sources.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~claim_id, ~candidate_id, ~event_date, ~source_id, ~claim_type, ~policy_key, ~topic_id, ~summary_text, ~position_text, ~position_key, ~stance_value, ~implementation_detail,
      "claim-1", "ivan-cepeda", "2026-04-20", "src-1", "policy_proposal", "salud-preventiva", "salud", "Fortalecer atención primaria", "Fortalecer atención primaria", "a_favor", 1, TRUE
    ),
    file.path(project_dir, "data", "inbox", "2026-04-20", "claims.csv")
  )

  writeLines(
    "texto de prueba",
    file.path(project_dir, "data", "inbox", "2026-04-20", "source_texts", "src-1.md")
  )

  outputs <- run_pipeline(project_dir)

  expect_true(file.exists(file.path(project_dir, "data", "processed", "validation_status.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "validation_status.json")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "validation_report.json")))
  expect_true(file.exists(file.path(project_dir, "data", "staging", "validation", "validation-2026-04-20.json")))
  expect_equal(outputs$validation_status$status[[1]], "pass_with_warnings")
})
