test_that("run_pipeline materializes processed and public artifacts from inbox files", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-18"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-18", "source_texts"), recursive = TRUE)
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
      ~candidate_id, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority, ~coalition, ~party_or_group,
      "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 1, TRUE, 1, "Pacto Histórico", "Pacto Histórico"
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence,
      "src-1", "ivan-cepeda", "2026-04-18T11:00:00Z", "official", "program", "Programa", "https://example.com/programa", "Programa de salud", "La salud preventiva será central.", 0.95
    ),
    file.path(project_dir, "data", "inbox", "2026-04-18", "sources.csv")
  )

  writeLines(
    c(
      "- source_id: src-1",
      "- candidate_hint: ivan-cepeda",
      "",
      "## Structured claims",
      "",
      "### Claim 1",
      "- claim_type: propuesta_concreta",
      "- topic_id: salud",
      "- policy_key: salud-preventiva",
      "- summary_text: Fortalecer atención primaria.",
      "- position_text: Fortalecer atención primaria.",
      "- stance_value: 1",
      "- specificity_score: 2",
      "- ambiguity_flag: false",
      "- insufficient_evidence_flag: false",
      "- possible_contradiction_flag: false",
      "- evidence_excerpt: Fortalecer atención primaria.",
      "",
      "## Source text or cleaned transcript",
      "",
      "Fortalecer atención primaria."
    ),
    file.path(project_dir, "data", "inbox", "2026-04-18", "source_texts", "src-1.md")
  )

  outputs <- run_pipeline(project_dir)

  expect_true(file.exists(file.path(project_dir, "data", "processed", "claim_records.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "processed", "candidate_dossiers.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "daily_digest.json")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "site_metadata.json")))
  expect_equal(outputs$claims$claim_type_id[[1]], "propuesta_concreta")
  expect_equal(outputs$dossiers$total_claims, 1)
  expect_true(all(c("updated_at", "latest_event_date", "public_claim_count") %in% names(outputs$site_metadata)))
  expect_equal(outputs$site_metadata$pipeline_mode[[1]], "structured_extraction_auto")
})
