test_that("run_pipeline materializes processed and public artifacts from inbox files", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-18"), recursive = TRUE)
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

  readr::write_csv(
    tibble::tribble(
      ~claim_id, ~candidate_id, ~event_date, ~source_id, ~claim_type, ~policy_key, ~topic_id, ~summary_text, ~position_text, ~position_key, ~stance_value, ~implementation_detail,
      "claim-1", "ivan-cepeda", "2026-04-18", "src-1", "policy_proposal", "salud-preventiva", "salud", "Fortalecer atención primaria", "Fortalecer atención primaria", "a_favor", 1, TRUE
    ),
    file.path(project_dir, "data", "inbox", "2026-04-18", "claims.csv")
  )

  outputs <- run_pipeline(project_dir)

  expect_true(file.exists(file.path(project_dir, "data", "processed", "claim_records.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "processed", "candidate_dossiers.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "daily_digest.json")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "site_metadata.json")))
  expect_equal(outputs$claims$claim_id, "claim-1")
  expect_equal(outputs$dossiers$total_claims, 1)
  expect_true(all(c("updated_at", "latest_event_date", "public_claim_count") %in% names(outputs$site_metadata)))
})
