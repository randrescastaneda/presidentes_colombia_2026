test_that("build_source_packets includes source text when available", {
  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence, ~inbox_batch,
    "src-1", "ivan-cepeda", as.POSIXct("2026-04-20 10:00:00", tz = "UTC"), "official", "program", "Programa", "https://example.com/a", "Titulo", "Cita breve", 0.95, "2026-04-20"
  )

  project_dir <- tempfile()
  dir.create(file.path(project_dir, "texts"), recursive = TRUE)
  text_path <- file.path(project_dir, "texts", "src-1.md")
  writeLines("texto extendido", text_path)

  source_text_files <- tibble::tibble(
    batch_date = as.Date("2026-04-20"),
    source_id = "src-1",
    path = text_path
  )

  packets <- build_source_packets(sources, source_text_files)

  expect_equal(length(packets), 1)
  expect_equal(packets[[1]]$text_content, "texto extendido")
  expect_equal(packets[[1]]$capture_method, "source_text_file")
})

test_that("materialize_extraction_results builds staged extraction from legacy claims", {
  project_dir <- tempfile()
  dir.create(project_dir)
  ensure_contract_layout(project_dir)

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence, ~inbox_batch,
    "src-1", "ivan-cepeda", as.POSIXct("2026-04-20 10:00:00", tz = "UTC"), "official", "program", "Programa", "https://example.com/a", "Titulo", "Cita breve", 0.95, "2026-04-20"
  )

  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~event_date, ~source_id, ~claim_type, ~policy_key, ~topic_id, ~summary_text, ~position_text, ~position_key, ~stance_value, ~implementation_detail, ~inbox_batch,
    "claim-1", "ivan-cepeda", as.Date("2026-04-20"), "src-1", "policy_proposal", "salud-preventiva", "salud", "Resumen", "Posicion", "a_favor", 1, TRUE, "2026-04-20"
  )

  extraction_results <- materialize_extraction_results(project_dir, claims, sources)
  flattened <- flatten_extraction_claims(extraction_results)

  expect_equal(length(extraction_results), 1)
  expect_true(file.exists(file.path(project_dir, "data", "staging", "extraction", "2026-04-20", "src-1.json")))
  expect_equal(flattened$claim_type_id[[1]], "propuesta_concreta")
  expect_equal(flattened$claim_type[[1]], "policy_proposal")
})

test_that("run_pipeline can derive claim records from explicit extraction results without claims.csv", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-20"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-20", "source_texts"), recursive = TRUE)
  ensure_contract_layout(project_dir)

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
      "ordenar-sistema-salud", -0.1, "orden institucional", "Tiende a correccion institucional."
    ),
    file.path(project_dir, "config", "ideology_rules.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "sergio-fajardo", "sergio-fajardo", "Sergio Fajardo", "Edna Bonilla", 13, TRUE, 5
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence,
      "src-1", "sergio-fajardo", "2026-04-20T11:00:00Z", "interview", "interview", "Blu Radio", "https://example.com/b", "Salud", "Reorganizar el sistema de salud", 0.85
    ),
    file.path(project_dir, "data", "inbox", "2026-04-20", "sources.csv")
  )

  extraction_result <- list(
    source_id = "src-1",
    batch_date = "2026-04-20",
    candidates_detected = c("sergio-fajardo"),
    claims = list(
      list(
        claim_id = "claim-1",
        candidate_id = "sergio-fajardo",
        claim_type = "postura_general",
        summary_text = "Prioriza reorganizacion del sistema de salud.",
        position_text = "Reorganizar el sistema de salud con auditoria fuerte.",
        topic_id = "salud",
        subtopic_id = NA_character_,
        policy_key = "ordenar-sistema-salud",
        mechanism_text = "Auditoria fuerte",
        target_population = NA_character_,
        problem_diagnosed = "Desorden institucional",
        stance_value = 1,
        specificity_score = 1L,
        ambiguity_flag = FALSE,
        insufficient_evidence_flag = FALSE,
        possible_contradiction_flag = FALSE,
        evidence_excerpt = "Reorganizar el sistema de salud con auditoria fuerte."
      )
    )
  )

  write_contract_json(
    extraction_result,
    file.path(project_dir, "data", "staging", "extraction", "2026-04-20", "src-1.json")
  )

  outputs <- run_pipeline(project_dir)

  expect_equal(outputs$claims$claim_type_id[[1]], "postura_general")
  expect_equal(outputs$claims$claim_type[[1]], "policy_proposal")
  expect_equal(outputs$site_metadata$pipeline_mode[[1]], "structured_extraction_only")
})
