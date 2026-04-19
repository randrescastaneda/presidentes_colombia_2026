test_that("build_candidate_dossiers and build_daily_digest preserve traceability", {
  taxonomy <- tibble::tribble(
    ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order, ~level,
    "salud", NA_character_, "Salud", "salud", "Sistema de salud", TRUE, 1, 1
  )

  candidates <- tibble::tribble(
    ~candidate_id, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
    "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 1, TRUE, 1
  )

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~quote_text, ~confidence,
    "src-1", "ivan-cepeda", as.POSIXct("2026-04-10 09:00:00", tz = "UTC"), "official", "program", "Programa", "https://example.com/programa", "Cita verificable", 0.95
  )

  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~event_date, ~source_id, ~claim_type, ~policy_key, ~topic_id, ~summary_text, ~position_text, ~position_key, ~stance_value, ~implementation_detail,
    "claim-1", "ivan-cepeda", as.Date("2026-04-10"), "src-1", "policy_proposal", "salud-preventiva", "salud", "Fortalecer atención primaria", "Fortalecer atención primaria", "a_favor", 1, TRUE
  )

  analysis_notes <- tibble::tribble(
    ~analysis_id, ~candidate_id, ~analysis_type, ~claim_ids, ~source_ids, ~confidence, ~public_reasoning_summary,
    "an-1", "ivan-cepeda", "vacio_de_implementacion", "claim-1", "src-1", 0.88, "La propuesta no detalla costos ni calendario."
  )

  dossiers <- build_candidate_dossiers(candidates, claims, analysis_notes, sources, taxonomy)
  digest <- build_daily_digest(claims, analysis_notes, as.Date("2026-04-10"))

  expect_equal(dossiers$total_claims, 1)
  expect_equal(dossiers$total_sources, 1)
  expect_match(dossiers$top_topics, "Salud")
  expect_equal(digest$total_claims, 1)
  expect_equal(digest$total_analysis_notes, 1)
})

test_that("build_candidate_dossiers adds an ideology label from mapped policy signals", {
  taxonomy <- tibble::tribble(
    ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order, ~level,
    "paz", NA_character_, "Paz y conflicto armado", "paz", "Paz", TRUE, 1, 1,
    "fiscal", NA_character_, "Fiscal", "fiscal", "Fiscal", TRUE, 2, 1
  )

  candidates <- tibble::tribble(
    ~candidate_id, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
    "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 1, TRUE, 1,
    "abelardo-de-la-espriella", "Abelardo de la Espriella", "José Manuel Restrepo", 5, TRUE, 2
  )

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~quote_text, ~confidence,
    "src-1", "ivan-cepeda", as.POSIXct("2026-04-10 09:00:00", tz = "UTC"), "official", "program", "Programa", "https://example.com/ivan", "Cita verificable", 0.95,
    "src-2", "abelardo-de-la-espriella", as.POSIXct("2026-04-11 09:00:00", tz = "UTC"), "interview", "interview", "Entrevista", "https://example.com/abelardo", "Cita verificable", 0.85
  )

  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~event_date, ~source_id, ~claim_type, ~policy_key, ~topic_id, ~summary_text, ~position_text, ~position_key, ~stance_value, ~implementation_detail,
    "claim-1", "ivan-cepeda", as.Date("2026-04-10"), "src-1", "policy_proposal", "paz-total", "paz", "Profundizar paz total", "Profundizar paz total", "a_favor", 1, TRUE,
    "claim-2", "abelardo-de-la-espriella", as.Date("2026-04-11"), "src-2", "policy_proposal", "recorte-del-estado", "fiscal", "Recortar 40 % del Estado", "Recortar 40 % del Estado", "a_favor", 1, TRUE
  )

  analysis_notes <- tibble::tibble(
    analysis_id = character(),
    candidate_id = character(),
    analysis_type = character(),
    claim_ids = character(),
    source_ids = character(),
    confidence = numeric(),
    public_reasoning_summary = character()
  )

  ideology_rules <- tibble::tribble(
    ~policy_key, ~base_weight, ~label_hint,
    "paz-total", -0.8, "negociacion y justicia social",
    "recorte-del-estado", 0.9, "reduccion fuerte del Estado"
  )

  dossiers <- build_candidate_dossiers(
    candidates,
    claims,
    analysis_notes,
    sources,
    taxonomy,
    ideology_rules = ideology_rules
  )

  expect_equal(dossiers$ideology_label[dossiers$candidate_id == "ivan-cepeda"], "Centroizquierda")
  expect_equal(dossiers$ideology_label[dossiers$candidate_id == "abelardo-de-la-espriella"], "Centroderecha")
  expect_match(dossiers$ideology_rationale[dossiers$candidate_id == "abelardo-de-la-espriella"], "Estado")
})
