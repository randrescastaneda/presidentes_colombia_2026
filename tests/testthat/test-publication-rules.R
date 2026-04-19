test_that("screen_public_records keeps only claims with traceable public sources", {
  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~event_date, ~source_id, ~claim_type, ~policy_key, ~topic_id, ~summary_text, ~position_text, ~position_key, ~stance_value, ~implementation_detail,
    "claim-1", "ivan-cepeda", as.Date("2026-04-10"), "src-1", "policy_proposal", "salud-preventiva", "salud", "Propone fortalecer atención primaria", "Fortalecer atención primaria", "a_favor", 1, TRUE,
    "claim-2", "ivan-cepeda", as.Date("2026-04-11"), "src-2", "policy_proposal", "salud-preventiva", "salud", "Afirmación sin fuente usable", "Texto no trazable", "a_favor", 1, TRUE
  )

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~quote_text, ~confidence,
    "src-1", "ivan-cepeda", as.POSIXct("2026-04-10 09:00:00", tz = "UTC"), "official", "speech", "Campaña oficial", "https://example.com/oficial", "Cita verificable", 0.95,
    "src-2", "ivan-cepeda", as.POSIXct(NA, tz = "UTC"), "media", "article", "Medio X", NA_character_, "Sin url ni fecha", 0.90
  )

  screened <- screen_public_records(claims, sources)

  expect_equal(screened$public_claims$claim_id, "claim-1")
  expect_equal(screened$rejected_claims$claim_id, "claim-2")
  expect_equal(screened$public_sources$source_id, "src-1")
})
