test_that("detect_analysis_notes flags supported changes of posture and contradictions", {
  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~event_date, ~source_id, ~claim_type, ~policy_key, ~topic_id, ~summary_text, ~position_text, ~position_key, ~stance_value, ~implementation_detail,
    "claim-1", "paloma-valencia", as.Date("2026-03-01"), "src-1", "policy_proposal", "impuesto-carbono", "ambiente-energia", "Rechaza impuesto al carbono", "Eliminar impuesto al carbono", "en_contra", -1, FALSE,
    "claim-2", "paloma-valencia", as.Date("2026-04-01"), "src-2", "policy_proposal", "impuesto-carbono", "ambiente-energia", "Acepta impuesto transitorio al carbono", "Mantener impuesto transitorio al carbono", "a_favor", 1, FALSE
  )

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~quote_text, ~confidence,
    "src-1", "paloma-valencia", as.POSIXct("2026-03-01 10:00:00", tz = "UTC"), "official", "interview", "Entrevista oficial", "https://example.com/1", "No apoyo ese impuesto", 0.92,
    "src-2", "paloma-valencia", as.POSIXct("2026-04-01 10:00:00", tz = "UTC"), "official", "program", "Programa", "https://example.com/2", "Proponemos un impuesto transitorio", 0.94
  )

  notes <- detect_analysis_notes(claims, sources)

  expect_true("cambio_de_postura" %in% notes$analysis_type)
  expect_true("contradiccion_interna" %in% notes$analysis_type)
  expect_true(all(notes$confidence >= 0.7))
})

test_that("detect_analysis_notes suppresses speculative contradictions", {
  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~event_date, ~source_id, ~claim_type, ~policy_key, ~topic_id, ~summary_text, ~position_text, ~position_key, ~stance_value, ~implementation_detail,
    "claim-1", "sergio-fajardo", as.Date("2026-03-05"), "src-1", "policy_proposal", "reforma-laboral", "empleo-empresa", "Habla de reforma gradual", "Reforma gradual", "matizado", 0, TRUE,
    "claim-2", "sergio-fajardo", as.Date("2026-04-07"), "src-2", "policy_proposal", "reforma-laboral", "empleo-empresa", "Nota ambigua en medio", "Comentario ambiguo", "matizado", 0, TRUE
  )

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~quote_text, ~confidence,
    "src-1", "sergio-fajardo", as.POSIXct("2026-03-05 09:00:00", tz = "UTC"), "official", "program", "Programa", "https://example.com/a", "Texto con plan", 0.91,
    "src-2", "sergio-fajardo", as.POSIXct("2026-04-07 09:00:00", tz = "UTC"), "media", "article", "Nota secundaria", "https://example.com/b", "Interpretación de tercero", 0.42
  )

  notes <- detect_analysis_notes(claims, sources)

  expect_equal(nrow(notes), 0)
})
