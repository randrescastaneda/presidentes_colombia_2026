test_that("build_candidate_analysis_artifacts covers all configured axes and themes", {
  candidates <- tibble::tribble(
    ~candidate_id, ~president_name,
    "sergio-fajardo", "Sergio Fajardo"
  )

  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~source_id, ~event_date, ~claim_type, ~claim_type_id, ~policy_key, ~topic_id, ~summary_text, ~position_text, ~stance_value, ~specificity_score, ~mechanism_text, ~ambiguity_flag, ~insufficient_evidence_flag,
    "claim-1", "sergio-fajardo", "src-1", as.Date("2026-04-20"), "policy_proposal", "propuesta_concreta", "ordenar-sistema-salud", "salud", "Ordenar el sistema publico de salud.", "Auditoria y gestion tecnica del sistema de salud.", 1, 2, "Auditoria y gestion tecnica", FALSE, FALSE,
    "claim-2", "sergio-fajardo", "src-2", as.Date("2026-04-20"), "policy_proposal", "postura_general", "continuidad-macroeconomica", "fiscal", "Mantener continuidad y ajuste gradual.", "Continuidad con ajuste gradual y enfoque institucional.", 1, 1, NA_character_, FALSE, FALSE
  )

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~confidence,
    "src-1", "sergio-fajardo", 0.9,
    "src-2", "sergio-fajardo", 0.85
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

  taxonomy <- tibble::tribble(
    ~topic_id, ~label_public,
    "salud", "Salud",
    "fiscal", "Fiscal"
  )

  analysis_axes <- tibble::tribble(
    ~axis_id, ~label_public, ~description, ~pole_a, ~pole_b, ~default_question, ~sort_order,
    "estado_vs_mercado", "Estado vs. mercado", "desc", "Estado coordinador", "Mercado coordinador", "pregunta", 1,
    "gradualismo_vs_disrupcion", "Gradualismo vs. disrupcion", "desc", "Gradualismo", "Disrupcion", "pregunta", 2
  )

  axis_rules <- tibble::tribble(
    ~axis_id, ~match_field, ~pattern, ~pole, ~rule_weight, ~rationale_fragment,
    "estado_vs_mercado", "combined_text", "sistema publico|salud", "pole_a", 1, "usa lenguaje de sistema publico",
    "gradualismo_vs_disrupcion", "combined_text", "gradual|continuidad|ajuste", "pole_a", 1, "usa lenguaje de cambio gradual"
  )

  artifacts <- build_candidate_analysis_artifacts(
    candidates = candidates,
    claims = claims,
    sources = sources,
    analysis_notes = analysis_notes,
    taxonomy = taxonomy,
    analysis_axes = analysis_axes,
    axis_rules = axis_rules,
    report_date = as.Date("2026-04-20")
  )

  expect_equal(length(artifacts), 1)
  expect_equal(artifacts[[1]]$candidate_id, "sergio-fajardo")
  expect_equal(length(artifacts[[1]]$ideology_axes), 2)
  expect_equal(length(artifacts[[1]]$thematic_analysis), 2)
  expect_match(artifacts[[1]]$political_philosophy, "filosofia politica|evidencia sugiere", ignore.case = TRUE)
})

test_that("run_pipeline writes candidate analysis artifacts and summaries", {
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
      "ordenar-sistema-salud", -0.1, "institucional", "Tiende a correccion institucional."
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
      "src-1", "sergio-fajardo", "2026-04-20T11:00:00Z", "interview", "interview", "Blu Radio", "https://example.com/b", "Salud", "Reorganizar el sistema publico de salud con auditoria.", 0.85
    ),
    file.path(project_dir, "data", "inbox", "2026-04-20", "sources.csv")
  )

  write_contract_json(
    list(
      source_id = "src-1",
      batch_date = "2026-04-20",
      candidates_detected = c("sergio-fajardo"),
      claims = list(
        list(
          claim_id = "claim-1",
          candidate_id = "sergio-fajardo",
          claim_type = "propuesta_concreta",
          summary_text = "Reorganizar el sistema publico de salud.",
          position_text = "Reorganizar el sistema publico de salud con auditoria.",
          topic_id = "salud",
          subtopic_id = NA_character_,
          policy_key = "ordenar-sistema-salud",
          mechanism_text = "Auditoria",
          target_population = NA_character_,
          problem_diagnosed = "Desorden institucional",
          stance_value = 1,
          specificity_score = 2L,
          ambiguity_flag = FALSE,
          insufficient_evidence_flag = FALSE,
          possible_contradiction_flag = FALSE,
          evidence_excerpt = "Reorganizar el sistema publico de salud con auditoria."
        )
      )
    ),
    file.path(project_dir, "data", "staging", "extraction", "2026-04-20", "src-1.json")
  )

  outputs <- run_pipeline(project_dir)

  expect_true(file.exists(file.path(project_dir, "data", "staging", "analysis", "2026-04-20", "sergio-fajardo.json")))
  expect_true(file.exists(file.path(project_dir, "data", "processed", "candidate_analysis_summary.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "candidate_analysis.json")))
  expect_equal(length(outputs$candidate_analysis), 1)
  expect_equal(outputs$site_metadata$candidate_analysis_count[[1]], 1)
})
