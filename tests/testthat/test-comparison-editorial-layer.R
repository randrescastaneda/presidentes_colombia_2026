test_that("build_comparison_report keeps symmetric candidate rows", {
  candidates <- tibble::tribble(
    ~candidate_id, ~president_name, ~watchlist_active, ~watchlist_priority, ~ballot_position,
    "a", "Candidata A", TRUE, 1, 1,
    "b", "Candidato B", TRUE, 2, 2
  )

  candidate_analysis <- list(
    a = list(
      candidate_id = "a",
      ideology_axes = list(
        list(axis_id = "estado_vs_mercado", placement = "Estado coordinador", confidence = 0.8, rationale = "A"),
        list(axis_id = "gradualismo_vs_disrupcion", placement = "Gradualismo", confidence = 0.7, rationale = "A")
      ),
      thematic_analysis = list(
        list(
          topic_id = "salud",
          description = "Tema A",
          inference = "Inf A",
          evaluation = "Eval A",
          feasibility = list(political = "parcial", fiscal = "parcial", institutional = "parcial", administrative = "parcial"),
          tradeoffs = c("tradeoff"),
          uncertainties = c("unc")
        )
      ),
      uncertainties = c("unc A")
    ),
    b = list(
      candidate_id = "b",
      ideology_axes = list(
        list(axis_id = "estado_vs_mercado", placement = "Mercado coordinador", confidence = 0.8, rationale = "B"),
        list(axis_id = "gradualismo_vs_disrupcion", placement = "Gradualismo", confidence = 0.6, rationale = "B")
      ),
      thematic_analysis = list(
        list(
          topic_id = "salud",
          description = "Tema B",
          inference = "Inf B",
          evaluation = "Eval B",
          feasibility = list(political = "incierta", fiscal = "incierta", institutional = "incierta", administrative = "incierta"),
          tradeoffs = c("tradeoff"),
          uncertainties = c("unc")
        )
      ),
      uncertainties = c("unc B")
    )
  )

  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~topic_id, ~specificity_score, ~source_id,
    "claim-1", "a", "salud", 2, "src-1",
    "claim-2", "b", "salud", 1, "src-2"
  )

  analysis_axes <- tibble::tribble(
    ~axis_id, ~label_public, ~description, ~pole_a, ~pole_b, ~default_question, ~sort_order,
    "estado_vs_mercado", "Estado vs. mercado", "desc", "Estado coordinador", "Mercado coordinador", "q", 1,
    "gradualismo_vs_disrupcion", "Gradualismo vs. disrupcion", "desc", "Gradualismo", "Disrupcion", "q", 2
  )

  report <- build_comparison_report(
    candidate_analysis = candidate_analysis,
    candidates = candidates,
    claims = claims,
    analysis_axes = analysis_axes,
    report_date = as.Date("2026-04-20")
  )

  expect_equal(report$report_id, "comparison-watchlist-2026-04-20")
  expect_equal(length(report$axes_comparison), 2)
  expect_equal(length(report$axes_comparison[[1]]$candidate_positions), 2)
  expect_equal(length(report$topic_comparison[[1]]$candidate_rows), 2)
})

test_that("build_editorial_packages creates reusable publication artifacts", {
  candidates <- tibble::tribble(
    ~candidate_id, ~president_name,
    "a", "Candidata A",
    "b", "Candidato B"
  )

  candidate_analysis <- list(
    a = list(
      candidate_id = "a",
      source_ids = c("src-1"),
      claim_ids = c("claim-1"),
      profile_overview = "Perfil A",
      political_philosophy = "Filosofia A",
      ideology_axes = list(list(axis_id = "estado_vs_mercado", placement = "Estado coordinador", confidence = 0.8, rationale = "Rationale A")),
      thematic_analysis = list(
        list(
          topic_id = "salud",
          description = "Descripcion A",
          inference = "Inf A",
          evaluation = "Eval A",
          feasibility = list(political = "parcial", fiscal = "parcial", institutional = "parcial", administrative = "parcial"),
          tradeoffs = c("tradeoff"),
          uncertainties = c("unc")
        )
      ),
      internal_coherence = "Coherencia A",
      mainstream_distance = "Distancia A",
      strengths = c("Fortaleza A"),
      weaknesses = c("Debilidad A"),
      uncertainties = c("Incertidumbre A")
    )
  )

  comparison_report <- list(
    report_id = "comparison-watchlist-2026-04-20",
    candidate_ids = c("a", "b"),
    axes_comparison = list(
      list(
        axis_id = "estado_vs_mercado",
        candidate_positions = list(
          list(candidate_id = "a", placement = "Estado coordinador", confidence = 0.8),
          list(candidate_id = "b", placement = "Mercado coordinador", confidence = 0.8)
        ),
        summary = "Resumen eje"
      )
    ),
    topic_comparison = list(
      list(
        topic_id = "salud",
        candidate_rows = list(
          list(candidate_id = "a", priority = "alta", instrument = "mecanismo", specificity = "alta", coherence = "alta", feasibility = "parcial"),
          list(candidate_id = "b", priority = "media", instrument = "general", specificity = "media", coherence = "media", feasibility = "incierta")
        ),
        summary = "Resumen tema"
      )
    ),
    convergences = c("Convergencia"),
    divergences = c("Divergencia"),
    uncertainties = c("Incertidumbre")
  )

  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~source_id, ~event_date,
    "claim-1", "a", "src-1", as.Date("2026-04-20"),
    "claim-2", "b", "src-2", as.Date("2026-04-20")
  )

  packages <- build_editorial_packages(
    candidate_analysis = candidate_analysis,
    comparison_report = comparison_report,
    claims = claims,
    candidates = candidates,
    report_date = as.Date("2026-04-20")
  )

  expect_true(any(vapply(packages, \(x) identical(x$artifact_type, "candidate_profile"), logical(1))))
  expect_true(any(vapply(packages, \(x) identical(x$artifact_type, "comparison_report"), logical(1))))
  expect_true(any(vapply(packages, \(x) identical(x$artifact_type, "daily_update"), logical(1))))
  expect_true(any(vapply(packages, \(x) identical(x$artifact_type, "homepage_brief"), logical(1))))
})

test_that("run_pipeline writes comparison and editorial public artifacts", {
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
      "a", "a", "Candidata A", "Vice A", 1, TRUE, 1,
      "b", "b", "Candidato B", "Vice B", 2, TRUE, 2
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence,
      "src-1", "a", "2026-04-20T11:00:00Z", "interview", "interview", "Medio A", "https://example.com/a", "Salud A", "Sistema publico de salud", 0.85,
      "src-2", "b", "2026-04-20T11:00:00Z", "interview", "interview", "Medio B", "https://example.com/b", "Salud B", "Mercado y competencia en salud", 0.80
    ),
    file.path(project_dir, "data", "inbox", "2026-04-20", "sources.csv")
  )

  write_contract_json(
    list(
      source_id = "src-1",
      batch_date = "2026-04-20",
      candidates_detected = c("a"),
      claims = list(
        list(
          claim_id = "claim-1",
          candidate_id = "a",
          claim_type = "propuesta_concreta",
          summary_text = "Fortalecer sistema publico de salud.",
          position_text = "Fortalecer sistema publico con auditoria.",
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
          evidence_excerpt = "Fortalecer sistema publico con auditoria."
        )
      )
    ),
    file.path(project_dir, "data", "staging", "extraction", "2026-04-20", "src-1.json")
  )

  write_contract_json(
    list(
      source_id = "src-2",
      batch_date = "2026-04-20",
      candidates_detected = c("b"),
      claims = list(
        list(
          claim_id = "claim-2",
          candidate_id = "b",
          claim_type = "postura_general",
          summary_text = "Abrir espacio a competencia en salud.",
          position_text = "Abrir espacio a competencia y mercado en salud.",
          topic_id = "salud",
          subtopic_id = NA_character_,
          policy_key = "ordenar-sistema-salud",
          mechanism_text = NA_character_,
          target_population = NA_character_,
          problem_diagnosed = "Rigidez institucional",
          stance_value = 1,
          specificity_score = 1L,
          ambiguity_flag = FALSE,
          insufficient_evidence_flag = FALSE,
          possible_contradiction_flag = FALSE,
          evidence_excerpt = "Abrir espacio a competencia y mercado en salud."
        )
      )
    ),
    file.path(project_dir, "data", "staging", "extraction", "2026-04-20", "src-2.json")
  )

  outputs <- run_pipeline(project_dir)

  expect_true(file.exists(file.path(project_dir, "data", "public", "comparison_report.json")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "editorial_packages.json")))
  expect_true(file.exists(file.path(project_dir, "data", "processed", "comparison_report_summary.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "processed", "editorial_package_index.csv")))
  expect_equal(outputs$site_metadata$comparison_report_count[[1]], 1)
  expect_true(outputs$site_metadata$editorial_package_count[[1]] >= 3)
})
