test_that("build_homepage_view_model normalizes homepage artifacts and evidence states", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "processed"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "a", "candidata-a", "Candidata A", "Vice A", 1, TRUE, 1,
      "b", "candidato-b", "Candidato B", "Vice B", 2, TRUE, 2,
      "c", "candidato-c", "Candidato C", "Vice C", 3, FALSE, NA
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  jsonlite::write_json(
    list(
      a = list(
        artifact_id = "candidate-profile-a",
        artifact_type = "candidate_profile",
        title = "Perfil A",
        candidate_ids = "a",
        source_ids = list("src-1"),
        claim_ids = list("claim-1"),
        sections = list()
      ),
      `17` = list(
        artifact_id = "homepage-brief-2026-04-20",
        artifact_type = "homepage_brief",
        title = "Resumen ejecutivo",
        dek = "Resumen de prueba",
        candidate_ids = c("a", "b"),
        source_ids = c("src-1", "src-2"),
        claim_ids = c("claim-1", "claim-2"),
        sections = list(
          list(section_id = "top_changes", body = "Se procesaron cambios nuevos."),
          list(section_id = "key_comparison_note", body = "estado_vs_mercado: a=Estado coordinador; b=Evidencia insuficiente"),
          list(section_id = "caveats", body = "La evidencia sigue siendo parcial.")
        )
      ),
      `16` = list(
        artifact_id = "daily-update-2026-04-20",
        artifact_type = "daily_update",
        title = "Actualización diaria",
        candidate_ids = c("a", "b"),
        source_ids = c("src-1", "src-2"),
        claim_ids = c("claim-1", "claim-2"),
        sections = list(
          list(section_id = "what_changed", body = "Cambios del día."),
          list(section_id = "open_questions", body = "Persisten preguntas abiertas.")
        )
      )
    ),
    file.path(project_dir, "data", "public", "editorial_packages.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    list(
      report_id = "comparison-watchlist-2026-04-20",
      candidate_ids = c("a", "b"),
      topic_comparison = list(
        list(
          topic_id = "salud",
          candidate_rows = list(
            list(candidate_id = "a", priority = "alta", instrument = "mecanismo claro", specificity = "alta", coherence = "alta", feasibility = "parcialmente evaluable"),
            list(candidate_id = "b", priority = "sin evidencia suficiente", instrument = "sin evidencia suficiente", specificity = "sin evidencia suficiente", coherence = "sin evidencia suficiente", feasibility = "sin base suficiente")
          ),
          summary = "destacan candidata-a con mayor prioridad relativa."
        )
      )
    ),
    file.path(project_dir, "data", "public", "comparison_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    list(
      report_id = "validation-2026-04-20",
      status = "pass_with_warnings",
      summary = "Validation passed with warnings."
    ),
    file.path(project_dir, "data", "public", "validation_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  readr::write_csv(
    tibble::tibble(
      report_id = "validation-2026-04-20",
      status = "pass_with_warnings",
      summary = "Validation passed with warnings."
    ),
    file.path(project_dir, "data", "processed", "validation_status.csv")
  )

  model <- build_homepage_view_model(project_dir = project_dir)

  expect_equal(model$title, "Resumen ejecutivo")
  expect_equal(model$top_changes, "Se procesaron cambios nuevos.")
  expect_match(model$key_comparison_note, "La comparación más útil hoy aparece en Salud")
  expect_no_match(model$key_comparison_note, "estado_vs_mercado|candidate_id|topic_id")
  expect_equal(model$methodology_badge$status, "pass_with_warnings")
  expect_equal(model$methodology_badge$label, "Metodología con advertencias")
  expect_length(model$comparison_blocks, 1)
  expect_equal(model$comparison_blocks[[1]]$evidence_state, "partial")
  expect_equal(model$comparison_blocks[[1]]$public_label, "lectura parcial")
  expect_match(model$comparison_blocks[[1]]$summary, "Candidata A")
  expect_no_match(model$comparison_blocks[[1]]$summary, "candidata-a|candidate_id|topic_id")
  expect_equal(model$comparison_blocks[[1]]$handoff$topic_or_axis, "salud")
  expect_equal(model$comparison_blocks[[1]]$handoff$section_anchor, "topic-salud")
  expect_equal(model$comparison_blocks[[1]]$handoff$fallback_destination, "comparador.html")
  expect_equal(model$comparison_blocks[[1]]$handoff$candidate_destinations[[1]]$href, "candidatos/candidata-a.html?from=homepage&topic=salud#propuestas-y-posiciones-publicas")
  expect_true(any(vapply(model$roster, \(entry) identical(entry$candidate_id, "c"), logical(1))))
  expect_false("c" %in% model$comparison_blocks[[1]]$candidate_ids)
})

test_that("build_homepage_view_model degrades safely when homepage brief or comparison report is missing", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "processed"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "a", "candidata-a", "Candidata A", "Vice A", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  jsonlite::write_json(
    list(
      `16` = list(
        artifact_id = "daily-update-2026-04-20",
        artifact_type = "daily_update",
        title = "Actualización diaria",
        candidate_ids = "a",
        source_ids = "src-1",
        claim_ids = "claim-1",
        sections = list(
          list(section_id = "what_changed", body = "Cambios del día."),
          list(section_id = "open_questions", body = "Persisten preguntas abiertas.")
        )
      )
    ),
    file.path(project_dir, "data", "public", "editorial_packages.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    list(
      report_id = "validation-2026-04-20",
      status = "pass",
      summary = "Validation passed with no blocking issues."
    ),
    file.path(project_dir, "data", "public", "validation_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  model <- build_homepage_view_model(project_dir = project_dir)

  expect_equal(model$title, "Resumen ejecutivo")
  expect_equal(model$top_changes, "Cambios del día.")
  expect_equal(model$key_comparison_note, "Persisten preguntas abiertas.")
  expect_equal(model$methodology_badge$label, "Metodología verificada")
  expect_length(model$comparison_blocks, 0)
  expect_match(model$empty_state, "Todavía no hay suficientes diferencias comparables")
})

test_that("build_homepage_view_model tolerates missing processed validation status and unknown candidate ids", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "processed"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "a", "candidata-a", "Candidata A", "Vice A", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  jsonlite::write_json(
    list(
      `17` = list(
        artifact_id = "homepage-brief-2026-04-20",
        artifact_type = "homepage_brief",
        title = "Resumen ejecutivo",
        sections = list(
          list(section_id = "top_changes", body = "Cambios nuevos."),
          list(section_id = "key_comparison_note", body = "estado_vs_mercado: a=Estado coordinador; b=Evidencia insuficiente")
        )
      )
    ),
    file.path(project_dir, "data", "public", "editorial_packages.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    list(
      report_id = "comparison-watchlist-2026-04-20",
      candidate_ids = c("a", "missing-candidate"),
      topic_comparison = list(
        list(
          topic_id = "salud-publica",
          candidate_rows = list(
            list(candidate_id = "a", priority = "alta", specificity = "alta", feasibility = "parcialmente evaluable"),
            list(candidate_id = "missing-candidate", priority = "alta", specificity = "alta", feasibility = "parcialmente evaluable")
          ),
          summary = "destacan missing-candidate y candidata-a con mayor prioridad relativa."
        )
      )
    ),
    file.path(project_dir, "data", "public", "comparison_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    list(
      report_id = "validation-2026-04-20",
      status = "pass",
      summary = "Validation passed with no blocking issues."
    ),
    file.path(project_dir, "data", "public", "validation_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  expect_no_error(model <- build_homepage_view_model(project_dir = project_dir))
  expect_equal(model$methodology_badge$label, "Metodología verificada")
  expect_equal(model$comparison_blocks[[1]]$candidate_ids, "a")
  expect_length(model$comparison_blocks[[1]]$handoff$candidate_destinations, 1)
  expect_equal(model$comparison_blocks[[1]]$handoff$candidate_destinations[[1]]$candidate_id, "a")
  expect_no_match(model$comparison_blocks[[1]]$summary, "missing-candidate")
})

test_that("build_homepage_view_model normalizes homepage handoffs to root topic ids", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "a", "candidata-a", "Candidata A", "Vice A", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "salud", NA_character_, "Salud", "salud", "Sistema de salud", TRUE, 1,
      "salud-publica", "salud", "Salud pública", "salud-publica", "Subtema de salud", TRUE, 2
    ),
    file.path(project_dir, "config", "taxonomy_v1.csv")
  )

  jsonlite::write_json(
    list(
      report_id = "comparison-watchlist-2026-04-20",
      candidate_ids = "a",
      topic_comparison = list(
        list(
          topic_id = "salud-publica",
          candidate_rows = list(
            list(candidate_id = "a", priority = "alta", instrument = "ley", specificity = "alta", coherence = "alta", feasibility = "parcialmente evaluable")
          ),
          summary = "Tema útil para comparar."
        )
      )
    ),
    file.path(project_dir, "data", "public", "comparison_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    list(
      report_id = "validation-2026-04-20",
      status = "pass",
      summary = "Validation passed with no blocking issues."
    ),
    file.path(project_dir, "data", "public", "validation_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  model <- build_homepage_view_model(project_dir = project_dir)

  expect_equal(model$comparison_blocks[[1]]$topic_id, "salud-publica")
  expect_equal(model$comparison_blocks[[1]]$handoff$topic_or_axis, "salud")
  expect_equal(model$comparison_blocks[[1]]$handoff$section_anchor, "topic-salud")
  expect_equal(model$comparison_blocks[[1]]$handoff$candidate_destinations[[1]]$href, "candidatos/candidata-a.html?from=homepage&topic=salud#propuestas-y-posiciones-publicas")
})

test_that("build_comparison_view_model derives comparison destinations from public JSON only", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "a", "candidata-a", "Candidata A", "Vice A", 1, TRUE, 1,
      "b", "candidato-b", "Candidato B", "Vice B", 2, TRUE, 2
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "salud", NA_character_, "Salud", "salud", "Sistema de salud", TRUE, 1
    ),
    file.path(project_dir, "config", "taxonomy_v1.csv")
  )

  jsonlite::write_json(
    tibble::tribble(
      ~claim_id, ~candidate_id, ~topic_id, ~claim_type, ~event_date, ~source_id, ~position_text, ~summary_text,
      "claim-1", "a", "salud", "policy_proposal", "2026-04-20", "src-1", "Fortalecer la red pública.", "Plantea fortalecer la red pública.",
      "claim-2", "b", "salud", "policy_proposal", "2026-04-20", "src-2", "Revisar incentivos de aseguramiento.", "Plantea una revisión gradual."
    ),
    file.path(project_dir, "data", "public", "claim_records.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    tibble::tribble(
      ~source_id, ~candidate_id, ~published_at, ~source_name, ~url,
      "src-1", "a", "2026-04-20T10:00:00Z", "Fuente A", "https://example.com/a",
      "src-2", "b", "2026-04-20T10:00:00Z", "Fuente B", "https://example.com/b"
    ),
    file.path(project_dir, "data", "public", "source_records.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    tibble::tribble(
      ~document_id, ~candidate_id, ~title, ~is_primary, ~published_at, ~download_url,
      "doc-a", "a", "Programa A", TRUE, "2026-04-20", "https://example.com/doc-a"
    ),
    file.path(project_dir, "data", "public", "program_documents.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    list(
      report_id = "comparison-watchlist-2026-04-20",
      candidate_ids = c("a", "b"),
      topic_comparison = list(
        list(
          topic_id = "salud",
          candidate_rows = list(
            list(candidate_id = "a", priority = "alta", instrument = "ley", specificity = "alta", coherence = "alta", feasibility = "parcialmente evaluable"),
            list(candidate_id = "b", priority = "sin evidencia suficiente", instrument = "sin evidencia suficiente", specificity = "sin evidencia suficiente", coherence = "sin evidencia suficiente", feasibility = "sin base suficiente")
          ),
          summary = "Tema útil para comparar a la candidata A."
        )
      )
    ),
    file.path(project_dir, "data", "public", "comparison_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  model <- build_comparison_view_model(project_dir = project_dir)

  expect_length(model$topics, 1)
  expect_equal(model$topics[[1]]$topic_label, "Salud")
  expect_equal(model$topics[[1]]$candidate_cards[[1]]$destination_state, "comparable")
  expect_equal(model$topics[[1]]$candidate_cards[[1]]$href, "candidatos/candidata-a.html?from=comparador&topic=salud#propuestas-y-posiciones-publicas")
  expect_equal(model$topics[[1]]$candidate_cards[[2]]$destination_state, "documented_only")
  expect_equal(model$topics[[1]]$candidate_cards[[2]]$href, "candidatos/candidato-b.html?from=comparador&topic=salud#propuestas-y-posiciones-publicas")
  expect_equal(model$topics[[1]]$candidate_cards[[1]]$primary_document$title, "Programa A")
})

test_that("build_candidate_policy_view_model separates comparable and documented-only topics", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "a", "candidata-a", "Candidata A", "Vice A", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "salud", NA_character_, "Salud", "salud", "Sistema de salud", TRUE, 1,
      "empleo", NA_character_, "Empleo", "empleo", "Trabajo e ingresos", TRUE, 2
    ),
    file.path(project_dir, "config", "taxonomy_v1.csv")
  )

  jsonlite::write_json(
    tibble::tribble(
      ~claim_id, ~candidate_id, ~topic_id, ~claim_type, ~event_date, ~source_id, ~position_text, ~summary_text,
      "claim-1", "a", "salud", "policy_proposal", "2026-04-20", "src-1", "Fortalecer la red pública.", "Plantea fortalecer la red pública.",
      "claim-2", "a", "empleo", "policy_proposal", "2026-04-21", "src-2", "Impulsar formación dual.", "Describe una propuesta laboral."
    ),
    file.path(project_dir, "data", "public", "claim_records.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    tibble::tribble(
      ~source_id, ~candidate_id, ~published_at, ~source_name, ~url,
      "src-1", "a", "2026-04-20T10:00:00Z", "Fuente A", "https://example.com/a",
      "src-2", "a", "2026-04-21T10:00:00Z", "Fuente B", "https://example.com/b"
    ),
    file.path(project_dir, "data", "public", "source_records.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    list(
      report_id = "comparison-watchlist-2026-04-20",
      candidate_ids = "a",
      topic_comparison = list(
        list(
          topic_id = "salud",
          candidate_rows = list(
            list(candidate_id = "a", priority = "alta", instrument = "ley", specificity = "alta", coherence = "alta", feasibility = "parcialmente evaluable")
          ),
          summary = "Salud ya es comparable."
        )
      )
    ),
    file.path(project_dir, "data", "public", "comparison_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  model <- build_candidate_policy_view_model(
    candidate_id = "a",
    topic_id = "empleo",
    from = "comparador",
    project_dir = project_dir
  )

  expect_equal(model$candidate_name, "Candidata A")
  expect_length(model$comparable_sections, 1)
  expect_equal(model$comparable_sections[[1]]$topic_id, "salud")
  expect_length(model$documented_sections, 1)
  expect_equal(model$documented_sections[[1]]$topic_id, "empleo")
  expect_equal(model$topic_focus$root_topic_id, "empleo")
  expect_equal(model$topic_focus$state, "documented_only")
  expect_equal(model$topic_focus$state_label, "documentado, aún no comparable")
  expect_false(model$empty_state)
})

test_that("build_candidate_policy_view_model preserves documented-only state for topics missing in taxonomy", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "a", "candidata-a", "Candidata A", "Vice A", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "salud", NA_character_, "Salud", "salud", "Sistema de salud", TRUE, 1
    ),
    file.path(project_dir, "config", "taxonomy_v1.csv")
  )

  jsonlite::write_json(
    tibble::tribble(
      ~claim_id, ~candidate_id, ~topic_id, ~claim_type, ~event_date, ~source_id, ~position_text, ~summary_text,
      "claim-1", "a", "seguridad-rural", "policy_proposal", "2026-04-21", "src-1", "Fortalecer presencia territorial.", "Describe una propuesta sin taxonomía aún."
    ),
    file.path(project_dir, "data", "public", "claim_records.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    tibble::tribble(
      ~source_id, ~candidate_id, ~published_at, ~source_name, ~url,
      "src-1", "a", "2026-04-21T10:00:00Z", "Fuente A", "https://example.com/a"
    ),
    file.path(project_dir, "data", "public", "source_records.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    list(
      report_id = "comparison-watchlist-2026-04-20",
      candidate_ids = "a",
      topic_comparison = list()
    ),
    file.path(project_dir, "data", "public", "comparison_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  model <- build_candidate_policy_view_model(
    candidate_id = "a",
    topic_id = "seguridad-rural",
    from = "comparador",
    project_dir = project_dir
  )

  expect_length(model$documented_sections, 1)
  expect_equal(model$documented_sections[[1]]$topic_id, "seguridad-rural")
  expect_equal(model$topic_focus$root_topic_id, "seguridad-rural")
  expect_equal(model$topic_focus$state, "documented_only")
})

test_that("build_source_library_view_model separates promoted and pending manual sources", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "paloma-valencia", "paloma-valencia", "Paloma Valencia", "Juan Daniel Oviedo", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  jsonlite::write_json(
    list(
      list(
        source_id = "src-manual-1",
        candidate_id = "paloma-valencia",
        published_at = "2026-04-20T00:00:00Z",
        source_tier = "official",
        source_type = "article",
        source_name = "Paloma Valencia",
        url = "https://palomavalencia.com/2026/04/20/propuestas.html",
        title = "Propuestas",
        confidence = 0.95
      )
    ),
    file.path(project_dir, "data", "public", "source_records.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  jsonlite::write_json(
    list(
      list(
        entry_id = "manual-2",
        candidate_id = NULL,
        source_name = "Fuente General",
        source_tier = "reference",
        source_type = "webpage",
        url = "https://example.com/general",
        title = "General",
        published_at = NULL,
        status = "pending_classification",
        status_reason = "validated_by_http",
        candidate_confidence = 0.2,
        source_files = "data/added_manually/manual.md"
      )
    ),
    file.path(project_dir, "data", "public", "manual_source_library.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  model <- build_source_library_view_model(project_dir = project_dir)

  expect_equal(nrow(model$promoted_sources), 1)
  expect_equal(model$promoted_sources$library_status_label[[1]], "Integrada al sistema")
  expect_equal(nrow(model$pending_sources), 1)
  expect_equal(model$pending_sources$president_name[[1]], "Por clasificar")
  expect_equal(model$pending_sources$library_status_label[[1]], "Pendiente de clasificar")
})

test_that("build_source_library_view_model returns an empty state when public source files are absent", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "paloma-valencia", "paloma-valencia", "Paloma Valencia", "Juan Daniel Oviedo", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  model <- build_source_library_view_model(project_dir = project_dir)

  expect_equal(nrow(model$promoted_sources), 0)
  expect_equal(nrow(model$pending_sources), 0)
  expect_match(model$empty_state, "Todavía no hay fuentes públicas")
})
