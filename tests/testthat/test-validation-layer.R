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
  expect_equal(outputs$validation_status$status[[1]], "pass")
})

test_that("build_validation_report blocks missing traceability in analytical artifacts", {
  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~claim_type_id,
    "claim-1", "ivan-cepeda", "propuesta_concreta"
  )

  candidate_analysis <- list(
    list(
      candidate_id = "ivan-cepeda",
      source_ids = character(),
      claim_ids = "claim-1",
      ideology_axes = list(),
      thematic_analysis = list(),
      uncertainties = "Evidencia insuficiente."
    )
  )

  comparison_report <- list(
    report_id = "comparison-watchlist-2026-04-20",
    candidate_ids = c("ivan-cepeda"),
    source_ids = character(),
    claim_ids = "claim-1",
    axes_comparison = list(
      list(axis_id = "estado_vs_mercado", candidate_positions = list(list(candidate_id = "ivan-cepeda", placement = "Evidencia insuficiente", confidence = 0.1)), summary = "Sin evidencia.")
    ),
    topic_comparison = list(),
    convergences = character(),
    divergences = character(),
    uncertainties = "Sin evidencia suficiente."
  )

  editorial_packages <- list(
    list(
      artifact_id = "candidate-profile-ivan",
      artifact_type = "candidate_profile",
      title = "Perfil",
      candidate_ids = c("ivan-cepeda"),
      source_ids = "src-1",
      claim_ids = character(),
      sections = list(
        list(section_id = "profile_overview", heading = "Perfil", content_type = "description", body = "Texto"),
        list(section_id = "political_philosophy", heading = "Filosofía", content_type = "inference", body = "Texto"),
        list(section_id = "internal_coherence", heading = "Coherencia", content_type = "evaluation", body = "Texto")
      )
    )
  )

  report <- build_validation_report(
    claims = claims,
    candidate_analysis = candidate_analysis,
    comparison_report = comparison_report,
    editorial_packages = editorial_packages,
    report_date = as.Date("2026-04-20"),
    project_dir = "."
  )

  expect_equal(report$status, "block")
  expect_true(any(vapply(report$checks, \(check) identical(check$rule_id, "traceability_required") && identical(check$status, "fail"), logical(1))))
})

test_that("build_validation_report blocks scaffold-only inbox batches without explicit no_findings status", {
  project_dir <- tempfile()
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-21", "source_texts"), recursive = TRUE)
  readr::write_csv(
    tibble::tibble(
      source_id = character(),
      candidate_id = character(),
      published_at = character(),
      source_tier = character(),
      source_type = character(),
      source_name = character(),
      url = character(),
      title = character(),
      quote_text = character(),
      confidence = numeric()
    ),
    file.path(project_dir, "data", "inbox", "2026-04-21", "sources.csv")
  )

  report <- build_validation_report(
    claims = tibble::tibble(),
    candidate_analysis = list(),
    comparison_report = NULL,
    editorial_packages = list(),
    report_date = as.Date("2026-04-21"),
    project_dir = project_dir
  )

  expect_equal(report$status, "block")
  expect_true(any(vapply(report$checks, \(check) identical(check$rule_id, "empty_inbox_batches_require_resolution") && identical(check$status, "fail"), logical(1))))
})

test_that("build_validation_report allows empty inbox batches with explicit no_findings status", {
  project_dir <- tempfile()
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-21", "source_texts"), recursive = TRUE)
  readr::write_csv(
    tibble::tibble(
      source_id = character(),
      candidate_id = character(),
      published_at = character(),
      source_tier = character(),
      source_type = character(),
      source_name = character(),
      url = character(),
      title = character(),
      quote_text = character(),
      confidence = numeric()
    ),
    file.path(project_dir, "data", "inbox", "2026-04-21", "sources.csv")
  )
  writeLines(
    '{"status":"no_findings","updated_at":"2026-04-21T10:30:00Z","summary":"No hubo hallazgos publicables en la ventana revisada.","notes":"Revisadas fuentes de las ultimas 24 horas.","checked_window_start":"2026-04-20T10:30:00Z","checked_window_end":"2026-04-21T10:30:00Z"}',
    file.path(project_dir, "data", "inbox", "2026-04-21", "batch_status.json")
  )

  report <- build_validation_report(
    claims = tibble::tibble(),
    candidate_analysis = list(),
    comparison_report = NULL,
    editorial_packages = list(),
    report_date = as.Date("2026-04-21"),
    project_dir = project_dir
  )

  expect_equal(report$status, "pass")
  expect_true(any(vapply(report$checks, \(check) identical(check$rule_id, "empty_inbox_batches_require_resolution") && identical(check$status, "pass"), logical(1))))
})
