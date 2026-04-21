test_that("program document registry synthesizes official program sources", {
  project_dir <- tempfile()
  dir.create(project_dir)
  ensure_contract_layout(project_dir)

  candidate_dir <- file.path(project_dir, "data", "program_documents", "files", "ivan-cepeda")
  dir.create(candidate_dir, recursive = TRUE)
  markdown_path <- file.path(candidate_dir, "plan-ivan.md")
  writeLines("Programa oficial en markdown", markdown_path)

  readr::write_csv(
    tibble::tribble(
      ~document_id, ~source_id, ~candidate_id, ~document_role, ~is_primary, ~official_page_url, ~download_url, ~source_name, ~title, ~published_at, ~discovery_method, ~download_status, ~conversion_status, ~pdf_path, ~markdown_path, ~notes,
      "plan-ivan", "src-programa-ivan", "ivan-cepeda", "programa-base", TRUE, "https://example.com/programa", "https://example.com/programa.pdf", "Campaña Iván Cepeda", "Programa oficial", "2026-04-20T11:00:00Z", "manual_curated", "downloaded", "converted", "data/program_documents/files/ivan-cepeda/plan-ivan.pdf", "data/program_documents/files/ivan-cepeda/plan-ivan.md", "Documento base"
    ),
    program_document_registry_path(project_dir)
  )

  program_documents <- load_program_documents(project_dir)
  sources <- build_program_document_sources(program_documents)
  text_files <- list_program_document_text_files(project_dir, program_documents)

  expect_equal(nrow(program_documents), 1)
  expect_equal(sources$source_type[[1]], "program")
  expect_equal(sources$source_tier[[1]], "official")
  expect_equal(text_files$source_id[[1]], "src-programa-ivan")
})

test_that("run_pipeline publishes program document outputs", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
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

  candidate_dir <- file.path(project_dir, "data", "program_documents", "files", "ivan-cepeda")
  dir.create(candidate_dir, recursive = TRUE)
  markdown_path <- file.path(candidate_dir, "plan-ivan.md")
  writeLines(
    c(
      "- source_id: src-programa-ivan",
      "- candidate_hint: ivan-cepeda",
      "",
      "## Structured claims",
      "",
      "### Claim 1",
      "- claim_type: propuesta_concreta",
      "- topic_id: salud",
      "- policy_key: salud-preventiva",
      "- summary_text: Fortalecer atención primaria.",
      "- position_text: Fortalecer atención primaria con foco territorial.",
      "- stance_value: 1",
      "- specificity_score: 2",
      "- ambiguity_flag: false",
      "- insufficient_evidence_flag: false",
      "- possible_contradiction_flag: false",
      "- evidence_excerpt: Fortalecer atención primaria con foco territorial.",
      "",
      "## Source text or cleaned transcript",
      "",
      "Fortalecer atención primaria con foco territorial."
    ),
    markdown_path
  )

  readr::write_csv(
    tibble::tribble(
      ~document_id, ~source_id, ~candidate_id, ~document_role, ~is_primary, ~official_page_url, ~download_url, ~source_name, ~title, ~published_at, ~discovery_method, ~download_status, ~conversion_status, ~pdf_path, ~markdown_path, ~notes,
      "plan-ivan", "src-programa-ivan", "ivan-cepeda", "programa-base", TRUE, "https://example.com/programa", "https://example.com/programa.pdf", "Campaña Iván Cepeda", "Programa oficial", "2026-04-20T11:00:00Z", "manual_curated", "downloaded", "converted", "", "data/program_documents/files/ivan-cepeda/plan-ivan.md", "Documento base"
    ),
    program_document_registry_path(project_dir)
  )

  outputs <- run_pipeline(project_dir)

  expect_true(file.exists(file.path(project_dir, "data", "processed", "program_documents.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "program_documents.json")))
  expect_true(outputs$program_documents$coverage_status[[1]] %in% c("analysis_ready", "comparison_ready"))
})

test_that("validation blocks when initialized watchlist corpus misses primary markdown", {
  candidates <- tibble::tribble(
    ~candidate_id, ~watchlist_active,
    "ivan-cepeda", TRUE,
    "paloma-valencia", TRUE
  )

  program_documents <- tibble::tribble(
    ~document_id, ~source_id, ~candidate_id, ~document_role, ~is_primary, ~official_page_url, ~download_url, ~source_name, ~title, ~published_at, ~discovery_method, ~download_status, ~conversion_status, ~pdf_path, ~markdown_path, ~notes,
    "plan-ivan", "src-programa-ivan", "ivan-cepeda", "programa-base", TRUE, "https://example.com/ivan", "https://example.com/ivan.pdf", "Campaña Iván Cepeda", "Programa Iván", as.POSIXct("2026-04-20 11:00:00", tz = "UTC"), "manual_curated", "downloaded", "converted", "", "missing-file.md", ""
  )

  report <- build_validation_report(
    claims = tibble::tibble(),
    candidate_analysis = list(),
    comparison_report = NULL,
    editorial_packages = list(),
    program_documents = program_documents,
    candidates = candidates,
    report_date = as.Date("2026-04-20"),
    project_dir = tempdir()
  )

  expect_equal(report$status, "block")
  expect_true(any(vapply(report$checks, \(check) identical(check$rule_id, "primary_program_documents_registered") && identical(check$status, "fail"), logical(1))))
})
