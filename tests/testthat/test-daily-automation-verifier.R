setup_daily_automation_fixture <- function(
    topic_id = "empleo-empresa",
    action = "incorporar",
    claim_type = "posicion_publica",
    claim_type_id = "postura_general",
    review_candidate_id = "ivan-cepeda",
    review_source_id = "src-1",
    review_url = "https://example.com/empleo",
    source_url = "https://example.com/empleo",
    include_structured_claims = TRUE,
    claim_evidence_excerpt = "Estatuto del trabajo con concertacion sindical.",
    duplicate_source_url = FALSE,
    extra_claim = FALSE,
    invalid_action = FALSE,
    blank_title = FALSE) {
  project_dir <- tempfile()
  dir.create(file.path(project_dir, "R"), recursive = TRUE)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "analysis", "daily_source_reviews"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-20", "source_texts"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "processed"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)
  dir.create(file.path(project_dir, "docs"), recursive = TRUE)
  file.copy(file.path(project_root, "R", "source_note_parsing.R"), file.path(project_dir, "R", "source_note_parsing.R"))

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda", "Aída Quilcué", 1, TRUE, 1,
      "claudia-lopez", "claudia-lopez", "Claudia López", "Sergio Torres", 2, TRUE, 2
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~review_date, ~source_id, ~candidate_id, ~source_name, ~published_at, ~title, ~url, ~theme, ~editorial_action, ~reason,
      "2026-04-20", review_source_id, review_candidate_id, "Medio", "2026-04-20", if (blank_title) "" else "Empleo", review_url, "mercado_laboral", if (invalid_action) "publicar" else action, "Fuente con curaduria suficiente."
    ),
    file.path(project_dir, "data", "analysis", "daily_source_reviews", "2026-04-20.csv")
  )
  writeLines(
    "# Fuentes evaluadas - 2026-04-20",
    file.path(project_dir, "data", "analysis", "daily_source_reviews", "2026-04-20.md")
  )
  writeLines(
    "<h1>Fuentes evaluadas - 2026-04-20</h1>",
    file.path(project_dir, "docs", "fuentes-evaluadas.html")
  )

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence,
    "src-1", "ivan-cepeda", "2026-04-20T11:00:00Z", "established-media", "article", "Medio", source_url, "Empleo", "Estatuto del trabajo", 0.9
  )
  if (duplicate_source_url) {
    sources <- dplyr::bind_rows(
      sources,
      tibble::tribble(
        ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence,
        "src-2", "ivan-cepeda", "2026-04-20T12:00:00Z", "established-media", "article", "Medio", source_url, "Empleo duplicado", "Estatuto del trabajo", 0.8
      )
    )
  }
  readr::write_csv(sources, file.path(project_dir, "data", "inbox", "2026-04-20", "sources.csv"))

  source_note <- c(
    if (include_structured_claims) c(
      "## Structured claims",
      "",
      "### Claim 1",
      "- `candidate_id`: ivan-cepeda",
      paste0("- `claim_type`: ", claim_type),
      "- `topic_id`: empleo-empresa",
      "- `subtopic_id`: mercado-laboral",
      "- `policy_key`: estatuto-trabajo-y-concertacion-sindical",
      "- `evidence_excerpt`: Estatuto del trabajo con concertacion sindical.",
      ""
    ),
    "## Source text or cleaned transcript",
    "",
    "Estatuto del trabajo con concertacion sindical."
  )
  writeLines(source_note, file.path(project_dir, "data", "inbox", "2026-04-20", "source_texts", "src-1.md"))

  claims <- tibble::tribble(
    ~claim_id, ~candidate_id, ~event_date, ~source_id, ~claim_type, ~claim_type_id, ~policy_key, ~topic_id, ~subtopic_id, ~summary_text, ~position_text, ~position_key, ~stance_value, ~implementation_detail, ~mechanism_text, ~target_population, ~problem_diagnosed, ~specificity_score, ~ambiguity_flag, ~insufficient_evidence_flag, ~possible_contradiction_flag, ~evidence_excerpt, ~inbox_batch, ~batch_date,
    "claim-1", "ivan-cepeda", "2026-04-20", "src-1", "policy_proposal", claim_type_id, "estatuto-trabajo-y-concertacion-sindical", topic_id, "mercado-laboral", "Concertacion laboral", "Concertacion laboral", "a_favor", 1, TRUE, "Dialogo social", "trabajadores", "precariedad laboral", 2, FALSE, FALSE, FALSE, claim_evidence_excerpt, "2026-04-20", "2026-04-20"
  )
  if (extra_claim) {
    claims <- dplyr::bind_rows(
      claims,
      tibble::tribble(
        ~claim_id, ~candidate_id, ~event_date, ~source_id, ~claim_type, ~claim_type_id, ~policy_key, ~topic_id, ~subtopic_id, ~summary_text, ~position_text, ~position_key, ~stance_value, ~implementation_detail, ~mechanism_text, ~target_population, ~problem_diagnosed, ~specificity_score, ~ambiguity_flag, ~insufficient_evidence_flag, ~possible_contradiction_flag, ~evidence_excerpt, ~inbox_batch, ~batch_date,
        "claim-extra", "ivan-cepeda", "2026-04-20", "src-1", "policy_proposal", "postura_general", "extra", "empleo-empresa", "mercado-laboral", "Extra", "Extra", "a_favor", 1, TRUE, "Extra", "trabajadores", "precariedad", 1, FALSE, FALSE, FALSE, "Extra", "2026-04-20", "2026-04-20"
      )
    )
  }
  readr::write_csv(claims, file.path(project_dir, "data", "processed", "claim_records.csv"))

  jsonlite::write_json(
    list(status = "pass", summary = "ok"),
    file.path(project_dir, "data", "public", "validation_report.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  project_dir
}

run_daily_verifier_fixture <- function(project_dir) {
  script_path <- file.path(project_root, "scripts", "verify_daily_automation.R")
  output <- suppressWarnings(system2(
    "Rscript",
    c(shQuote(script_path), paste0("--project-dir=", project_dir), "--date=2026-04-20"),
    stdout = TRUE,
    stderr = TRUE
  ))
  list(output = output, status = attr(output, "status") %||% 0L)
}

daily_check_status <- function(project_dir, check_id) {
  report <- jsonlite::fromJSON(file.path(project_dir, "data", "automation", "run_reports", "2026-04-20.json"))
  report$checks$status[report$checks$check_id == check_id]
}

test_that("verify_daily_automation reconciles incorporated structured claims", {
  project_dir <- setup_daily_automation_fixture()

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 0L)
  report <- jsonlite::fromJSON(file.path(project_dir, "data", "automation", "run_reports", "2026-04-20.json"))
  expect_equal(report$status, "pass")
})

test_that("verify_daily_automation accepts pipe-delimited review candidates", {
  project_dir <- setup_daily_automation_fixture(
    action = "reservar",
    review_candidate_id = "ivan-cepeda|claudia-lopez",
    review_source_id = NA_character_,
    review_url = "https://example.com/no-inbox-source"
  )

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 0L)
  expect_equal(daily_check_status(project_dir, "daily_source_review_contract"), "pass")
})

test_that("verify_daily_automation blocks invalid review contract rows", {
  project_dir <- setup_daily_automation_fixture(invalid_action = TRUE)

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 1L)
  expect_equal(daily_check_status(project_dir, "daily_source_review_contract"), "block")
})

test_that("verify_daily_automation blocks incorporated sources missing inbox source_id", {
  project_dir <- setup_daily_automation_fixture(review_source_id = "src-missing")

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 1L)
  expect_equal(daily_check_status(project_dir, "daily_source_review_claim_reconciliation"), "block")
})

test_that("verify_daily_automation blocks ambiguous URL fallback matches", {
  project_dir <- setup_daily_automation_fixture(review_source_id = NA_character_, duplicate_source_url = TRUE)

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 1L)
  expect_equal(daily_check_status(project_dir, "daily_source_review_claim_reconciliation"), "block")
})

test_that("verify_daily_automation blocks non-incorporated review rows that create claims", {
  project_dir <- setup_daily_automation_fixture(action = "reservar")

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 1L)
  expect_equal(daily_check_status(project_dir, "daily_source_review_claim_reconciliation"), "block")
})

test_that("verify_daily_automation blocks curated source topic mismatches", {
  project_dir <- setup_daily_automation_fixture(topic_id = "seguridad-justicia")

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 1L)
  expect_equal(daily_check_status(project_dir, "daily_source_review_claim_reconciliation"), "block")
})

test_that("verify_daily_automation blocks invalid curated claim types", {
  project_dir <- setup_daily_automation_fixture(claim_type = "tipo_no_contractual", claim_type_id = "dato_contextual")

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 1L)
  expect_equal(daily_check_status(project_dir, "daily_source_review_claim_reconciliation"), "block")
})

test_that("verify_daily_automation blocks incorporated source notes without structured claims", {
  project_dir <- setup_daily_automation_fixture(include_structured_claims = FALSE)

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 1L)
  expect_equal(daily_check_status(project_dir, "daily_source_review_claim_reconciliation"), "block")
})

test_that("verify_daily_automation blocks evidence excerpt drift", {
  project_dir <- setup_daily_automation_fixture(claim_evidence_excerpt = "Otro fragmento.")

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 1L)
  expect_equal(daily_check_status(project_dir, "daily_source_review_claim_reconciliation"), "block")
})

test_that("verify_daily_automation blocks extra claims under incorporated source_id", {
  project_dir <- setup_daily_automation_fixture(extra_claim = TRUE)

  result <- run_daily_verifier_fixture(project_dir)

  expect_equal(result$status, 1L)
  expect_equal(daily_check_status(project_dir, "daily_source_review_claim_reconciliation"), "block")
})
