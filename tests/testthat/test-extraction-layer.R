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

test_that("materialize_extraction_results can derive structured claims from source notes without claims.csv", {
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
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "sergio-fajardo", "sergio-fajardo", "Sergio Fajardo", "Edna Bonilla", 13, TRUE, 5
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence, ~inbox_batch,
    "src-1", "sergio-fajardo", as.POSIXct("2026-04-20 10:00:00", tz = "UTC"), "official", "program", "Programa", "https://example.com/a", "Titulo", "Cita breve", 0.95, "2026-04-20"
  )

  note_path <- file.path(project_dir, "src-1.md")
  writeLines(
    c(
      "- source_id: src-1",
      "- candidate_hint: sergio-fajardo",
      "",
      "## Structured claims",
      "",
      "### Claim 1",
      "- claim_type: propuesta_concreta",
      "- topic_id: salud",
      "- policy_key: ordenar-sistema-salud",
      "- summary_text: Reorganizar el sistema de salud.",
      "- position_text: Reorganizar el sistema de salud con auditoria fuerte.",
      "- mechanism_text: Auditoria fuerte",
      "- target_population: usuarios del sistema",
      "- problem_diagnosed: desorden institucional",
      "- stance_value: 1",
      "- specificity_score: 2",
      "- ambiguity_flag: false",
      "- insufficient_evidence_flag: false",
      "- possible_contradiction_flag: false",
      "- evidence_excerpt: Reorganizar el sistema de salud con auditoria fuerte.",
      "",
      "## Source text or cleaned transcript",
      "",
      "Reorganizar el sistema de salud con auditoria fuerte."
    ),
    note_path
  )

  source_text_files <- tibble::tibble(
    batch_date = as.Date("2026-04-20"),
    source_id = "src-1",
    path = note_path
  )

  source_packets <- build_source_packets(sources, source_text_files)
  extraction_results <- materialize_extraction_results(project_dir, source_packets = source_packets)
  flattened <- flatten_extraction_claims(extraction_results)

  expect_equal(length(extraction_results), 1)
  expect_equal(flattened$claim_type_id[[1]], "propuesta_concreta")
  expect_equal(flattened$policy_key[[1]], "ordenar-sistema-salud")
  expect_equal(flattened$mechanism_text[[1]], "Auditoria fuerte")
})

test_that("structured source notes accept markdown key formatting", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  ensure_contract_layout(project_dir)

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "empleo-empresa", NA_character_, "Empleo y empresa", "empleo-empresa", "Trabajo y empresa", TRUE, 1,
      "mercado-laboral", "empleo-empresa", "Mercado laboral", "mercado-laboral", "Trabajo formal", TRUE, 2
    ),
    file.path(project_dir, "config", "taxonomy_v1.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda", "Aída Quilcué", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence, ~inbox_batch,
    "src-1", NA_character_, as.POSIXct("2026-04-20 10:00:00", tz = "UTC"), "official", "program", "Programa", "https://example.com/a", "Empleo", "Estatuto del trabajo", 0.95, "2026-04-20"
  )

  note_path <- file.path(project_dir, "src-1.md")
  writeLines(
    c(
      "- `source_id`: src-1",
      "- `candidate_hint`: ivan-cepeda",
      "",
      "## Structured claims",
      "",
      "### Claim 1",
      "- `candidate_id`: ivan-cepeda",
      "- `claim_type`: posicion_publica",
      "- `topic_id`: empleo-empresa",
      "- `subtopic_id`: mercado-laboral",
      "- `policy_key`: estatuto-trabajo-y-concertacion-sindical",
      "esta linea no es clave valor",
      "- `summary_text`: Impulsar concertacion laboral.",
      "- `position_text`: Impulsar estatuto del trabajo con concertacion sindical.",
      "- `mechanism_text`: Concertacion sindical",
      "- `target_population`: trabajadores",
      "- `problem_diagnosed`: informalidad laboral",
      "- `evidence_excerpt`: Impulsar estatuto del trabajo con concertacion sindical.",
      "",
      "## Source text or cleaned transcript",
      "",
      "Impulsar estatuto del trabajo con concertacion sindical."
    ),
    note_path
  )

  source_text_files <- tibble::tibble(
    batch_date = as.Date("2026-04-20"),
    source_id = "src-1",
    path = note_path
  )

  source_packets <- build_source_packets(sources, source_text_files)
  extraction_results <- materialize_extraction_results(project_dir, source_packets = source_packets)
  flattened <- flatten_extraction_claims(extraction_results)

  expect_equal(source_packets[[1]]$capture_method, "source_text_file")
  expect_true("ivan-cepeda" %in% source_packets[[1]]$candidate_hints)
  expect_equal(flattened$candidate_id[[1]], "ivan-cepeda")
  expect_equal(flattened$claim_type_id[[1]], "postura_general")
  expect_equal(flattened$topic_id[[1]], "empleo-empresa")
  expect_equal(flattened$subtopic_id[[1]], "mercado-laboral")
  expect_equal(flattened$policy_key[[1]], "estatuto-trabajo-y-concertacion-sindical")
  expect_equal(flattened$mechanism_text[[1]], "Concertacion sindical")
  expect_equal(flattened$target_population[[1]], "trabajadores")
  expect_equal(flattened$problem_diagnosed[[1]], "informalidad laboral")
  expect_equal(flattened$evidence_excerpt[[1]], "Impulsar estatuto del trabajo con concertacion sindical.")
})

test_that("structured source notes flag invalid candidate, topic, and subtopic metadata", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  ensure_contract_layout(project_dir)

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "empleo-empresa", NA_character_, "Empleo y empresa", "empleo-empresa", "Trabajo y empresa", TRUE, 1,
      "mercado-laboral", "empleo-empresa", "Mercado laboral", "mercado-laboral", "Trabajo formal", TRUE, 2
    ),
    file.path(project_dir, "config", "taxonomy_v1.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda", "Aída Quilcué", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  sources <- tibble::tribble(
    ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence, ~inbox_batch,
    "src-1", NA_character_, as.POSIXct("2026-04-20 10:00:00", tz = "UTC"), "official", "program", "Programa", "https://example.com/a", "Empleo", "Estatuto del trabajo", 0.95, "2026-04-20"
  )

  note_path <- file.path(project_dir, "src-1.md")
  writeLines(
    c(
      "- `source_id`: src-1",
      "",
      "## Structured claims",
      "",
      "### Claim 1",
      "- `candidate_id`: candidato-inexistente",
      "- `claim_type`: propuesta_concreta",
      "- `topic_id`: tema-inexistente",
      "- `subtopic_id`: mercado-laboral",
      "- `policy_key`: estatuto-trabajo-y-concertacion-sindical",
      "- `summary_text`: Impulsar concertacion laboral.",
      "- `position_text`: Impulsar estatuto del trabajo con concertacion sindical.",
      "- `evidence_excerpt`: Impulsar estatuto del trabajo con concertacion sindical.",
      "",
      "## Source text or cleaned transcript",
      "",
      "Impulsar estatuto del trabajo con concertacion sindical."
    ),
    note_path
  )

  source_text_files <- tibble::tibble(
    batch_date = as.Date("2026-04-20"),
    source_id = "src-1",
    path = note_path
  )

  source_packets <- build_source_packets(sources, source_text_files)
  extraction_results <- materialize_extraction_results(project_dir, source_packets = source_packets)
  flattened <- flatten_extraction_claims(extraction_results)

  expect_true(is.na(flattened$candidate_id[[1]]))
  expect_true(is.na(flattened$topic_id[[1]]))
  expect_true(is.na(flattened$subtopic_id[[1]]))
  expect_true(flattened$insufficient_evidence_flag[[1]])
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
  expect_equal(outputs$site_metadata$pipeline_mode[[1]], "structured_extraction_auto")
})
