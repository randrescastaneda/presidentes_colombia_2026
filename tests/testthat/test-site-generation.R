test_that("generate_candidate_pages creates one qmd per candidate", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "candidatos"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 1, TRUE, 1,
      "paloma-valencia", "paloma-valencia", "Paloma Valencia", "Juan Daniel Oviedo", 12, TRUE, 3
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  paths <- generate_candidate_pages(project_dir)

  expect_length(paths, 2)
  expect_true(all(file.exists(paths)))
  expect_match(readLines(paths[[1]], warn = FALSE)[1], "^---$")
})

test_that("site helpers can read candidate registry without hidden pipeline dependencies", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 1, TRUE, 1
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  isolated_env <- new.env(parent = baseenv())
  sys.source(file.path(project_root, "R", "site_helpers.R"), envir = isolated_env)

  expect_no_error(isolated_env$read_candidate_registry_public(project_dir))
})

test_that("index page marks HTML-emitting chunks as asis", {
  index_lines <- readLines(file.path(project_root, "index.qmd"), warn = FALSE)

  expect_gte(sum(index_lines == "#| results: asis"), 3)
})

test_that("generated candidate pages mark output chunks as asis", {
  template <- generate_candidate_page_template(
    tibble::tibble(
      candidate_id = "ivan-cepeda",
      slug = "ivan-cepeda",
      president_name = "Iván Cepeda Castro",
      vicepresident_name = "Aída Marina Quilcué Vivas",
      ballot_position = 1,
      watchlist_active = TRUE,
      watchlist_priority = 1
    )
  )

  expect_gte(stringr::str_count(template, stringr::fixed("#| results: asis")), 5)
})

test_that("generated candidate pages favor narrative sections over tables", {
  template <- generate_candidate_page_template(
    tibble::tibble(
      candidate_id = "ivan-cepeda",
      slug = "ivan-cepeda",
      president_name = "Iván Cepeda Castro",
      vicepresident_name = "Aída Marina Quilcué Vivas",
      ballot_position = 1,
      watchlist_active = TRUE,
      watchlist_priority = 1
    )
  )

  expect_match(template, "## Ubicación ideológica")
  expect_match(template, "## Propuestas y posiciones públicas")
  expect_no_match(template, "safe_kable")
})

test_that("comparison page is no longer driven by a single output table", {
  comparison_lines <- readLines(file.path(project_root, "comparador.qmd"), warn = FALSE)
  comparison_text <- paste(comparison_lines, collapse = "\n")

  expect_no_match(comparison_text, "safe_kable")
})

test_that("quarto freeze is disabled for daily data-driven rendering", {
  quarto_config <- readLines(file.path(project_root, "_quarto.yml"), warn = FALSE)
  quarto_text <- paste(quarto_config, collapse = "\n")

  expect_no_match(quarto_text, "freeze:\\s*auto")
})

test_that("claim narrative links to the claim's own source", {
  claim_row <- tibble::tibble(
    source_id = "src-2",
    event_date = as.Date("2026-04-10"),
    topic_label = "Fiscal",
    root_label = "Economía",
    position_text = "Reducir impuestos de renta",
    summary_text = "La campaña lo presenta como alivio tributario"
  )

  sources <- tibble::tribble(
    ~source_id, ~source_name, ~url,
    "src-1", "Fuente A", "https://example.com/a",
    "src-2", "Fuente B", "https://example.com/b"
  )

  paragraph <- claim_paragraph_html(claim_row, sources)

  expect_match(paragraph, "Fuente B")
  expect_match(paragraph, "example.com/b")
  expect_no_match(paragraph, "Fuente A")
})
