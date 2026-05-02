write_manual_source_test_taxonomy <- function(project_dir) {
  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "economia", NA_character_, "Economía", "economia", "Marco fiscal, inflación, crecimiento, impuestos y gasto público.", TRUE, 1,
      "empleo-empresa", NA_character_, "Empleo y empresa", "empleo-empresa", "Trabajo, emprendimiento, formalización, salarios y regulación empresarial.", TRUE, 2,
      "salud", NA_character_, "Salud", "salud", "Sistema de salud", TRUE, 3,
      "educacion", NA_character_, "Educación", "educacion", "Primera infancia, colegios, educación superior, calidad y financiación.", TRUE, 4,
      "seguridad-justicia", NA_character_, "Seguridad y justicia", "seguridad-justicia", "Seguridad ciudadana, Fuerza Pública, justicia, cárceles y crimen organizado.", TRUE, 5,
      "pobreza-desigualdad", NA_character_, "Pobreza, desigualdad y protección social", "pobreza-desigualdad", "Transferencias, hambre, movilidad social, protección y redistribución.", TRUE, 6,
      "agro-rural", NA_character_, "Agro y ruralidad", "agro-rural", "Campo, tierras, seguridad alimentaria, vías rurales y desarrollo territorial.", TRUE, 7,
      "ambiente-energia", NA_character_, "Ambiente y energía", "ambiente-energia", "Transición energética, licencias, agua, clima y biodiversidad.", TRUE, 8,
      "vivienda-infraestructura", NA_character_, "Vivienda e infraestructura", "vivienda-infraestructura", "Vivienda, transporte, conectividad y obras públicas.", TRUE, 9,
      "derechos-genero", NA_character_, "Derechos, aborto y género", "derechos-genero", "Aborto, identidad de género, familia, libertades civiles e igualdad.", TRUE, 10,
      "instituciones-anticorrupcion", NA_character_, "Instituciones y anticorrupción", "instituciones-anticorrupcion", "Reforma política, transparencia, contratación, ramas del poder y controles.", TRUE, 11,
      "politica-internacional-paz", NA_character_, "Política internacional y paz", "politica-internacional-paz", "Relaciones exteriores, comercio, migración, defensa y paz total.", TRUE, 12,
      "vida-publica", NA_character_, "Vida pública y trayectoria", "vida-publica", "Trayectoria pública, redes de poder, controversias y antecedentes de interés público.", FALSE, 13
    ),
    file.path(project_dir, "config", "taxonomy_v1.csv")
  )
}

test_that("build_manual_source_registry extracts, normalizes, and aggregates manual URLs", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 1, TRUE, 1,
      "paloma-valencia", "paloma-valencia", "Paloma Valencia", "Juan Daniel Oviedo", 2, TRUE, 2
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  writeLines(
    c(
      "https://palomavalencia.com/propuestas.html",
      "https://palomavalencia.com/propuestas.html?utm_source=test",
      "https://example.com/general"
    ),
    file.path(project_dir, "data", "added_manually", "manual.md")
  )

  validator <- function(url) {
    if (grepl("example.com/general", url, fixed = TRUE)) {
      return(list(
        reachable = TRUE,
        final_url = url,
        http_status = 200L,
        title = "General source",
        published_at = as.POSIXct(NA, tz = "UTC"),
        validation_status = "reachable",
        status_reason = "validated_by_http"
      ))
    }

    list(
      reachable = TRUE,
      final_url = url,
      http_status = 200L,
      title = "Paloma Valencia Propuestas",
      published_at = as.POSIXct("2026-04-20 00:00:00", tz = "UTC"),
      validation_status = "reachable",
      status_reason = "validated_by_http"
    )
  }

  registry <- build_manual_source_registry(project_dir = project_dir, validator = validator)

  expect_equal(nrow(registry), 2)
  paloma_row <- registry |>
    dplyr::filter(.data$candidate_id == "paloma-valencia") |>
    dplyr::slice_head(n = 1)
  pending_row <- registry |>
    dplyr::filter(.data$title == "General source") |>
    dplyr::slice_head(n = 1)

  expect_equal(paloma_row$discovery_count[[1]], 2L)
  expect_equal(paloma_row$public_status[[1]], "promoted")
  expect_equal(pending_row$public_status[[1]], "pending_classification")
})

test_that("list_manual_source_files ignores directory readmes", {
  project_dir <- tempfile()
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)
  writeLines("https://example.com/a", file.path(project_dir, "data", "added_manually", "manual.md"))
  writeLines("https://example.com/b", file.path(project_dir, "data", "added_manually", "README.md"))

  files <- list_manual_source_files(project_dir)

  expect_equal(length(files), 1)
  expect_match(files[[1]], "manual.md")
})

test_that("manual_source_occurrences tolerates files without URLs or readable text", {
  project_dir <- tempfile()
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)
  writeLines("sin urls aqui", file.path(project_dir, "data", "added_manually", "notes.md"))
  writeBin(
    as.raw(c(0x25, 0x50, 0x44, 0x46, 0x2d, 0xff, 0x00, 0x61)),
    file.path(project_dir, "data", "added_manually", "sample.pdf")
  )

  occurrences <- manual_source_occurrences(project_dir)

  expect_s3_class(occurrences, "tbl_df")
  expect_equal(nrow(occurrences), 0)
  expect_true(all(c("source_file", "context_hint", "raw_url", "normalized_url") %in% names(occurrences)))
})

test_that("build_promoted_manual_sources and pending library split registry rows by public status", {
  registry <- tibble::tribble(
    ~entry_id, ~raw_url, ~normalized_url, ~final_url, ~source_files, ~discovery_count, ~context_hint, ~source_name, ~source_tier, ~source_type, ~candidate_id, ~candidate_confidence, ~candidate_match_method, ~title, ~published_at, ~validation_status, ~public_status, ~status_reason, ~http_status, ~reachable, ~discovery_method, ~processed_at,
    "manual-a", "https://example.com/a", "https://example.com/a", "https://example.com/a", "data/added_manually/a.md", 1L, "", "Fuente A", "official", "article", "ivan-cepeda", 0.95, "alias_score", "Fuente A", as.POSIXct("2026-04-20 00:00:00", tz = "UTC"), "reachable", "promoted", "validated_by_http", 200L, TRUE, "manual_added_file", as.POSIXct("2026-04-23 12:00:00", tz = "UTC"),
    "manual-b", "https://example.com/b", "https://example.com/b", "https://example.com/b", "data/added_manually/b.md", 1L, "", "Fuente B", "reference", "webpage", NA_character_, 0.2, "no_match", "Fuente B", as.POSIXct(NA, tz = "UTC"), "reachable", "pending_classification", "validated_by_http", 200L, TRUE, "manual_added_file", as.POSIXct("2026-04-23 12:00:00", tz = "UTC")
  )

  promoted <- build_promoted_manual_sources(registry, batch_label = "manual-curated")
  pending <- build_pending_manual_source_library(registry)

  expect_equal(nrow(promoted), 1)
  expect_equal(promoted$inbox_batch[[1]], "manual-curated")
  expect_equal(nrow(pending), 1)
  expect_equal(pending$status[[1]], "pending_classification")
})

test_that("manual source inference does not promote multi-candidate context by alias alone", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority,
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 1, TRUE, 1,
      "paloma-valencia", "paloma-valencia", "Paloma Valencia", "Juan Daniel Oviedo", 2, TRUE, 2,
      "claudia-lopez", "claudia-lopez", "Claudia López", "Leonardo Huerta", 3, TRUE, 3
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  writeLines(
    "Iván Cepeda Paloma Valencia Claudia López encuesta y contexto: https://example.com/2026/05/02/encuesta-presidencial.html",
    file.path(project_dir, "data", "added_manually", "daily.md")
  )

  validator <- function(url) {
    list(
      reachable = TRUE,
      final_url = url,
      http_status = 200L,
      title = "Encuesta presidencial",
      published_at = as.POSIXct("2026-05-02 00:00:00", tz = "UTC"),
      validation_status = "reachable",
      status_reason = "validated_by_http"
    )
  }

  registry <- build_manual_source_registry(project_dir = project_dir, validator = validator)

  expect_equal(nrow(registry), 1)
  expect_equal(registry$public_status[[1]], "pending_classification")
  expect_equal(registry$candidate_match_method[[1]], "ambiguous_multi_candidate_context")
})

test_that("manual source inference still promotes candidate-specific URLs", {
  candidates <- tibble::tribble(
    ~candidate_id, ~slug, ~president_name,
    "ivan-cepeda", "ivan-cepeda", "Iván Cepeda Castro",
    "paloma-valencia", "paloma-valencia", "Paloma Valencia"
  )

  guess <- infer_manual_source_candidate(
    normalized_url = "https://palomavalencia.com/propuestas.html",
    context_hint = "Iván Cepeda y Paloma Valencia aparecen en una revisión de fuentes.",
    candidates = candidates
  )

  expect_equal(guess$candidate_id, "paloma-valencia")
  expect_equal(guess$candidate_match_method, "alias_score")
})

test_that("run_pipeline promotes validated manual sources and publishes pending manual library", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "processed"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)

  write_manual_source_test_taxonomy(project_dir)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority, ~coalition, ~party_or_group,
      "paloma-valencia", "paloma-valencia", "Paloma Valencia", "Juan Daniel Oviedo", 1, TRUE, 1, "Coalición", "Partido",
      "ivan-cepeda", "ivan-cepeda", "Iván Cepeda Castro", "Aída Marina Quilcué Vivas", 2, TRUE, 2, "Coalición", "Partido"
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  writeLines(
    c(
      "https://palomavalencia.com/2026/04/20/propuestas-seguridad.html",
      "https://example.com/fuente-general"
    ),
    file.path(project_dir, "data", "added_manually", "manual.md")
  )

  validator <- function(url) {
    if (grepl("palomavalencia.com", url, fixed = TRUE)) {
      return(list(
        reachable = TRUE,
        final_url = url,
        http_status = 200L,
        title = "Propuestas de seguridad",
        published_at = as.POSIXct("2026-04-20 00:00:00", tz = "UTC"),
        validation_status = "reachable",
        status_reason = "validated_by_http"
      ))
    }

    list(
      reachable = TRUE,
      final_url = url,
      http_status = 200L,
      title = "Fuente general",
      published_at = as.POSIXct(NA, tz = "UTC"),
      validation_status = "reachable",
      status_reason = "validated_by_http"
    )
  }

  old_options <- options(manual_source_url_validator = validator)
  on.exit(options(old_options), add = TRUE)

  outputs <- run_pipeline(project_dir)

  expect_true(file.exists(file.path(project_dir, "data", "state", "manual_source_registry.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "processed", "manual_source_library.csv")))
  expect_true(file.exists(file.path(project_dir, "data", "public", "manual_source_library.json")))
  expect_true(any(outputs$sources$url == "https://palomavalencia.com/2026/04/20/propuestas-seguridad.html"))

  pending <- readr::read_csv(file.path(project_dir, "data", "processed", "manual_source_library.csv"), show_col_types = FALSE)
  expect_true(any(pending$url == "https://example.com/fuente-general"))
})

test_that("run_pipeline prefers inbox sources over duplicated promoted manual sources", {
  project_dir <- tempfile()
  dir.create(project_dir)
  dir.create(file.path(project_dir, "config"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "added_manually"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "inbox", "2026-04-23"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "processed"), recursive = TRUE)
  dir.create(file.path(project_dir, "data", "public"), recursive = TRUE)

  write_manual_source_test_taxonomy(project_dir)

  readr::write_csv(
    tibble::tribble(
      ~candidate_id, ~slug, ~president_name, ~vicepresident_name, ~ballot_position, ~watchlist_active, ~watchlist_priority, ~coalition, ~party_or_group,
      "paloma-valencia", "paloma-valencia", "Paloma Valencia", "Juan Daniel Oviedo", 1, TRUE, 1, "Coalición", "Partido"
    ),
    file.path(project_dir, "config", "candidate_registry.csv")
  )

  readr::write_csv(
    tibble::tribble(
      ~source_id, ~candidate_id, ~published_at, ~source_tier, ~source_type, ~source_name, ~url, ~title, ~quote_text, ~confidence,
      "src-existing", "paloma-valencia", "2026-04-20T00:00:00Z", "official", "article", "Paloma Valencia", "https://palomavalencia.com/propuestas", "Propuestas", "Texto base", 0.95
    ),
    file.path(project_dir, "data", "inbox", "2026-04-23", "sources.csv")
  )

  writeLines(
    "https://palomavalencia.com/propuestas",
    file.path(project_dir, "data", "added_manually", "manual.md")
  )

  validator <- function(url) {
    list(
      reachable = TRUE,
      final_url = url,
      http_status = 200L,
      title = "Propuestas",
      published_at = as.POSIXct("2026-04-20 00:00:00", tz = "UTC"),
      validation_status = "reachable",
      status_reason = "validated_by_http"
    )
  }

  old_options <- options(manual_source_url_validator = validator)
  on.exit(options(old_options), add = TRUE)

  outputs <- run_pipeline(project_dir)

  matching_sources <- outputs$sources |>
    dplyr::filter(.data$candidate_id == "paloma-valencia", .data$url == "https://palomavalencia.com/propuestas")

  expect_equal(nrow(matching_sources), 1)
  expect_equal(matching_sources$source_id[[1]], "src-existing")
})
