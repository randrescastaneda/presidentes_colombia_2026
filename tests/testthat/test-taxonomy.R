test_that("load_taxonomy computes hierarchy levels and supports new subcategories", {
  taxonomy_path <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "economia", NA_character_, "Economía", "economia", "Tema macroeconómico", TRUE, 1,
      "fiscal", "economia", "Fiscal", "fiscal", "Ingresos y gasto", TRUE, 2,
      "politica-internacional", NA_character_, "Política internacional y paz", "politica-internacional", "Temas exteriores", TRUE, 11,
      "migracion", "politica-internacional", "Migración", "migracion", "Movilidad humana", FALSE, 12
    ),
    taxonomy_path
  )

  taxonomy <- load_taxonomy(taxonomy_path)

  expect_true(all(c("topic_id", "parent_topic_id", "label_public", "slug", "level") %in% names(taxonomy)))
  expect_equal(taxonomy |> filter(topic_id == "economia") |> pull(level), 1)
  expect_equal(taxonomy |> filter(topic_id == "fiscal") |> pull(level), 2)
  expect_equal(taxonomy |> filter(topic_id == "migracion") |> pull(level), 2)
  expect_equal(taxonomy |> filter(topic_id == "migracion") |> pull(parent_topic_id), "politica-internacional")
  expect_equal(nrow(taxonomy), 4)
})

test_that("load_taxonomy rejects duplicated slugs and missing parents", {
  taxonomy_path <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tribble(
      ~topic_id, ~parent_topic_id, ~label_public, ~slug, ~description, ~is_core, ~sort_order,
      "economia", NA_character_, "Economía", "economia", "Tema macroeconómico", TRUE, 1,
      "fiscal", "no-existe", "Fiscal", "economia", "Ingresos y gasto", TRUE, 2
    ),
    taxonomy_path
  )

  expect_error(load_taxonomy(taxonomy_path), "parent_topic_id|slug")
})
