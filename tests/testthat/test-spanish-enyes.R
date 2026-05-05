test_that("public Spanish text preserves words with enye", {
  strip_non_visible <- function(text) {
    text |>
      stringr::str_replace_all("https?://[^[:space:]<>\"')\\]]+", " ") |>
      stringr::str_replace_all("(?is)<script[^>]*>.*?</script>", " ") |>
      stringr::str_replace_all("(?is)<style[^>]*>.*?</style>", " ") |>
      stringr::str_replace_all("<[^>]+>", " ")
  }

  source_files <- system2(
    "git",
    c(
      "ls-files",
      "config",
      "data/analysis",
      "data/inbox",
      "data/program_documents",
      "data/public"
    ),
    stdout = TRUE
  )
  source_files <- source_files[
    stringr::str_detect(source_files, stringr::regex("[.](csv|json|md|ya?ml|txt)$", ignore_case = TRUE))
  ]

  docs_files <- list.files(
    file.path(project_root, "docs"),
    pattern = "[.](html|json|md)$",
    recursive = TRUE,
    full.names = TRUE
  )
  docs_files <- docs_files[
    !stringr::str_detect(docs_files, "/site_libs/|/search[.]json$")
  ]

  files <- unique(c(file.path(project_root, source_files), docs_files))
  files <- files[file.exists(files)]

  forbidden <- paste(
    c(
      "campana", "campanas",
      "nino", "ninos", "nina", "ninas",
      "acompanada", "acompanado", "acompanamiento", "acompanar",
      "senal", "senales",
      "ano", "anos",
      "narino"
    ),
    collapse = "|"
  )
  forbidden_pattern <- stringr::regex(
    paste0("(?<![\\p{L}\\p{N}_])(", forbidden, ")(?![\\p{L}\\p{N}_])"),
    ignore_case = TRUE
  )

  hits <- purrr::map_dfr(files, function(file) {
    text <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    visible_text <- strip_non_visible(text)
    matches <- stringr::str_extract_all(visible_text, forbidden_pattern)[[1]]

    if (length(matches) == 0) {
      return(tibble::tibble())
    }

    tibble::tibble(
      file = gsub(paste0("^", stringr::fixed(project_root), "/"), "", file),
      token = unique(matches)
    )
  })
  if (nrow(hits) == 0) {
    hits <- tibble::tibble(file = character(), token = character())
  }

  expect_equal(hits, tibble::tibble(file = character(), token = character()))
})
