args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[[1]], winslash = "/", mustWork = FALSE) else "."

old_wd <- setwd(project_dir)
on.exit(setwd(old_wd), add = TRUE)

r_files <- list.files("R", pattern = "[.][Rr]$", full.names = TRUE)
purrr::walk(r_files, ~ source(.x, local = FALSE))

quarto_render_outputs_complete <- function(project_dir = ".") {
  candidates <- read_candidate_registry_public(project_dir)
  candidate_slugs <- dplyr::coalesce(candidates$slug, candidates$candidate_id)

  expected_paths <- c(
    "index.html",
    "comparador.html",
    "cronologia.html",
    "fuentes.html",
    "fuentes-evaluadas.html",
    "metodologia.html",
    "search.json",
    file.path("candidatos", "index.html"),
    file.path("candidatos", paste0(candidate_slugs, ".html"))
  )

  all(file.exists(file.path(project_dir, "docs", expected_paths)))
}

cleanup_quarto_render_residue <- function(project_dir = ".") {
  candidates <- read_candidate_registry_public(project_dir)
  candidate_slugs <- dplyr::coalesce(candidates$slug, candidates$candidate_id)

  stray_paths <- file.path(
    project_dir,
    c(
      "comparador.html",
      "cronologia.html",
      "fuentes.html",
      "fuentes-evaluadas.html",
      "index.html",
      "metodologia.html",
      "search.json",
      "site_libs",
      file.path("candidatos", "index.html"),
      file.path("candidatos", paste0(candidate_slugs, ".html"))
    )
  )

  unlink(unique(stray_paths[file.exists(stray_paths)]), recursive = TRUE, force = TRUE)

  deleted_solution_files <- suppressWarnings(system2(
    "git",
    c("diff", "--name-only", "--diff-filter=D", "--", "docs/solutions"),
    stdout = TRUE,
    stderr = TRUE
  ))

  if (length(deleted_solution_files) > 0 && any(nzchar(trimws(deleted_solution_files)))) {
    suppressWarnings(system2(
      "git",
      c("restore", "--source=HEAD", "--worktree", "--", "docs/solutions"),
      stdout = TRUE,
      stderr = TRUE
    ))
  }
}

pipeline_outputs <- run_pipeline(project_dir = ".")
if (identical(pipeline_outputs$validation_report$status %||% "block", "block")) {
  stop(
    "Validation blocked publication. Review ",
    file.path("data", "public", "validation_report.json"),
    " before rendering the public site."
  )
}

invisible(generate_candidate_pages(project_dir = "."))

quarto_home <- file.path(tempdir(), "quarto-home")
dir.create(file.path(quarto_home, "Library", "Caches", "quarto"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(quarto_home, "Library", "Application Support"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(quarto_home, ".cache"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(quarto_home, ".config"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(quarto_home, ".local", "share"), recursive = TRUE, showWarnings = FALSE)

quarto_env <- c(
  paste0("HOME=", quarto_home),
  paste0("XDG_CACHE_HOME=", file.path(quarto_home, ".cache")),
  paste0("XDG_CONFIG_HOME=", file.path(quarto_home, ".config")),
  paste0("XDG_DATA_HOME=", file.path(quarto_home, ".local", "share")),
  paste0("R_LIBS=", paste(.libPaths(), collapse = .Platform$path.sep))
)

render_output <- suppressWarnings(system2(
  "quarto",
  "render",
  env = quarto_env,
  stdout = TRUE,
  stderr = TRUE
))

status <- attr(render_output, "status") %||% 0L

rename_notfound_bug <- status != 0L &&
  grepl(
    "ERROR: NotFound: No such file or directory \\(os error 2\\): rename '.*candidatos/.+\\.html' -> '.*docs/candidatos/.+\\.html'",
    paste(render_output, collapse = "\n"),
    perl = TRUE
  )

if (!identical(status, 0L)) {
  if (rename_notfound_bug && quarto_render_outputs_complete(project_dir = ".")) {
    cleanup_quarto_render_residue(project_dir = ".")
    warning(
      "Quarto returned a duplicate candidate-page rename error after generating the expected docs output; continuing with cleaned render artifacts."
    )
  } else {
    stop("Quarto render failed with exit status ", status)
  }
} else {
  cleanup_quarto_render_residue(project_dir = ".")
}

publish_program_document_files(project_dir = ".", program_documents = pipeline_outputs$program_documents)

message("Pipeline y sitio renderizados correctamente.")
