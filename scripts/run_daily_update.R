args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[[1]], winslash = "/", mustWork = FALSE) else "."

old_wd <- setwd(project_dir)
on.exit(setwd(old_wd), add = TRUE)

r_files <- list.files("R", pattern = "[.][Rr]$", full.names = TRUE)
purrr::walk(r_files, ~ source(.x, local = FALSE))

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

status <- system2("quarto", "render", env = quarto_env)

if (!identical(status, 0L)) {
  stop("Quarto render failed with exit status ", status)
}

message("Pipeline y sitio renderizados correctamente.")
