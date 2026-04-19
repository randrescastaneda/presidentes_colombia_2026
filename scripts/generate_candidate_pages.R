args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[[1]], winslash = "/", mustWork = FALSE) else "."

old_wd <- setwd(project_dir)
on.exit(setwd(old_wd), add = TRUE)

r_files <- list.files("R", pattern = "[.][Rr]$", full.names = TRUE)
purrr::walk(r_files, ~ source(.x, local = FALSE))

paths <- invisible(generate_candidate_pages(project_dir = "."))
message("Páginas generadas: ", length(paths))
