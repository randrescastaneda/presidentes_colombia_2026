args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[[1]], winslash = "/", mustWork = FALSE) else "."
target_date <- if (length(args) >= 2) args[[2]] else as.character(Sys.Date())

target_dir <- file.path(project_dir, "data", "inbox", target_date)
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

source_template <- file.path(project_dir, "data", "inbox", "template_sources.csv")
claim_template <- file.path(project_dir, "data", "inbox", "template_claims.csv")

source_target <- file.path(target_dir, "sources.csv")
claim_target <- file.path(target_dir, "claims.csv")

if (!file.exists(source_target)) {
  file.copy(source_template, source_target)
}

if (!file.exists(claim_target)) {
  file.copy(claim_template, claim_target)
}

message("Lote listo en ", target_dir)
