args <- commandArgs(trailingOnly = TRUE)
project_dir <- if (length(args) >= 1) normalizePath(args[[1]], winslash = "/", mustWork = FALSE) else "."
target_date <- if (length(args) >= 2) args[[2]] else as.character(Sys.Date())

target_dir <- file.path(project_dir, "data", "inbox", target_date)
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(target_dir, "source_texts"), recursive = TRUE, showWarnings = FALSE)

source_template <- file.path(project_dir, "data", "inbox", "template_sources.csv")
source_note_template <- file.path(project_dir, "data", "inbox", "template_source_note.md")
batch_status_template <- file.path(project_dir, "data", "inbox", "template_batch_status.json")

if (!file.exists(source_template)) {
  stop("Missing source template at ", source_template)
}

if (!file.exists(source_note_template)) {
  stop("Missing source note template at ", source_note_template)
}

if (!file.exists(batch_status_template)) {
  stop("Missing batch status template at ", batch_status_template)
}

source_target <- file.path(target_dir, "sources.csv")
batch_status_target <- file.path(target_dir, "batch_status.json")

if (!file.exists(source_target)) {
  copied <- file.copy(source_template, source_target)
  if (!isTRUE(copied)) {
    stop("Could not copy source template into ", source_target)
  }
}

if (!file.exists(batch_status_target)) {
  copied <- file.copy(batch_status_template, batch_status_target)
  if (!isTRUE(copied)) {
    stop("Could not copy batch status template into ", batch_status_target)
  }
}

message("Lote listo en ", target_dir, " con source_texts/ para captura por fuente y extracción estructurada.")
