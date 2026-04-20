load_extraction_results <- function(project_dir = ".") {
  extraction_dir <- file.path(project_dir, "data", "staging", "extraction")
  if (!dir.exists(extraction_dir)) {
    return(list())
  }

  files <- list.files(extraction_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), tools::file_path_sans_ext(basename(files)))
}

empty_flattened_claims_tibble <- function() {
  tibble::tibble(
    claim_id = character(),
    candidate_id = character(),
    event_date = as.Date(character()),
    source_id = character(),
    claim_type = character(),
    claim_type_id = character(),
    policy_key = character(),
    topic_id = character(),
    subtopic_id = character(),
    summary_text = character(),
    position_text = character(),
    position_key = character(),
    stance_value = numeric(),
    implementation_detail = logical(),
    mechanism_text = character(),
    target_population = character(),
    problem_diagnosed = character(),
    specificity_score = integer(),
    ambiguity_flag = logical(),
    insufficient_evidence_flag = logical(),
    possible_contradiction_flag = logical(),
    evidence_excerpt = character(),
    inbox_batch = character(),
    batch_date = as.Date(character())
  )
}

first_character_or_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }

  as.character(x[[1]])
}

contract_claim_type_to_legacy <- function(claim_type_id) {
  dplyr::case_when(
    claim_type_id %in% c("propuesta_concreta", "postura_general", "promesa_vaga") ~ "policy_proposal",
    claim_type_id == "dato_contextual" ~ "biography",
    claim_type_id == "critica_adversario" ~ "campaign_status",
    TRUE ~ "campaign_status"
  )
}

normalize_extraction_text <- function(x) {
  cleaned <- normalize_analysis_text(x %||% "")
  cleaned[is.na(cleaned)] <- ""
  cleaned
}

clean_optional_value <- function(x) {
  value <- stringr::str_squish(as.character(x %||% ""))
  if (identical(value, "") || identical(tolower(value), "na") || identical(tolower(value), "null")) {
    return(NA_character_)
  }
  value
}

coerce_extraction_flag <- function(x, default = FALSE) {
  value <- tolower(clean_optional_value(x) %||% "")
  if (value %in% c("true", "t", "1", "si", "sí", "yes")) {
    return(TRUE)
  }
  if (value %in% c("false", "f", "0", "no")) {
    return(FALSE)
  }
  default
}

coerce_extraction_numeric <- function(x) {
  value <- suppressWarnings(as.numeric(clean_optional_value(x)))
  if (length(value) == 0 || is.na(value)) {
    return(NA_real_)
  }
  value[[1]]
}

coerce_extraction_integer <- function(x, default = 0L) {
  value <- suppressWarnings(as.integer(clean_optional_value(x)))
  if (length(value) == 0 || is.na(value)) {
    return(as.integer(default))
  }
  value[[1]]
}

load_claim_type_taxonomy <- function(project_dir = ".") {
  path <- file.path(project_dir, "config", "claim_type_taxonomy.csv")
  if (!file.exists(path)) {
    return(tibble::tibble(claim_type_id = character()))
  }

  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(claim_type_id = as.character(claim_type_id))
}

normalize_contract_claim_type <- function(value, allowed_types = character()) {
  normalized <- normalize_extraction_text(value)

  guess <- dplyr::case_when(
    grepl("propuesta", normalized, perl = TRUE) ~ "propuesta_concreta",
    grepl("postura", normalized, perl = TRUE) ~ "postura_general",
    grepl("diagnost", normalized, perl = TRUE) ~ "diagnostico_problema",
    grepl("slogan|consigna", normalized, perl = TRUE) ~ "slogan",
    grepl("critica|adversar", normalized, perl = TRUE) ~ "critica_adversario",
    grepl("promesa", normalized, perl = TRUE) ~ "promesa_vaga",
    TRUE ~ "dato_contextual"
  )

  if (length(allowed_types) > 0 && !guess %in% allowed_types) {
    return("dato_contextual")
  }

  guess
}

parse_markdown_key_values <- function(lines) {
  matches <- regmatches(
    lines,
    regexec("^\\s*-\\s+([A-Za-z0-9_]+)\\s*:\\s*(.*)$", lines, perl = TRUE)
  )

  values <- purrr::map(matches, function(match) {
    if (length(match) < 3) {
      return(NULL)
    }

    key <- trimws(match[[2]])
    value <- clean_optional_value(match[[3]])
    stats::setNames(list(value), key)
  })

  purrr::compact(values) |>
    purrr::flatten()
}

extract_markdown_section_lines <- function(lines, heading_pattern) {
  heading_idx <- grep(heading_pattern, lines, perl = TRUE)
  if (length(heading_idx) == 0) {
    return(character())
  }

  start_idx <- heading_idx[[1]] + 1
  if (start_idx > length(lines)) {
    return(character())
  }

  next_heading <- grep("^##\\s+", lines, perl = TRUE)
  next_heading <- next_heading[next_heading > heading_idx[[1]]]
  end_idx <- if (length(next_heading) == 0) length(lines) else next_heading[[1]] - 1

  if (end_idx < start_idx) {
    return(character())
  }

  lines[start_idx:end_idx]
}

extract_structured_claim_blocks <- function(text) {
  if (is.na(text) || identical(text, "")) {
    return(list())
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  section_lines <- extract_markdown_section_lines(lines, "^##\\s+Structured claims\\s*$")
  if (length(section_lines) == 0) {
    return(list())
  }

  block_markers <- grep("^###\\s+", section_lines, perl = TRUE)
  if (length(block_markers) == 0) {
    parsed <- parse_markdown_key_values(section_lines)
    return(if (length(parsed) == 0) list() else list(parsed))
  }

  purrr::map(seq_along(block_markers), function(i) {
    start_idx <- block_markers[[i]] + 1
    end_idx <- if (i == length(block_markers)) length(section_lines) else block_markers[[i + 1]] - 1
    parse_markdown_key_values(section_lines[start_idx:end_idx])
  }) |>
    purrr::keep(\(block) length(block) > 0)
}

extract_source_text_body <- function(text) {
  if (is.na(text) || identical(text, "")) {
    return("")
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  body_lines <- extract_markdown_section_lines(lines, "^##\\s+Source text or cleaned transcript\\s*$")

  if (length(body_lines) == 0) {
    body_lines <- lines[!grepl("^\\s*-\\s+[A-Za-z0-9_]+\\s*:", lines, perl = TRUE)]
    body_lines <- body_lines[!grepl("^##\\s+Structured claims\\s*$", body_lines, perl = TRUE)]
    body_lines <- body_lines[!grepl("^###\\s+", body_lines, perl = TRUE)]
  }

  stringr::str_squish(paste(body_lines, collapse = "\n"))
}

build_candidate_alias_table <- function(candidates) {
  if (nrow(candidates) == 0) {
    return(tibble::tibble(candidate_id = character(), alias = character()))
  }

  purrr::map_dfr(seq_len(nrow(candidates)), function(i) {
    row <- candidates[i, , drop = FALSE]
    slug_parts <- unlist(strsplit(row$slug[[1]] %||% "", "-", fixed = TRUE))
    aliases <- unique(stats::na.omit(c(
      row$president_name[[1]] %||% NA_character_,
      row$slug[[1]] %||% NA_character_,
      paste(slug_parts, collapse = " "),
      dplyr::last(slug_parts)
    )))

    tibble::tibble(
      candidate_id = row$candidate_id[[1]],
      alias = normalize_extraction_text(aliases)
    ) |>
      dplyr::filter(alias != "")
  })
}

detect_candidate_ids_from_text <- function(text, packet, candidates) {
  hinted <- unique(stats::na.omit(packet$candidate_hints %||% character()))
  if (length(hinted) > 0) {
    return(hinted)
  }

  if (nrow(candidates) == 0) {
    return(character())
  }

  normalized <- normalize_extraction_text(text)
  alias_table <- build_candidate_alias_table(candidates)

  matched <- alias_table |>
    dplyr::filter(alias != "") |>
    dplyr::rowwise() |>
    dplyr::mutate(mentioned = grepl(paste0("\\b", stringr::str_replace_all(alias, "\\s+", "\\\\s+"), "\\b"), normalized, perl = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::filter(mentioned) |>
    dplyr::pull(candidate_id) |>
    unique()

  matched
}

build_topic_alias_table <- function(taxonomy) {
  if (nrow(taxonomy) == 0) {
    return(tibble::tibble(topic_id = character(), alias = character()))
  }

  purrr::map_dfr(seq_len(nrow(taxonomy)), function(i) {
    row <- taxonomy[i, , drop = FALSE]
    aliases <- unique(stats::na.omit(c(
      row$topic_id[[1]] %||% NA_character_,
      row$slug[[1]] %||% NA_character_,
      row$label_public[[1]] %||% NA_character_
    )))

    tibble::tibble(
      topic_id = row$topic_id[[1]],
      alias = normalize_extraction_text(aliases)
    ) |>
      dplyr::filter(alias != "")
  })
}

detect_topic_id_from_text <- function(text, taxonomy) {
  if (nrow(taxonomy) == 0) {
    return(NA_character_)
  }

  normalized <- normalize_extraction_text(text)
  alias_table <- build_topic_alias_table(taxonomy)

  scores <- alias_table |>
    dplyr::rowwise() |>
    dplyr::mutate(
      matched = grepl(paste0("\\b", stringr::str_replace_all(alias, "\\s+", "\\\\s+"), "\\b"), normalized, perl = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(matched) |>
    dplyr::count(topic_id, sort = TRUE)

  if (nrow(scores) == 0) {
    return(NA_character_)
  }

  scores$topic_id[[1]]
}

derive_policy_key <- function(topic_id, text) {
  normalized <- normalize_extraction_text(text)
  tokens <- unlist(strsplit(gsub("[^a-z0-9 ]", " ", normalized), "\\s+", perl = TRUE))
  tokens <- tokens[tokens != "" & !tokens %in% c("para", "con", "del", "las", "los", "una", "uno", "que", "por", "sin", "sobre")]
  tokens <- tokens[seq_len(min(3, length(tokens)))]

  if (length(tokens) == 0) {
    return(NA_character_)
  }

  paste(c(topic_id %||% "general", tokens), collapse = "-")
}

infer_mechanism_text <- function(text) {
  normalized_original <- stringr::str_squish(text %||% "")
  match <- stringr::str_match(
    normalized_original,
    "(?i)(?:mediante|a trav[eé]s de|por medio de|con)\\s+([^.;:]{6,140})"
  )

  mechanism <- clean_optional_value(match[, 2])
  mechanism
}

infer_target_population <- function(text) {
  normalized_original <- stringr::str_squish(text %||% "")
  match <- stringr::str_match(
    normalized_original,
    "(?i)(?:para|a)\\s+(?:los|las|el|la)?\\s*([^.;:]{4,80})"
  )

  clean_optional_value(match[, 2])
}

infer_problem_diagnosed <- function(text, claim_type_id = NA_character_) {
  normalized_original <- stringr::str_squish(text %||% "")

  if (identical(claim_type_id, "diagnostico_problema")) {
    return(normalized_original)
  }

  match <- stringr::str_match(
    normalized_original,
    "(?i)(?:por la|por el|frente a|ante|debido a|para resolver)\\s+([^.;:]{5,120})"
  )

  clean_optional_value(match[, 2])
}

infer_claim_type_from_text <- function(text, mechanism_text = NA_character_) {
  normalized <- normalize_extraction_text(text)

  if (grepl("cuestion|critica|ataca|denuncia|responsabiliza|culpa|contra", normalized, perl = TRUE) &&
      grepl("gobierno|adversario|oposic|candidato", normalized, perl = TRUE)) {
    return("critica_adversario")
  }

  if (grepl("crisis|problema|desorden|corrupcion|violencia|rezago|deuda social|falta de", normalized, perl = TRUE) &&
      !grepl("crear|fortalecer|reorganizar|implementar|proponer|impulsar", normalized, perl = TRUE)) {
    return("diagnostico_problema")
  }

  if (nchar(normalized) < 90 && grepl("mano dura|primero colombia|sin miedo|con toda|el cambio", normalized, perl = TRUE)) {
    return("slogan")
  }

  if (!is.na(mechanism_text) ||
      grepl("crear|fortalecer|reorganizar|implementar|impulsar|reducir|aumentar|subsid|auditor|plan|programa|reforma|eliminar|construir", normalized, perl = TRUE)) {
    if (grepl("buscar|promet|aspira|trabajara por|har[aá] que", normalized, perl = TRUE) && is.na(mechanism_text)) {
      return("promesa_vaga")
    }
    return("propuesta_concreta")
  }

  if (grepl("prioriza|defiende|apuesta por|quiere|busca|plantea|insiste en", normalized, perl = TRUE)) {
    return("postura_general")
  }

  if (grepl("promete|garantizar[aá]|lograr[aá]|asegurar[aá]", normalized, perl = TRUE)) {
    return("promesa_vaga")
  }

  "dato_contextual"
}

infer_specificity_score <- function(claim_type_id, text, mechanism_text = NA_character_) {
  normalized <- normalize_extraction_text(text)

  base_score <- dplyr::case_when(
    claim_type_id == "propuesta_concreta" ~ 2L,
    claim_type_id %in% c("postura_general", "diagnostico_problema") ~ 1L,
    TRUE ~ 0L
  )

  if (!is.na(mechanism_text)) {
    base_score <- base_score + 1L
  }

  if (grepl("\\b[0-9]+\\b|auditor|subsid|impuesto|ministerio|agencia|programa|plan|bono", normalized, perl = TRUE)) {
    base_score <- base_score + 1L
  }

  as.integer(min(3L, base_score))
}

infer_ambiguity_flag <- function(text) {
  grepl("buscar|podria|podr[aá]|explorar|evaluar|eventualmente|sin detallar|varios|alguna", normalize_extraction_text(text), perl = TRUE)
}

split_source_text_segments <- function(text) {
  if (is.na(text) || identical(stringr::str_squish(text), "")) {
    return(character())
  }

  normalized <- gsub("\r", "", text, fixed = TRUE)
  segments <- unlist(strsplit(normalized, "\n\\s*\n", perl = TRUE))
  segments <- stringr::str_squish(gsub("^[-*]\\s+", "", segments, perl = TRUE))
  segments <- segments[segments != ""]
  segments <- segments[!grepl("^##\\s+|^###\\s+", segments, perl = TRUE)]

  if (length(segments) == 0) {
    segments <- unlist(strsplit(normalized, "(?<=[.!?])\\s+", perl = TRUE))
    segments <- stringr::str_squish(segments)
    segments <- segments[nchar(segments) >= 20]
  }

  unique(segments)
}

claim_from_structured_block <- function(block, packet, index, project_dir = ".") {
  taxonomy <- tryCatch(load_taxonomy(file.path(project_dir, "config", "taxonomy_v1.csv")), error = function(...) tibble::tibble())
  candidates <- tryCatch(load_candidate_registry(project_dir), error = function(...) tibble::tibble())
  claim_types <- load_claim_type_taxonomy(project_dir)$claim_type_id

  body_text <- paste(
    clean_optional_value(block$position_text),
    clean_optional_value(block$summary_text),
    clean_optional_value(packet$text_content),
    sep = " "
  )

  candidate_id <- clean_optional_value(block$candidate_id) %||%
    (first_character_or_na(detect_candidate_ids_from_text(body_text, packet, candidates)) %||% first_character_or_na(packet$candidate_hints) %||% NA_character_)
  topic_id <- clean_optional_value(block$topic_id) %||% detect_topic_id_from_text(body_text, taxonomy)
  mechanism_text <- clean_optional_value(block$mechanism_text) %||% infer_mechanism_text(body_text)
  claim_type_id <- normalize_contract_claim_type(
    clean_optional_value(block$claim_type) %||% infer_claim_type_from_text(body_text, mechanism_text),
    allowed_types = claim_types
  )

  list(
    claim_id = clean_optional_value(block$claim_id) %||% paste0(packet$source_id, "-claim-", index),
    candidate_id = candidate_id,
    claim_type = claim_type_id,
    summary_text = clean_optional_value(block$summary_text) %||% clean_optional_value(block$position_text) %||% clean_optional_value(packet$captured_excerpt) %||% "",
    position_text = clean_optional_value(block$position_text) %||% clean_optional_value(block$summary_text) %||% "",
    topic_id = topic_id %||% NA_character_,
    subtopic_id = clean_optional_value(block$subtopic_id),
    policy_key = clean_optional_value(block$policy_key) %||% derive_policy_key(topic_id, body_text),
    mechanism_text = mechanism_text,
    target_population = clean_optional_value(block$target_population) %||% infer_target_population(body_text),
    problem_diagnosed = clean_optional_value(block$problem_diagnosed) %||% infer_problem_diagnosed(body_text, claim_type_id),
    stance_value = coerce_extraction_numeric(block$stance_value),
    specificity_score = coerce_extraction_integer(block$specificity_score, infer_specificity_score(claim_type_id, body_text, mechanism_text)),
    ambiguity_flag = coerce_extraction_flag(block$ambiguity_flag, infer_ambiguity_flag(body_text)),
    insufficient_evidence_flag = coerce_extraction_flag(block$insufficient_evidence_flag, is.na(candidate_id) || is.na(topic_id)),
    possible_contradiction_flag = coerce_extraction_flag(block$possible_contradiction_flag, FALSE),
    evidence_excerpt = clean_optional_value(block$evidence_excerpt) %||% stringr::str_trunc(clean_optional_value(packet$captured_excerpt) %||% body_text, 240)
  )
}

claim_from_text_segment <- function(segment, packet, index, candidates, taxonomy, project_dir = ".") {
  candidate_ids <- detect_candidate_ids_from_text(segment, packet, candidates)
  candidate_id <- first_character_or_na(candidate_ids) %||% first_character_or_na(packet$candidate_hints) %||% NA_character_
  topic_id <- detect_topic_id_from_text(segment, taxonomy)
  mechanism_text <- infer_mechanism_text(segment)
  claim_type_id <- normalize_contract_claim_type(
    infer_claim_type_from_text(segment, mechanism_text),
    allowed_types = load_claim_type_taxonomy(project_dir)$claim_type_id
  )

  list(
    claim_id = paste0(packet$source_id, "-claim-", index),
    candidate_id = candidate_id,
    claim_type = claim_type_id,
    summary_text = stringr::str_trunc(stringr::str_squish(segment), 180),
    position_text = stringr::str_squish(segment),
    topic_id = topic_id %||% NA_character_,
    subtopic_id = NA_character_,
    policy_key = derive_policy_key(topic_id, segment),
    mechanism_text = mechanism_text,
    target_population = infer_target_population(segment),
    problem_diagnosed = infer_problem_diagnosed(segment, claim_type_id),
    stance_value = if (claim_type_id %in% c("critica_adversario", "dato_contextual")) NA_real_ else 1,
    specificity_score = infer_specificity_score(claim_type_id, segment, mechanism_text),
    ambiguity_flag = infer_ambiguity_flag(segment),
    insufficient_evidence_flag = is.na(candidate_id) || is.na(topic_id),
    possible_contradiction_flag = FALSE,
    evidence_excerpt = stringr::str_trunc(stringr::str_squish(segment), 240)
  )
}

build_extraction_result_from_source_packet <- function(packet, project_dir = ".") {
  taxonomy <- tryCatch(load_taxonomy(file.path(project_dir, "config", "taxonomy_v1.csv")), error = function(...) tibble::tibble())
  candidates <- tryCatch(load_candidate_registry(project_dir), error = function(...) tibble::tibble())
  structured_blocks <- extract_structured_claim_blocks(packet$text_content)

  claims <- if (length(structured_blocks) > 0) {
    purrr::imap(structured_blocks, \(block, index) claim_from_structured_block(block, packet, index, project_dir))
  } else {
    body_text <- extract_source_text_body(packet$text_content)
    segments <- unique(c(
      split_source_text_segments(body_text),
      split_source_text_segments(packet$captured_excerpt %||% ""),
      split_source_text_segments(packet$title %||% "")
    ))

    purrr::imap(segments, \(segment, index) claim_from_text_segment(segment, packet, index, candidates, taxonomy, project_dir))
  }

  valid_claims <- purrr::keep(claims, \(claim) !is.na(claim$candidate_id %||% NA_character_) && !is.na(claim$topic_id %||% NA_character_))
  if (length(valid_claims) == 0 && length(claims) > 0) {
    valid_claims <- claims
  }

  list(
    source_id = packet$source_id,
    batch_date = packet$batch_date %||% as.character(Sys.Date()),
    candidates_detected = unique(stats::na.omit(c(
      packet$candidate_hints %||% character(),
      purrr::map_chr(valid_claims, \(claim) claim$candidate_id %||% NA_character_)
    ))),
    claims = valid_claims
  )
}

build_extraction_results_from_source_packets <- function(source_packets, project_dir = ".") {
  if (length(source_packets) == 0) {
    return(list())
  }

  extraction_results <- purrr::map(source_packets, build_extraction_result_from_source_packet, project_dir = project_dir)
  stats::setNames(extraction_results, purrr::map_chr(extraction_results, "source_id"))
}

normalize_extraction_claims <- function(claims) {
  if (is.null(claims) || length(claims) == 0) {
    return(tibble::tibble())
  }

  if (inherits(claims, "data.frame")) {
    return(tibble::as_tibble(claims))
  }

  purrr::map_dfr(claims, function(claim) {
    if (is.null(claim) || length(claim) == 0) {
      return(tibble::tibble())
    }

    tibble::as_tibble(claim)
  })
}

write_extraction_results <- function(extraction_results, project_dir = ".") {
  if (length(extraction_results) == 0) {
    return(character())
  }

  output_dir <- file.path(project_dir, "data", "staging", "extraction")

  paths <- purrr::imap_chr(extraction_results, function(result, source_id) {
    batch_date <- result$batch_date %||% "undated"
    path <- file.path(output_dir, batch_date, paste0(source_id, ".json"))
    write_contract_json(result, path)
    path
  })

  unname(paths)
}

materialize_extraction_results <- function(project_dir = ".", source_packets = load_source_packets(project_dir)) {
  existing <- load_extraction_results(project_dir)
  automated <- build_extraction_results_from_source_packets(source_packets, project_dir = project_dir)

  merged <- automated
  if (length(existing) > 0) {
    merged[names(existing)] <- existing
  }

  write_extraction_results(merged, project_dir)
  merged
}

flatten_extraction_claims <- function(extraction_results) {
  if (length(extraction_results) == 0) {
    return(empty_flattened_claims_tibble())
  }

  purrr::imap_dfr(extraction_results, function(result, name) {
    claims <- normalize_extraction_claims(result$claims)
    if (nrow(claims) == 0) {
      return(empty_flattened_claims_tibble())
    }

    claims |>
      dplyr::mutate(
        source_id = result$source_id %||% tools::file_path_sans_ext(name),
        event_date = as.Date(result$batch_date %||% NA_character_),
        claim_type_id = .data$claim_type,
        claim_type = contract_claim_type_to_legacy(.data$claim_type),
        position_key = dplyr::case_when(
          !is.na(.data$stance_value) & .data$stance_value > 0 ~ "a_favor",
          !is.na(.data$stance_value) & .data$stance_value < 0 ~ "en_contra",
          TRUE ~ NA_character_
        ),
        implementation_detail = dplyr::if_else(!is.na(.data$mechanism_text) & .data$mechanism_text != "", TRUE, FALSE),
        inbox_batch = result$batch_date %||% NA_character_,
        batch_date = as.Date(result$batch_date %||% NA_character_)
      ) |>
      dplyr::select(
        claim_id,
        candidate_id,
        event_date,
        source_id,
        claim_type,
        claim_type_id,
        policy_key,
        topic_id,
        subtopic_id,
        summary_text,
        position_text,
        position_key,
        stance_value,
        implementation_detail,
        mechanism_text,
        target_population,
        problem_diagnosed,
        specificity_score,
        ambiguity_flag,
        insufficient_evidence_flag,
        possible_contradiction_flag,
        evidence_excerpt,
        inbox_batch,
        batch_date
      )
  })
}
