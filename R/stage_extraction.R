load_extraction_results <- function(project_dir = ".") {
  extraction_dir <- file.path(project_dir, "data", "staging", "extraction")
  if (!dir.exists(extraction_dir)) {
    return(list())
  }

  files <- list.files(extraction_dir, pattern = "[.]json$", recursive = TRUE, full.names = TRUE)
  stats::setNames(lapply(files, read_contract_json), basename(files))
}

legacy_claim_type_to_contract <- function(claim_type) {
  dplyr::case_when(
    claim_type == "policy_proposal" ~ "propuesta_concreta",
    claim_type == "biography" ~ "dato_contextual",
    claim_type == "campaign_status" ~ "dato_contextual",
    TRUE ~ "dato_contextual"
  )
}

contract_claim_type_to_legacy <- function(claim_type_id) {
  dplyr::case_when(
    claim_type_id %in% c("propuesta_concreta", "postura_general", "promesa_vaga") ~ "policy_proposal",
    claim_type_id == "dato_contextual" ~ "biography",
    claim_type_id == "critica_adversario" ~ "campaign_status",
    TRUE ~ "campaign_status"
  )
}

legacy_specificity_score <- function(claim_type, implementation_detail) {
  dplyr::case_when(
    claim_type == "policy_proposal" & implementation_detail %in% c(TRUE, 1) ~ 2L,
    claim_type == "policy_proposal" ~ 1L,
    TRUE ~ 0L
  )
}

build_extraction_results_from_claims <- function(claims, sources) {
  if (nrow(claims) == 0 || nrow(sources) == 0) {
    return(list())
  }

  joined_claims <- claims |>
    dplyr::left_join(
      sources |>
        dplyr::select(source_id, candidate_id, quote_text, inbox_batch),
      by = "source_id",
      suffix = c("", "_source")
    ) |>
    dplyr::mutate(
      batch_date = .data$inbox_batch_source %||% .data$inbox_batch
    )

  grouped <- joined_claims |>
    dplyr::group_split(.data$source_id)

  extraction_results <- purrr::map(grouped, function(source_claims) {
    source_id <- source_claims$source_id[[1]]
    source_rows <- sources |>
      dplyr::filter(.data$source_id == source_id) |>
      dplyr::slice_head(n = 1)

    claims_payload <- purrr::pmap(source_claims, function(
      claim_id, candidate_id, event_date, source_id, claim_type, policy_key, topic_id, summary_text,
      position_text, position_key, stance_value, implementation_detail, inbox_batch, candidate_id_source,
      quote_text, inbox_batch_source, batch_date, ...
    ) {
      list(
        claim_id = claim_id,
        candidate_id = candidate_id,
        claim_type = legacy_claim_type_to_contract(claim_type),
        summary_text = summary_text %||% position_text %||% "",
        position_text = position_text %||% summary_text %||% "",
        topic_id = topic_id %||% NA_character_,
        subtopic_id = NA_character_,
        policy_key = policy_key %||% NA_character_,
        mechanism_text = if (implementation_detail %in% c(TRUE, 1)) position_text %||% NA_character_ else NA_character_,
        target_population = NA_character_,
        problem_diagnosed = if (legacy_claim_type_to_contract(claim_type) == "diagnostico_problema") summary_text %||% NA_character_ else NA_character_,
        stance_value = stance_value %||% NA_real_,
        specificity_score = legacy_specificity_score(claim_type, implementation_detail),
        ambiguity_flag = FALSE,
        insufficient_evidence_flag = FALSE,
        possible_contradiction_flag = FALSE,
        evidence_excerpt = quote_text %||% summary_text %||% position_text %||% ""
      )
    })

    list(
      source_id = source_id,
      batch_date = source_rows$inbox_batch[[1]] %||% source_claims$batch_date[[1]] %||% as.character(Sys.Date()),
      candidates_detected = unique(stats::na.omit(source_claims$candidate_id)),
      claims = claims_payload
    )
  })

  stats::setNames(extraction_results, vapply(extraction_results, `[[`, character(1), "source_id"))
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

materialize_extraction_results <- function(project_dir = ".", claims, sources) {
  existing <- load_extraction_results(project_dir)
  legacy <- build_extraction_results_from_claims(claims, sources)

  merged <- legacy
  if (length(existing) > 0) {
    merged[names(existing)] <- existing
  }

  write_extraction_results(merged, project_dir)
  merged
}

flatten_extraction_claims <- function(extraction_results) {
  if (length(extraction_results) == 0) {
    return(tibble::tibble())
  }

  purrr::imap_dfr(extraction_results, function(result, name) {
    claims <- normalize_extraction_claims(result$claims)
    if (nrow(claims) == 0) {
      return(tibble::tibble())
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
