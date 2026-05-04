---
title: Harden Daily Editorial Source Review Gate
date: 2026-05-03
category: logic-errors
module: daily_source_review_automation
problem_type: logic_error
component: development_workflow
symptoms:
  - "Incorporated curated sources could pass verification without Structured claims."
  - "Evidence excerpts were checked for presence, not equality with curated notes."
  - "Daily review rows were matched to inbox sources by URL only."
  - "Pipe-delimited multi-candidate review rows were rejected."
  - "Tracked run reports persisted volatile git status."
root_cause: logic_error
resolution_type: workflow_improvement
severity: high
tags:
  - daily-automation
  - source-review
  - editorial-gates
  - claim-reconciliation
  - r-pipeline
---

# Harden Daily Editorial Source Review Gate

## Problem

The daily source review verifier could report success even when curated editorial decisions did not fully match the generated public claims. The gate treated `daily_source_reviews`, `source_texts`, and `claim_records` as loosely correlated artifacts instead of enforcing a source-to-claim manifest.

## Symptoms

- A row with `editorial_action == "incorporar"` could pass when `source_texts/<source_id>.md` had no `## Structured claims` block.
- `evidence_excerpt` only had to be non-empty; it did not have to equal the curated excerpt.
- Review rows joined inbox sources by URL, which is unsafe for shared articles and multi-candidate live pages.
- Valid review rows such as `sergio-fajardo|claudia-lopez|luis-gilberto-murillo` were rejected by the `candidate_id` contract.
- Source-note parsing existed in multiple places, so ingestion, extraction, and verification could drift.
- Persisted run reports embedded branch-local `git status`, creating noisy, non-deterministic diffs.
- Generated artifact churn initially made the real editorial changes harder to review.

## What Didn't Work

URL-only reconciliation was too weak:

```r
daily_review |>
  dplyr::left_join(
    source_lookup |>
      dplyr::select(source_id, inbox_candidate_id = candidate_id, url),
    by = "url"
  )
```

This assumes each reviewed URL maps to exactly one source. In this project that is not a safe invariant because media articles and live pages can cover several candidates.

Treating missing structured blocks as an empty mismatch set was also unsafe:

```r
blocks <- extract_review_structured_claim_blocks(source_text_path)
if (length(blocks) == 0) {
  return(tibble::tibble())
}
```

For an incorporated source, zero expected claims should be a blocking error, not success.

Finally, checking only for a non-empty `evidence_excerpt` proved that a claim had some quote-like text, but not that it used the exact excerpt editors approved in the source note.

## Solution

Make source-note parsing shared and make daily verification manifest-based.

The shared parser lives in `R/source_note_parsing.R` and is used by ingestion, extraction, and verification. It owns markdown key parsing, section extraction, `## Structured claims` parsing, source text body extraction, and claim type normalization.

```r
source(file.path(project_dir, "R", "source_note_parsing.R"), local = FALSE)
```

`R/stage_ingestion.R` now uses `parse_source_note_metadata()` from that shared file. `R/stage_extraction.R` now uses the shared markdown parsing and claim type normalization instead of local parser copies.

The verifier resolves reviewed sources by stable ID first:

```r
review_prepared <- daily_review |>
  dplyr::mutate(
    review_source_id = normalize_source_note_optional_text(.data$source_id),
    single_candidate_id = vapply(.data$candidate_id, single_review_candidate_id, character(1))
  )
```

If `source_id` is absent, it falls back to a unique inbox match and narrows by single `candidate_id` when one is available. Ambiguous or missing matches block the reconciliation check.

`candidate_id` validation now accepts a single registered ID, `multiple`, or a pipe-delimited list where every ID is registered:

```r
valid_review_candidate_id <- function(value) {
  value <- normalize_source_note_optional_text(value)
  if (is.na(value) || identical(value, "multiple")) {
    return(!is.na(value))
  }

  candidate_ids <- unlist(strsplit(value, "\\|", perl = TRUE))
  candidate_ids <- stringr::str_squish(candidate_ids)
  length(candidate_ids) > 0 && all(candidate_ids %in% known_candidate_ids)
}
```

For every `incorporar` row, the verifier now requires `## Structured claims`. Missing blocks create a `missing_structured_claims` mismatch instead of silently passing.

Expected and observed claims are compared on the full editorial identity:

```r
key_fields <- c(
  "source_id",
  "candidate_id",
  "claim_type_id",
  "topic_id",
  "subtopic_id",
  "policy_key",
  "evidence_excerpt"
)
```

The verifier blocks both directions:

```r
missing_expected <- valid_expected |>
  dplyr::anti_join(observed |> dplyr::select(dplyr::all_of(key_fields)), by = key_fields)

extra_observed <- observed |>
  dplyr::anti_join(valid_expected |> dplyr::select(dplyr::all_of(key_fields)), by = key_fields)
```

Mismatch rows include repair context such as `review_row_id`, `review_url`, `review_candidate_id`, `source_text_path`, expected fields, observed claim IDs, and observed field values. That makes the JSON report useful to both humans and agents.

The daily review CSV now carries stable IDs for incorporated rows:

```csv
review_date,source_id,candidate_id,source_name,published_at,title,url,theme,editorial_action,reason
2026-05-02,src-20260502-paloma-meta-infraestructura-api,paloma-valencia,...
2026-05-02,src-20260502-ivan-trabajo-sindicatos-api,ivan-cepeda,...
```

The persisted daily report no longer includes raw `git_status`; branch-local state should remain a runtime diagnostic, not a tracked artifact.

The operational contract was documented in `PROJECT_CONTEXT.md` and `data/inbox/template_source_note.md`.

## Why This Works

`daily_source_reviews` and `source_texts/<source_id>.md` now form an explicit manifest for public claims created from curated daily sources. The verifier no longer asks, "Did something plausible get generated?" It asks, "Does every generated claim for this source exactly match the curated source note, and are there no extras?"

Using `source_id` removes URL ambiguity. Falling back only to a unique inbox match preserves compatibility for older review rows without allowing unsafe joins.

Requiring `## Structured claims` for `incorporar` prevents heuristic extraction from being mistaken for editorial approval. Comparing `evidence_excerpt` by equality ensures that public claims cite the exact evidence editors intended, not merely any non-empty excerpt.

Centralizing parsing prevents contract drift. A future change to markdown keys, backtick handling, section boundaries, or claim type normalization now affects ingestion, extraction, and verification consistently.

Removing `git_status` from tracked reports keeps daily automation artifacts focused on verification results and avoids branch-state churn in code review.

## Prevention

- Treat every `incorporar` row as a manifest entry: it should resolve to a stable `source_id`, have structured claims, and reconcile against generated claims by full key equality.
- Keep source-note parsing shared. Do not add a second parser inside a verifier, renderer, or intake script.
- Include negative-path verifier tests whenever adding a new editorial action, source note field, or reconciliation rule.
- Before publishing regenerated artifacts, inspect unrelated public-output diffs; generated churn in source records or candidate pages should not ship unless intentional.

Regression coverage belongs in `tests/testthat/test-daily-automation-verifier.R`. It should include missing structured claims, evidence excerpt drift, duplicate URL fallback, invalid claim types, non-incorporated rows that create claims, and extra generated claims under the same `source_id`.

Run the focused verifier tests and daily checks after changing this contract:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-daily-automation-verifier.R")'
Rscript scripts/run_daily_update.R
Rscript scripts/verify_daily_automation.R --date=YYYY-MM-DD --notify
Rscript scripts/check_daily_automation_health.R --date=YYYY-MM-DD --max-age-hours=30 --notify
```

## Related Issues

- Implementation references:
  - `scripts/verify_daily_automation.R`
  - `R/source_note_parsing.R`
  - `R/stage_extraction.R`
  - `R/stage_ingestion.R`
  - `tests/testthat/test-daily-automation-verifier.R`
  - `tests/testthat/test-extraction-layer.R`
  - `data/analysis/daily_source_reviews/`
  - `data/inbox/template_source_note.md`
  - `PROJECT_CONTEXT.md`
