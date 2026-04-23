---
title: feat: Formalize manual source intake and promotion pipeline
type: feat
status: active
date: 2026-04-23
origin: docs/brainstorms/2026-04-23-manual-source-intake-requirements.md
---

# feat: Formalize manual source intake and promotion pipeline

## Overview

Formalize `data/added_manually/` as a discovery channel that feeds the existing source pipeline without bypassing editorial guardrails.

## Implementation Units

- [x] **Unit 1: Add a canonical discovery ledger for manual source files**
  Goal: parse curator-authored files, normalize URLs, and persist a durable registry in `data/state/manual_source_registry.csv`.

- [x] **Unit 2: Materialize semiautomatic promotion into the formal pipeline**
  Goal: promote only validated manual rows with sufficient metadata into the public source corpus consumed by `run_pipeline()`.

- [x] **Unit 3: Publish a dual-layer source library**
  Goal: expose promoted sources and `Fuentes por clasificar` through the site’s source library.

- [x] **Unit 4: Document curator workflow and operational guardrails**
  Goal: update repo docs so future sessions understand the relation between `data/added_manually/` and `data/inbox/`.

## Key Files

- `R/manual_source_intake.R`
- `R/pipeline.R`
- `R/state_index.R`
- `R/site_public_view_models.R`
- `fuentes.qmd`
- `data/added_manually/README.md`
- `tests/testthat/test-manual-source-intake.R`

## Verification

- Focused suites for contract, pipeline, manual intake, site view-models, site generation, and publication rules pass.
- Real repo run produced populated `data/state/manual_source_registry.csv`, `data/processed/manual_source_library.csv`, and updated `data/processed/site_metadata.csv`.
