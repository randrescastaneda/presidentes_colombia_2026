# Mapa de componentes

## Base actual

- `scripts/run_daily_update.R`: entrypoint diario actual
- `R/pipeline.R`: batch principal actual
- `R/core.R`: taxonomía, screening y análisis simple actual
- `R/site_helpers.R`: render narrativo actual

## Capa contractual nueva

- `config/claim_type_taxonomy.csv`: tipos de afirmación
- `config/analysis_axes.csv`: ejes analíticos
- `config/validation_rules.yml`: reglas de QA metodológica
- `config/editorial_style.md`: estilo y disciplina editorial
- `config/output_templates.yml`: artefactos publicables esperados

## Prompts

- `prompts/orchestrator.md`
- `prompts/extractor.md`
- `prompts/analyzer.md`
- `prompts/comparator.md`
- `prompts/writer.md`
- `prompts/validator.md`

## Schemas

- `schemas/source_packet.schema.json`
- `schemas/extraction_result.schema.json`
- `schemas/candidate_analysis.schema.json`
- `schemas/comparison_report.schema.json`
- `schemas/editorial_package.schema.json`
- `schemas/validation_report.schema.json`

## Estado y staging

- `data/state/source_registry.csv`
- `data/state/candidate_state.csv`
- `data/staging/extraction/`
- `data/staging/analysis/`
- `data/staging/comparison/`
- `data/staging/editorial/`
- `data/staging/validation/`
