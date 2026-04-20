# Mapa de componentes

## Base actual

- `scripts/run_daily_update.R`: entrypoint diario actual
- `R/pipeline.R`: orquestación de etapas y gate de publicación
- `R/core.R`: taxonomía, screening y componentes legacy todavía usados por compatibilidad
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
- `data/staging/source_packets/`
- `data/staging/extraction/`
- `data/staging/analysis/`
- `data/staging/comparison/`
- `data/staging/editorial/`
- `data/staging/validation/`

## Etapas activas del pipeline

- `R/stage_ingestion.R`: arma `source_packet` desde `sources.csv` y `source_texts/`
- `R/stage_extraction.R`: materializa `extraction_result` y normaliza claims comparables
- `R/stage_analysis.R`: construye `candidate_analysis` multidimensional
- `R/stage_comparison.R`: construye `comparison_report` simétrico
- `R/stage_editorial.R`: construye `editorial_package`
- `R/stage_validation.R`: valida y decide si se permite publicación pública
