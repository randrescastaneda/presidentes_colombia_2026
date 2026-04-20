# Arquitectura analítica mínima

## Propósito

Evolucionar el monitor actual hacia un sistema especializado en análisis político-programático diario sin romper la base existente de `R + Quarto`.

## Etapas

1. `source_packet`
2. `extraction_result`
3. `candidate_analysis`
4. `comparison_report`
5. `editorial_package`
6. `validation_report`
7. `data/public` y render web

## Principio central

Los componentes no comparten criterio implícito; comparten contratos versionados.

## Estado actual

- Los contratos y prompts ya existen en el repo.
- `source_packet`, `extraction_result`, `candidate_analysis`, `comparison_report`, `editorial_package` y `validation_report` ya se materializan en `data/staging/`.
- `data/public/` ya recibe `candidate_analysis.json`, `comparison_report.json` y `editorial_packages.json` cuando la validación no bloquea.
- `source_texts/` y `sources.csv` alimentan la extracción automática; el scaffold diario ya no depende de `claims.csv`.
- `scripts/run_daily_update.R` ya detiene el render si `validation_report.status == "block"`.

## Restricciones

- No inventar hechos.
- No perder trazabilidad.
- No colapsar todo a izquierda-derecha.
- No publicar análisis fuertes sin validación final.
- No publicar artefactos analíticos nuevos a `data/public/` si falla la validación.
