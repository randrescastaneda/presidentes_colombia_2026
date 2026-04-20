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
7. `data/public`
8. `view-models públicos por superficie`
9. render web

## Principio central

Los componentes no comparten criterio implícito; comparten contratos versionados.

## Estado actual

- Los contratos y prompts ya existen en el repo.
- `source_packet`, `extraction_result`, `candidate_analysis`, `comparison_report`, `editorial_package` y `validation_report` ya se materializan en `data/staging/`.
- `data/public/` ya recibe `candidate_analysis.json`, `comparison_report.json` y `editorial_packages.json` cuando la validación no bloquea.
- `source_texts/` y `sources.csv` alimentan la extracción automática; el scaffold diario ya no depende de `claims.csv`.
- `scripts/run_daily_update.R` ya detiene el render si `validation_report.status == "block"`.
- la homepage ya no debería pensarse como lectura directa de tablas legacy: consume un adapter público derivado de `homepage_brief`, `comparison_report` y `validation_report`.
- el render público necesita una capa de normalización entre artifacts internos y copy visible: síntesis editorial, labels públicos de evidencia y handoff contextual seguro hacia comparador y fichas.

## Restricciones

- No inventar hechos.
- No perder trazabilidad.
- No colapsar todo a izquierda-derecha.
- No publicar análisis fuertes sin validación final.
- No publicar artefactos analíticos nuevos a `data/public/` si falla la validación.
- No renderizar en superficies públicas payload interno o texto pipeline-shaped sin una capa explícita de normalización editorial.
- No prometer drill-down contextual si el destino público no consume ese contexto o no degrada de forma segura.
