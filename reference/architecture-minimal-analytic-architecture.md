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
- El pipeline público todavía usa `claims.csv` manual.
- `data/staging/` y `data/state/` ya están preparados para la siguiente iteración.

## Restricciones

- No inventar hechos.
- No perder trazabilidad.
- No colapsar todo a izquierda-derecha.
- No publicar análisis fuertes sin validación final.
