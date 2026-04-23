---
date: 2026-04-23
topic: artifact-driven-comparador-fichas
---

# Artifact-Driven Comparador Y Fichas

## Problem Frame

La portada pública ya opera con un contrato público derivado de artefactos. El siguiente paso definido fue extender ese enfoque al comparador y a la sección `Propuestas y posiciones públicas` de las fichas, manteniendo la distinción entre contenido comparable y contenido documentado pero aún no comparable.

## Requirements

- R1. El siguiente slice debe ser `comparador-first`.
- R2. Debe incluir un destino soportado en la ficha hasta `Propuestas y posiciones públicas`.
- R3. `Análisis lógico publicado` queda fuera del scope principal.
- R4. El comparador debe gobernarse por un contrato público consistente derivado de `comparison_report`.
- R5. La ficha no debe consumir artefactos crudos como contrato principal de UI.
- R6. La ficha programática debe separar `comparable` y `documentado, aún no comparable`.

## Success Criteria

- Homepage, comparador y ficha sostienen una semántica pública consistente.
- La ficha puede recibir navegación contextual por tema sin fingir más comparabilidad de la que existe.

## Scope Boundaries

- No entra la migración completa de fichas.
- No entra la contractualización principal de `Análisis lógico publicado`.

## Next Steps

-> `docs/plans/2026-04-23-001-feat-comparador-ficha-public-contract-plan.md`
