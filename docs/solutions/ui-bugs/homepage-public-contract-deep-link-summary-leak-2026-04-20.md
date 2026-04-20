---
title: Homepage public contract fix for drill-down, summary sanitization, and safe fallback
date: 2026-04-20
category: ui-bugs
module: homepage
problem_type: ui_bug
component: rails_view
symptoms:
  - "La homepage comparativa podía renderizar payload interno del pipeline en la nota principal y en los summaries por tema."
  - "Los links por candidato desde los bloques comparativos no restauraban el contexto temático al aterrizar en la ficha."
  - "La portada podía degradar mal cuando faltaba `validation_status.csv` o cuando `comparison_report.json` traía candidatos ausentes del registry."
root_cause: logic_error
resolution_type: code_fix
severity: high
related_components:
  - documentation
  - tooling
tags:
  - homepage
  - comparison-handoff
  - candidate-drilldown
  - validation-fallback
  - summary-sanitization
  - candidate-registry
---

# Homepage public contract fix for drill-down, summary sanitization, and safe fallback

## Problem
La primera versión del slice `homepage-first` dejó una brecha entre la promesa editorial de la portada y el contrato real de navegación pública. La home ya estaba organizada alrededor de comparación rápida, pero todavía podía exponer texto interno del pipeline, perder el contexto temático al abrir una ficha y degradar mal cuando faltaba metadata pública auxiliar.

## Symptoms
- La hero note podía mostrar texto del estilo `estado_vs_mercado: a=...; b=...` en vez de una síntesis editorial pública.
- Los summaries de los bloques comparativos podían filtrar wording interno o slugs de candidatos.
- Un clic desde una comparación temática llegaba a la ficha sin restaurar el contexto comparativo más allá del anchor genérico.
- Si `validation_status.csv` no existía o venía vacío, la badge metodológica podía romper la portada.
- Si `comparison_report.json` incluía `candidate_id` que no existían en `candidate_registry.csv`, la home podía construir links inválidos.

## What Didn't Work
- No funcionó pasar `homepage_brief.key_comparison_note` directamente a la UI. Cuando el artifact venía con forma semiestructurada del pipeline, la portada terminaba publicando payload interno.
- Tampoco funcionó pasar `topic_row$summary` directo a la homepage. El summary podía ser técnicamente “válido” pero seguía siendo texto pensado para el pipeline, no para lectura pública.
- Resolver el drill-down solo con un anchor común tampoco bastaba. Sin un consumidor del `topic` en la ficha, la URL preservaba contexto pero el destino no lo restituía visualmente.

Ejemplos del failure mode original:

```r
"estado_vs_mercado: a=Estado coordinador; b=Evidencia insuficiente"
```

```text
candidatos/ivan-cepeda.html
```

## Solution
El fix movió la homepage a una capa explícita de view-model público en [R/site_public_view_models.R](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/R/site_public_view_models.R) y completó el handoff contextual en [R/site_generation.R](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/R/site_generation.R).

Cambios principales:
- `build_public_key_comparison_note()` ahora sintetiza una lectura editorial pública cuando el texto de origen parece payload interno.
- `build_public_comparison_summary()` reemplaza el summary crudo por prosa pública basada en `topic_id`, nombres públicos y `evidence_state`.
- `validation_badge_view_model()` deja de indexar eagermente `validation_status[[1]]` y degrada de forma segura si la tabla falta o viene vacía.
- `build_homepage_comparison_blocks()` filtra candidatos ausentes del registry antes de construir `candidate_names` y `candidate_destinations`.
- `generate_candidate_page_template()` ahora consume `?from=homepage&topic=...` y muestra una nota contextual en la sección de propuestas.
- [index.qmd](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/index.qmd) consume solo el view-model público para links candidate-focused y copy comparativa.

El handoff quedó así:

```r
href = paste0(
  "candidatos/",
  candidate_meta$slug %||% candidate_id,
  ".html?from=homepage&topic=",
  utils::URLencode(normalize_public_scalar(topic_row$topic_id, default = "sin-tema"), reserved = TRUE),
  "#propuestas-y-posiciones-publicas"
)
```

Y la ficha ahora revela el contexto de origen:

```js
var params = new URLSearchParams(window.location.search);
var topic = params.get('topic');
var from = params.get('from');
if (from !== 'homepage' || !topic) return;
```

## Why This Works
Funciona porque separa claramente artefactos internos y superficie pública. La homepage ya no decide desde la plantilla si un texto “se ve raro”; consume un view-model que:
- sintetiza copy público,
- evita links para candidatos huérfanos,
- preserva contexto temático en la URL,
- y aterriza en una ficha que sí consume ese contexto.

Eso convierte homepage y fichas en un flujo coherente, en lugar de dos superficies conectadas solo por enlaces genéricos.

## Prevention
- No renderizar en UI pública campos crudos de artifacts comparativos sin una capa de normalización editorial.
- Mantener el handoff de navegación en el view-model, no recomponer URLs ad hoc desde la plantilla.
- Tratar `candidate_registry.csv` como fuente de verdad para destinos públicos; si un candidato no existe ahí, degradar a un destino más amplio o eliminar el link.
- Blindar el comportamiento con tests de contrato en:
  - [tests/testthat/test-site-public-view-models.R](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/tests/testthat/test-site-public-view-models.R)
  - [tests/testthat/test-site-generation.R](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/tests/testthat/test-site-generation.R)
- Revalidar siempre con render real después de tocar handoff público (`quarto render`), porque parte del contrato vive en el HTML generado y no solo en los helpers.

La ronda final de `ce:review` no dejó findings accionables abiertos.

## Related Issues
- [README.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/README.md)
- [PROJECT_CONTEXT.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/PROJECT_CONTEXT.md)
- [reference/architecture-minimal-analytic-architecture.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/reference/architecture-minimal-analytic-architecture.md)
- [reference/daily-operations.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/reference/daily-operations.md)
- No se encontraron issues relacionadas en GitHub para este fix.
