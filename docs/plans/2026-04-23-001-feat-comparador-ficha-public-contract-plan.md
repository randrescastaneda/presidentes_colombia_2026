---
title: feat: Extend artifact-driven public contracts to comparador and candidate proposals
type: feat
status: active
date: 2026-04-23
origin: docs/brainstorms/2026-04-23-artifact-driven-comparador-fichas-requirements.md
---

# feat: Extend artifact-driven public contracts to comparador and candidate proposals

## Overview

Plan previo para extender el patrón `homepage-first` al comparador y a `Propuestas y posiciones públicas` de las fichas.

## Implementation Units

- [ ] Extender el adapter público para comparador y ficha programática.
- [ ] Refactorizar helpers de render para consumir contratos públicos en vez de tablas legacy.
- [ ] Conectar `comparador.qmd` y las fichas generadas al nuevo contrato.

## Verification

- Contratos públicos cubiertos por `tests/testthat/test-site-public-view-models.R`
- Plantillas y páginas generadas cubiertas por `tests/testthat/test-site-generation.R`
