---
date: 2026-04-23
topic: artifact-driven-comparador-fichas
---

# Artifact-Driven Comparador Y Fichas

## Problem Frame

El sitio ya dio un primer paso con `homepage-first`: la portada pública consume un view-model derivado de artefactos públicos en vez de depender directamente de tablas legacy. La siguiente etapa debe extender ese enfoque al comparador y al destino de ficha para que la navegación entre superficies sostenga una semántica pública consistente, sin prometer comparaciones que la evidencia o el destino todavía no pueden sostener.

Hoy existe una asimetría relevante: el comparador ya está más cerca de un contrato público estable, mientras que las fichas todavía mezclan varias tablas legacy y varias capas editoriales distintas en una sola superficie. El problema a resolver no es solo migrar lecturas de datos, sino definir qué promesa pública hace cada superficie y qué contrato la gobierna.

## Requirements

**Slice Y Superficies**
- R1. El siguiente slice debe tratarse como `comparador-first`, no como migración integral de fichas.
- R2. El slice debe incluir un destino soportado en ficha programática mínima hasta `Propuestas y posiciones públicas`.
- R3. `Análisis lógico publicado` debe quedar explícitamente fuera del scope principal de este slice.

**Contratos Públicos**
- R4. El comparador debe quedar gobernado por un contrato público consistente basado en `comparison_report`, con adaptación pública adicional si la superficie de render lo necesita.
- R5. La ficha programática no debe consumir directamente artefactos analíticos crudos como contrato de UI principal.
- R6. La ficha programática debe quedar gobernada por un view-model público derivado, específico para web y orientado a propuestas comparables por candidato y por tema.

**Navegación Y Continuidad**
- R7. La navegación desde homepage o comparador hacia una ficha debe aterrizar en la sección completa `Propuestas y posiciones públicas`, no en una vista recortada.
- R8. Cuando exista un `topic` de origen, la ficha debe mostrar foco contextual visible sobre ese tema dentro de la sección completa.
- R9. El destino debe seguir sintiéndose como ficha completa de candidato, no como una mini-surface distinta o temporal.

**Semántica Pública De La Ficha**
- R10. La ficha programática debe separar visual y semánticamente dos estados:
  - `Propuestas comparables`
  - `Propuestas documentadas aún no comparables`
- R11. La presencia de propuestas documentadas pero aún no comparables debe poder publicarse sin inflar su estatus metodológico.
- R12. Solo el contenido que realmente sostenga comparabilidad debe presentarse como comparable; el resto debe declararse explícitamente como documentado pero aún no comparable.

**Reglas De Handoff Entre Superficies**
- R13. La homepage debe seguir siendo una superficie estrictamente comparativa.
- R14. Los enlaces salientes desde la homepage deben existir solo cuando el destino pueda sostener contenido comparable para el tema destacado.
- R15. El comparador puede enlazar a ficha también cuando el tema del candidato esté solo documentado y aún no sea comparable, siempre que el estado quede claro en el destino.
- R16. La ficha debe declarar de inmediato el estado del tema de aterrizaje para evitar que el usuario confunda documentación temática con comparación transversal madura.

## Success Criteria

- La promesa pública entre homepage, comparador y ficha queda explícita y consistente.
- El comparador deja de depender conceptualmente de tablas legacy como fuente pública principal.
- La ficha puede recibir navegación contextual por tema sin fingir más comparabilidad de la que realmente existe.
- El usuario puede distinguir con facilidad entre contenido comparable y contenido solo documentado.
- El próximo planning puede diseñar el slice sin reabrir decisiones básicas de producto, scope o semántica pública.

## Scope Boundaries

- No entra en este slice la migración completa de las fichas como producto editorial integral.
- No entra en este slice la contractualización principal de `Análisis lógico publicado`.
- No entra en este slice rediseñar toda la homepage; se asume el estado resuelto de `homepage-first`.
- No entra en este slice decidir el detalle de implementación del pipeline, schemas internos o layouts de archivos.

## Key Decisions

- `comparador-first` como slice principal: es la superficie más cercana hoy a un contrato público estable y la que mejor ordena la continuidad entre superficies.
- Ficha programática mínima como destino: permite sostener el handoff sin forzar una re-plataformización completa de la ficha.
- View-model público específico para la ficha: evita repetir el problema actual de acoplar la web a artefactos más crudos o con semántica interna.
- Separar comparable vs documentado-no-comparable: preserva honestidad metodológica y reduce el riesgo editorial de sobrerrepresentar madurez comparativa.
- Homepage estrictamente comparativa y comparador más tolerante: alinea mejor la expectativa del usuario con la promesa real de cada superficie.

## Dependencies / Assumptions

- Se asume que `comparison_report` seguirá siendo el artefacto rector para la semántica comparativa pública.
- Se asume que la ficha puede derivar una capa pública enfocada en propuestas sin exigir todavía la migración total de header, trayectoria, biblioteca completa de fuentes u otras capas legacy.
- Se asume que la web debe priorizar claridad metodológica sobre completitud superficial cuando ambas entren en tensión.

## Outstanding Questions

### Deferred to Planning
- [Affects R4-R6][Technical] Qué adapter/view-model público concreto necesita el comparador además de `comparison_report`, y si conviene un adapter separado o compartido con otras superficies.
- [Affects R6-R12][Technical] Qué forma mínima debe tener el view-model público de ficha para sostener tema, estado comparativo, evidencia disponible y degradación honesta.
- [Affects R7-R9][Technical] Cómo se expresa el foco contextual por `topic` dentro de `Propuestas y posiciones públicas` sin romper el orden editorial normal de la ficha.
- [Affects R14-R16][Needs research] Qué reglas exactas de elegibilidad y copy necesitan homepage y comparador para distinguir enlaces a contenido comparable vs documentado-no-comparable.

## Next Steps

-> /ce:plan para structured implementation planning
