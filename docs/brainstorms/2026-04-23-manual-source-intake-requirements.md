---
date: 2026-04-23
topic: manual-source-intake
---

# Manual Source Intake For Candidate Research

## Problem Frame

`data/added_manually/` necesita dejar de ser una carpeta de notas sueltas y convertirse en un canal formal de descubrimiento. El sistema debe conservar todas las URLs válidas de forma auditable, promover automáticamente solo las que pasen validación mínima y dejar visibles para el usuario las fuentes útiles que todavía estén `por clasificar`.

## Requirements

**Biblioteca auditable**
- R1. `data/added_manually/` debe operar como canal de descubrimiento conectado al pipeline.
- R2. Toda URL válida descubierta allí debe quedar guardada en un ledger auditable.
- R3. El ledger debe registrar enlace original, archivo de origen, estado de validación y estado editorial.
- R4. Enlaces rotos o inválidos no deben publicarse.

**Promoción**
- R5. El sistema debe validar enlaces, deduplicar y proponer clasificación básica.
- R6. Solo las fuentes que pasen reglas mínimas deben promoverse automáticamente al corpus formal.
- R7. Las fuentes válidas pero incompletas deben quedar en revisión, no descartarse.
- R8. No se debe forzar clasificación especulativa de candidato.

**Acceso público**
- R9. Debe existir acceso en repo y en el sitio público.
- R10. Las fuentes válidas sin clasificación suficiente deben aparecer como `Fuentes por clasificar`.
- R11. El sitio debe distinguir entre fuentes integradas y fuentes pendientes.
- R12. Toda fuente pública debe enlazar al original.

## Success Criteria

- Las listas manuales ya no se pierden fuera del sistema.
- El repo conserva un ledger auditable de hallazgos manuales.
- El pipeline promueve automáticamente solo las fuentes trazables.
- El sitio expone tanto fuentes integradas como fuentes pendientes de clasificación.

## Scope Boundaries

- No entra extraer claims automáticamente desde cada URL descubierta.
- No entra resolver clasificación perfecta por candidato desde el primer pase.
- No entra publicar enlaces rotos por completitud.

## Key Decisions

- Enfoque híbrido: repo auditable completo y sitio público selectivo.
- Promoción semiautomática en vez de totalmente manual o totalmente automática.
- `Fuentes por clasificar` como estado público válido.

## Next Steps

-> `docs/plans/2026-04-23-002-feat-manual-source-intake-plan.md`
