# AGENTS.md

## Propósito

Este repo implementa un monitor público y riguroso de candidaturas presidenciales de Colombia 2026.
El proyecto investiga propuestas, movimientos de campaña, contexto público y análisis lógico conservador con trazabilidad de fuentes.

## Qué Leer Primero En Una Nueva Sesión

1. Consultar `Family Brain` con `project = presidentes_colombia_2026` para recuperar contexto técnico, editorial y operativo antes de trabajar.
2. `README.md`
3. `PROJECT_CONTEXT.md`
4. `config/taxonomy_v1.csv`
5. `config/candidate_registry.csv`
6. `scripts/run_daily_update.R`
7. `docs/solutions/` cuando exista trabajo previo en el área; contiene soluciones documentadas por categoría con frontmatter buscable (`module`, `tags`, `problem_type`)

## Modelo Mental Del Repo

- `main` es la rama fuente de verdad para desarrollo.
- `origin/gh-pages` es la rama de deployment.
- `docs/` en `main` contiene el render local y auditable del sitio.
- El sitio público servido por GitHub Pages se publica desde `origin/gh-pages`, no desde `main`.
- No mantener worktrees adicionales cuando no sean estrictamente necesarios.

## Flujo Operativo

1. Cargar nuevas fuentes y claims en `data/inbox/YYYY-MM-DD/`.
2. Ejecutar `Rscript scripts/run_daily_update.R`.
3. Revisar `data/processed/`, `data/public/` y `docs/`.
4. Si todo está correcto, hacer commit en `main`.
5. Dejar que GitHub Actions publique a `gh-pages`.

## Convenciones Editoriales Clave

- No inventar hechos.
- No publicar análisis crítico si la evidencia es insuficiente.
- Distinguir siempre entre:
  - hecho trazable
  - síntesis
  - análisis IA
- Una fuente oficial puede bastar para documentos o propuestas explícitas.
- Controversias o afirmaciones sensibles requieren más cautela.

## Estado Técnico Esperado

- Branch de trabajo: `main`
- Remoto: `origin = https://github.com/randrescastaneda/presidentes_colombia_2026.git`
- GitHub Pages: `https://randrescastaneda.github.io/presidentes_colombia_2026/`
- Workflow principal: `.github/workflows/publish.yml`
- Automatización diaria: `colombia-2026-diario`

## Uso De Family Brain

En una nueva sesión, `Family Brain` no es opcional: debe consultarse antes de asumir que el contexto del repo es suficiente. `AGENTS.md` y `PROJECT_CONTEXT.md` resumen el proyecto, pero no reemplazan la memoria acumulada en `Family Brain`.

Guardar en `Family Brain` bajo `project = presidentes_colombia_2026`:

- cada fuente nueva publicada
- cada `analysis_note` nueva publicada
- decisiones metodológicas
- fallos y correcciones técnicas relevantes
- vacíos editoriales y pendientes de investigación

Formato recomendado:

- para fuentes:
  `Fuente <source_id> | candidato=<candidate_id> | fecha=<published_at> | medio=<source_name> | tipo=<source_type> | url=<url> | hallazgo="<quote o síntesis breve>"`
- para análisis:
  `Análisis <analysis_id> | candidato=<candidate_id> | tipo=<analysis_type> | claim=<claim_id> | source=<source_id> | confianza=<confidence> | nota="<public_reasoning_summary>"`

## Mantenimiento De Contexto

- Mantener `PROJECT_CONTEXT.md` actualizado cuando cambien:
  - rama principal o estrategia de deployment
  - estado de GitHub Pages
  - automatización diaria
  - reglas editoriales importantes
  - estructura del pipeline
