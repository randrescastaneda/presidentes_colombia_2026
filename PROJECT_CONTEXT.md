# Project Context

Última actualización: 2026-05-04

## Resumen

Proyecto para monitorear candidaturas presidenciales de Colombia 2026 con `Quarto + R`, publicar un sitio estático y ejecutar una investigación diaria automatizada con trazabilidad de fuentes.

## Instrucción Para Nuevas Sesiones

Antes de trabajar en este repo, Codex debe consultar `Family Brain` con `project = presidentes_colombia_2026`. Allí están fuentes, análisis, decisiones técnicas, pendientes editoriales y cambios operativos recientes. Este documento resume el estado del proyecto, pero no reemplaza esa memoria acumulada.

## Snapshot Operativo

- Repo GitHub: `randrescastaneda/candidatos_presidenciales_colombia_2026`
- Rama de desarrollo: `main`
- Rama de publicación: `origin/gh-pages`
- Sitio público: `https://randrescastaneda.github.io/candidatos_presidenciales_colombia_2026/`
- Workflow de publicación: `.github/workflows/publish.yml`
- Automatización diaria activa: `colombia-2026-fuentes-evaluadas`, programada a las 05:45
- La automatización diaria debe correr en worktree aislada y limpia, no sobre el checkout principal compartido.

## Estado Arquitectónico

El repo ya no debe pensarse solo como `inbox -> claims -> render`.

La arquitectura vigente del pipeline ya es:

1. `source_packet`
2. `extraction_result`
3. `candidate_analysis`
4. `comparison_report`
5. `editorial_package`
6. `validation_report`
7. `data/public` + render web

Estado actual:

- la capa contractual existe y ya está conectada al pipeline
- `source_texts/` ya forma parte del scaffold diario
- `source_packet` y `extraction_result` ya son la interfaz interna del pipeline
- `candidate_analysis`, `comparison_report` y `editorial_package` ya se escriben en `data/staging/` y `data/public`
- `validation_report` ya es un gate real: si da `block`, `scripts/run_daily_update.R` aborta antes del render público
- la homepage pública ya consume un view-model derivado desde `homepage_brief`, `comparison_report` y `validation_report`, en vez de depender solo de tablas procesadas legacy
- el contrato público de homepage ahora incluye handoff contextual hacia fichas: links con `?from=homepage&topic=...#propuestas-y-posiciones-publicas`, síntesis pública de comparaciones y degradación segura cuando faltan candidatos o metadata auxiliar
- `data/added_manually/` ahora funciona como canal de descubrimiento curado: produce `data/state/manual_source_registry.csv`, promueve automáticamente URLs válidas con metadata mínima y publica una capa de `fuentes por clasificar` para enlaces útiles aún no integrados al corpus candidato-trazable
- `data/analysis/daily_source_reviews/` registra la bitácora diaria de fuentes evaluadas y alimenta `fuentes-evaluadas.qmd`; las fuentes multi-candidato no deben promoverse automáticamente desde `data/added_manually/` sin candidato, fecha y uso editorial confirmados
- `scripts/verify_daily_automation.R` escribe `data/automation/run_reports/YYYY-MM-DD.*` sin estado git volátil y bloquea commit/push automático si fallan validación pública, topic_id, frases internas, render de fuentes evaluadas, promociones ambiguas desde ingesta manual o la conciliación editorial de `daily_source_reviews`
- las filas de `daily_source_reviews/*.csv` aceptan `candidate_id` único, `multiple` o listas `candidate_id` separadas por `|`; cuando una fila usa `editorial_action=incorporar`, debe reconciliar contra un `source_id` de `data/inbox/*/sources.csv`, un bloque `## Structured claims` en `source_texts/<source_id>.md` y los campos publicados en `data/processed/claim_records.csv`, incluido `evidence_excerpt`
- `scripts/check_daily_automation_health.R` revisa después de la corrida que el reporte diario exista, no esté bloqueado ni obsoleto, y que la bitácora diaria tenga Markdown y CSV
- `scripts/prepare_daily_automation_worktree.sh` crea una worktree temporal desde `origin/main` para la automatización diaria
- `scripts/finalize_daily_automation_worktree.sh` rerenderiza, verifica, commitea, empuja `HEAD:main`, intenta fast-forward local cuando el checkout principal está limpio y elimina la worktree/rama temporal

## Estado Git Consolidado

Al cierre de esta sesión:

- solo debe persistir una worktree activa, la principal; la automatización puede crear worktrees temporales, pero debe eliminarlas al finalizar correctamente
- la rama local activa es `main`
- `gh-pages` existe como rama remota de deployment
- las ramas de trabajo previas de contratos, ingesta manual, viabilidad y fuentes evaluadas fueron incorporadas a `main`
- la automatización diaria quedó configurada para preparar una worktree limpia desde `origin/main`, validar allí, commitear y hacer `git push origin HEAD:main` solo cuando las verificaciones pasen
- la automatización diaria debe ejecutar `scripts/verify_daily_automation.R --date=YYYY-MM-DD --notify` antes de commitear; `--check-oracle` puede usarse como smoke test adicional cuando se quiera probar ChatGPT/Oracle

Commits importantes:

- `06ebc7f` `Inicializa monitor presidencial Colombia 2026`
- `5da7308` `Corrige workflow de GitHub Pages`
- `586d03a` `Documenta operación y contexto del repo`
- `b49a3d3` `Merge pull request #3 from randrescastaneda/codex/deepen-policy-viability`

## Deployment

Situación consolidada el 19 de abril de 2026:

- GitHub Pages quedó habilitado sobre `gh-pages`
- el sitio respondió `HTTP 200`
- el workflow `Quarto Publish` ya superó los fallos iniciales de dependencias

Problemas resueltos:

- Quarto local en macOS necesitó un `HOME` temporal escribible para evitar fallos de caché `deno_kv`
- GitHub Actions falló inicialmente porque `rmarkdown` no se instaló al romperse `fs`
- el fix fue agregar `libuv1-dev` en Ubuntu antes de instalar paquetes R
- también hubo que inicializar correctamente la rama `gh-pages`

## Datos Y Sitio

Estado del corpus al final de esta sesión:

- `40` fuentes públicas
- `69` claims públicos
- `30` notas analíticas públicas
- cobertura inicial para 14 candidaturas
- watchlist operativa de 6 candidatos

## Family Brain

Ya se guardó en `Family Brain`, bajo `project = presidentes_colombia_2026`:

- memoria base del proyecto
- fuentes públicas cargadas
- notas analíticas cargadas
- decisiones técnicas de GitHub Actions y deployment
- pautas de uso intensivo para futuras corridas

La automatización diaria fue actualizada para que también guarde en Family Brain:

- cada fuente nueva
- cada análisis nuevo
- junto con candidato, fecha, URL o relación con claim

Instrucción operativa persistente:

- en toda nueva sesión, Codex debe empezar consultando `Family Brain`
- usar estos markdowns como mapa rápido, no como reemplazo del contexto acumulado

## Pendientes Editoriales

- profundizar backfill de candidatos secundarios
- mantener especial densidad analítica en la watchlist de 6
- seguir ampliando temas como política internacional, salud, empleo y derechos
- registrar también en Family Brain vacíos de investigación y contradicciones potenciales descartadas por evidencia insuficiente
- seguir curando `fuentes por clasificar` para convertirlas en fuentes trazables cuando aparezca metadata suficiente

## Pendientes Técnicos Inmediatos

- extender el adapter público compartido para que comparador y fichas consuman más directamente `candidate_analysis`, `comparison_report` y `editorial_package`
- reforzar el estado incremental para reruns parciales por candidato y por fuente
- seguir ampliando reglas y cobertura del validador metodológico
- decidir si la promoción automática desde `data/added_manually/` necesita fetch de metadata más profundo para aumentar la tasa de fuentes promovidas
- monitorear la primera corrida desatendida posterior al hardening con `scripts/check_daily_automation_health.R --date=YYYY-MM-DD --max-age-hours=30 --notify`; si pasa, el pendiente operativo queda cerrado
- si una corrida falla antes de finalizar, revisar la worktree temporal bajo `.automation-worktrees/`; no debe dejarse como rama de trabajo permanente
