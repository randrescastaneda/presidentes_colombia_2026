# Project Context

Última actualización: 2026-04-19

## Resumen

Proyecto para monitorear candidaturas presidenciales de Colombia 2026 con `Quarto + R`, publicar un sitio estático y ejecutar una investigación diaria automatizada con trazabilidad de fuentes.

## Instrucción Para Nuevas Sesiones

Antes de trabajar en este repo, Codex debe consultar `Family Brain` con `project = presidentes_colombia_2026`. Allí están fuentes, análisis, decisiones técnicas, pendientes editoriales y cambios operativos recientes. Este documento resume el estado del proyecto, pero no reemplaza esa memoria acumulada.

## Snapshot Operativo

- Repo GitHub: `randrescastaneda/presidentes_colombia_2026`
- Rama de desarrollo: `main`
- Rama de publicación: `origin/gh-pages`
- Sitio público: `https://randrescastaneda.github.io/presidentes_colombia_2026/`
- Workflow de publicación: `.github/workflows/publish.yml`
- Automatización diaria activa: `colombia-2026-diario`

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
- `claims.csv` se mantiene solo como fallback de transición
- `candidate_analysis`, `comparison_report` y `editorial_package` ya se escriben en `data/staging/` y `data/public`
- `validation_report` ya es un gate real: si da `block`, `scripts/run_daily_update.R` aborta antes del render público

## Estado Git Consolidado

Al cierre de esta sesión:

- solo existe una worktree activa, la principal
- la rama local activa es `main`
- `gh-pages` existe como rama remota de deployment
- el árbol local está limpio

Commits importantes:

- `06ebc7f` `Inicializa monitor presidencial Colombia 2026`
- `5da7308` `Corrige workflow de GitHub Pages`
- `586d03a` `Documenta operación y contexto del repo`

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

## Pendientes Técnicos Inmediatos

- sustituir por completo el fallback manual de `claims.csv` por extracción estructurada automática
- hacer que la web consuma más directamente `candidate_analysis`, `comparison_report` y `editorial_package`
- reforzar el estado incremental para reruns parciales por candidato y por fuente
- seguir ampliando reglas y cobertura del validador metodológico
