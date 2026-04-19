# Project Context

Última actualización: 2026-04-19

## Resumen

Proyecto para monitorear candidaturas presidenciales de Colombia 2026 con `Quarto + R`, publicar un sitio estático y ejecutar una investigación diaria automatizada con trazabilidad de fuentes.

## Snapshot Operativo

- Repo GitHub: `randrescastaneda/presidentes_colombia_2026`
- Rama de desarrollo: `main`
- Rama de publicación: `origin/gh-pages`
- Sitio público: `https://randrescastaneda.github.io/presidentes_colombia_2026/`
- Workflow de publicación: `.github/workflows/publish.yml`
- Automatización diaria activa: `colombia-2026-diario`

## Estado Git Consolidado

Al cierre de esta sesión:

- solo existe una worktree activa, la principal
- la rama local activa es `main`
- `gh-pages` existe como rama remota de deployment
- el árbol local está limpio

Commits importantes:

- `06ebc7f` `Inicializa monitor presidencial Colombia 2026`
- `5da7308` `Corrige workflow de GitHub Pages`

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

## Pendientes Editoriales

- profundizar backfill de candidatos secundarios
- mantener especial densidad analítica en la watchlist de 6
- seguir ampliando temas como política internacional, salud, empleo y derechos
- registrar también en Family Brain vacíos de investigación y contradicciones potenciales descartadas por evidencia insuficiente
