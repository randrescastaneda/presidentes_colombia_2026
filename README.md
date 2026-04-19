# Monitor Colombia 2026

Proyecto base para investigar, estructurar y publicar hallazgos sobre las candidaturas presidenciales de Colombia en 2026.

## Qué incluye

- `pipeline` en `R` para validar taxonomía, filtrar registros públicos trazables, detectar análisis conservadores y materializar artefactos.
- `sitio estático` en `Quarto` para publicar fichas por candidato, comparaciones temáticas, cronología, fuentes y metodología.
- `plantillas de inbox` para que una automatización diaria o un editor carguen nuevos hallazgos.
- `workflow` de publicación para `GitHub Pages`.

## Flujo diario esperado

1. Crear o actualizar una carpeta en `data/inbox/YYYY-MM-DD/`.
2. Llenar `sources.csv` y `claims.csv` con hallazgos de las últimas 24 horas.
3. Ejecutar:

```bash
Rscript scripts/run_daily_update.R
```

4. Revisar `data/processed/`, `data/public/` y `docs/`.

## Estructura principal

- `config/`: taxonomía, registro oficial de candidaturas y reglas editoriales.
- `data/inbox/`: insumos diarios de investigación.
- `data/processed/`: tablas consolidadas listas para auditoría.
- `data/public/`: JSON usados por el sitio público.
- `R/`: funciones del pipeline y helpers del sitio.
- `scripts/`: comandos de render, bootstrap de lotes diarios y generación de páginas.
- `candidatos/`: páginas públicas por candidato, generadas a partir del registro.

## Cómo Pensar Este Repo

La forma correcta de pensar este proyecto es separar `fuente`, `artefacto público` y `deployment`:

- `main` es la rama de trabajo real.
  Aquí viven el código en `R`, la taxonomía, la configuración editorial, el inbox diario, los tests, los `.qmd` y también el render local en `docs/`.
- `docs/` en `main` es el artefacto renderizado para auditoría local.
  Sirve para abrir el sitio en local, revisar cambios antes de publicar y conservar una versión visible del estado actual.
- `gh-pages` es solo la rama de publicación.
  No es una rama de desarrollo. Su función es servir el sitio público en GitHub Pages desde la raíz de la rama.
- GitHub Actions toma `main`, corre `Rscript scripts/run_daily_update.R` y publica a `gh-pages`.
  El sitio público no debe editarse manualmente en `gh-pages` salvo una intervención técnica puntual para destrabar Pages.
- En estado normal no debe existir una `worktree` extra.
  Si alguna vez se usa una worktree para bootstrap o reparación de `gh-pages`, debe eliminarse al terminar.

En resumen:

- desarrolla en `main`
- revisa el render en `docs/`
- deja que Actions publique a `origin/gh-pages`
- evita trabajar manualmente en `gh-pages`

## Contexto Persistente

Para futuras sesiones, el contexto operativo del proyecto quedó documentado en:

- [AGENTS.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/AGENTS.md)
- [PROJECT_CONTEXT.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/PROJECT_CONTEXT.md)

## Fuente oficial base

La lista inicial de 14 fórmulas presidenciales se sembró usando el comunicado oficial de Registraduría del 25 de marzo de 2026:

- [Conozca la posición de los candidatos presidenciales en la tarjeta electoral](https://www.registraduria.gov.co/Conozca-la-posicion-de-los-candidatos-presidenciales-en-la-tarjeta-electoral.html)

La `watchlist` activa de 6 candidatos es una configuración operativa inicial y puede reajustarse con nuevas encuestas o decisiones editoriales.
