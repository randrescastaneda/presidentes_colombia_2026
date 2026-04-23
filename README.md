# Monitor Colombia 2026

Proyecto base para investigar, estructurar y publicar hallazgos sobre las candidaturas presidenciales de Colombia en 2026.

## Qué incluye

- `pipeline` en `R` por etapas para ingestión, extracción estructurada, análisis por candidato, comparación transversal, redacción editorial y validación bloqueante.
- `sitio estático` en `Quarto` para publicar fichas por candidato, comparaciones temáticas, cronología, fuentes y metodología.
- `plantillas de inbox` para que una automatización diaria o un editor carguen nuevos hallazgos.
- `capa contractual` en `config/`, `schemas/`, `prompts/`, `examples/` y `data/state/` para evolucionar hacia un sistema agentic y analítico por etapas.
- `workflow` de publicación para `GitHub Pages`.

## Flujo diario esperado

1. Crear o actualizar una carpeta en `data/inbox/YYYY-MM-DD/`.
2. Llenar `sources.csv` y, cuando aplique, `source_texts/` con texto capturado o limpiado por fuente.
   Si no hubo hallazgos publicables, registrar el resultado en `batch_status.json` en vez de dejar el lote vacío en `pending`.
3. Cuando existan listas curadas de URLs aún no estructuradas, agregarlas a `data/added_manually/`.
   El pipeline las convierte en un ledger auditable, promueve automáticamente las que pasan validación mínima y deja el resto como `fuentes por clasificar`.
4. Ejecutar:

```bash
Rscript scripts/run_daily_update.R
```

5. Revisar `data/staging/`, `data/processed/`, `data/public/`, `data/state/` y `docs/`.
6. Si `validation_report.json` queda en `block`, no se debe renderizar ni publicar el sitio.

## Estructura principal

- `config/`: taxonomía, registro oficial de candidaturas y reglas editoriales.
- `prompts/`: instrucciones versionadas para extractor, analista, comparador, writer, validator y orquestador.
- `schemas/`: contratos JSON para artefactos estructurados intermedios.
- `data/inbox/`: insumos diarios de investigación.
- `data/added_manually/`: listas curadas manualmente de URLs para descubrimiento y promoción semiautomática.
- `data/staging/`: artefactos intermedios por etapa analítica.
- `data/state/`: estado incremental para fuentes y candidatos.
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

## Dirección arquitectónica actual

El repo ya publica un monitor trazable y funcional, pero la dirección vigente es convertirlo en un sistema por etapas:

1. `source_packet`
2. `extraction_result`
3. `candidate_analysis`
4. `comparison_report`
5. `editorial_package`
6. `validation_report`
7. `render y publicación`

La extracción estructurada ya es el insumo operativo del pipeline. La interfaz interna pasa por `source_packet`, `extraction_result`, `candidate_analysis`, `comparison_report` y `editorial_package`. La publicación pública queda bloqueada si falla `validation_report`.

## Ingesta Manual De Fuentes

El repo ahora tiene una capa adicional de descubrimiento en `data/added_manually/`:

- sirve para capturar listas heterogéneas de URLs sin exigir `sources.csv` desde el primer momento
- genera un ledger auditable en `data/state/manual_source_registry.csv`
- promueve automáticamente solo las URLs válidas con metadata mínima
- deja visibles las URLs válidas pero todavía ambiguas en la biblioteca pública de fuentes como `Fuentes por clasificar`

Esto permite ampliar cobertura sin relajar la regla editorial principal: no convertir enlaces débiles o rotos en evidencia pública trazable.

## Contexto Persistente

Para futuras sesiones, el contexto operativo del proyecto quedó documentado en:

- [AGENTS.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/AGENTS.md)
- [PROJECT_CONTEXT.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/PROJECT_CONTEXT.md)

## Fuente oficial base

La lista inicial de 14 fórmulas presidenciales se sembró usando el comunicado oficial de Registraduría del 25 de marzo de 2026:

- [Conozca la posición de los candidatos presidenciales en la tarjeta electoral](https://www.registraduria.gov.co/Conozca-la-posicion-de-los-candidatos-presidenciales-en-la-tarjeta-electoral.html)

La `watchlist` activa de 6 candidatos es una configuración operativa inicial y puede reajustarse con nuevas encuestas o decisiones editoriales.
