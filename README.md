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

## Fuente oficial base

La lista inicial de 14 fórmulas presidenciales se sembró usando el comunicado oficial de Registraduría del 25 de marzo de 2026:

- [Conozca la posición de los candidatos presidenciales en la tarjeta electoral](https://www.registraduria.gov.co/Conozca-la-posicion-de-los-candidatos-presidenciales-en-la-tarjeta-electoral.html)

La `watchlist` activa de 6 candidatos es una configuración operativa inicial y puede reajustarse con nuevas encuestas o decisiones editoriales.
