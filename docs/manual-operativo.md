# Manual Operativo Del Proyecto

## Propósito

Este proyecto monitorea candidaturas presidenciales de Colombia 2026 con un pipeline en `R` y un sitio estático en `Quarto`.

La meta no es solo acumular enlaces: es transformar insumos heterogéneos en artefactos públicos con trazabilidad, validación metodológica y una separación clara entre:

- fuente cruda
- artefacto procesado
- artefacto público
- sitio renderizado

Este manual explica cómo pensar el sistema y qué te toca hacer a ti como operador/editor.

## Qué Hace El Proyecto

El repo hace cuatro cosas principales:

1. Recibe nuevas fuentes y textos de trabajo.
2. Extrae claims y artefactos analíticos por candidato.
3. Valida si lo producido se puede publicar.
4. Renderiza y prepara el sitio público.

## Cómo Pensar La Arquitectura

La arquitectura vigente del pipeline es:

1. `source_packet`
2. `extraction_result`
3. `candidate_analysis`
4. `comparison_report`
5. `editorial_package`
6. `validation_report`
7. `data/public` + render de `docs/`

La intuición correcta es esta:

- `data/inbox/` y `data/added_manually/` son entradas.
- `data/staging/` son artefactos intermedios.
- `data/processed/` es la capa auditable consolidada.
- `data/public/` es la capa JSON que consume el sitio.
- `docs/` es el render visible del estado actual.

## Qué Carpetas Importan Más

- `config/`: taxonomía, registro de candidatos y reglas.
- `R/`: lógica del pipeline.
- `scripts/`: comandos operativos.
- `data/inbox/`: entrada formal estructurada.
- `data/added_manually/`: entrada curada para descubrimiento manual.
- `data/staging/`: salidas intermedias por etapa.
- `data/processed/`: tablas consolidadas para revisión.
- `data/public/`: JSON para el sitio.
- `data/state/`: ledger y estado incremental.
- `docs/`: render local/auditable del sitio y documentación interna.

## Qué Te Toca Hacer A Ti

Tu rol operativo normal es uno de estos dos:

1. Cargar evidencia estructurada en `data/inbox/`.
2. Cargar listas curadas de URLs en `data/added_manually/`.

Luego correr el pipeline, revisar resultados y decidir si el estado está listo para commit/publicación.

## Dos Formas De Ingresar Información

### Ruta 1: `data/inbox/` (formal)

Usa esta ruta cuando ya tienes suficiente claridad para crear fuentes trazables por candidato.

Normalmente aquí agregas:

- `data/inbox/YYYY-MM-DD/sources.csv`
- `data/inbox/YYYY-MM-DD/source_texts/*.md`
- opcionalmente `batch_status.json` cuando no hubo hallazgos publicables

Esta es la vía canónica para fuentes que ya están listas para entrar al corpus público.

### Ruta 2: `data/added_manually/` (descubrimiento curado)

Usa esta ruta cuando todavía no quieres o no puedes estructurar `sources.csv`, pero sí tienes listas útiles de URLs.

Aquí puedes poner:

- un archivo `.md`
- listas con un enlace por línea
- listas Markdown con encabezados y notas

No necesitas agregar metadata manual para que entren al ledger básico.

## Qué Pasa Cuando Agregas Un Archivo A `data/added_manually/`

Agregar el archivo **no lo procesa por sí solo**.

Ese contenido entra al sistema cuando corre el pipeline, normalmente por:

```bash
Rscript scripts/run_daily_update.R
```

En cada corrida, el pipeline:

1. escanea `data/added_manually/`
2. extrae y normaliza URLs
3. valida cuáles siguen respondiendo
4. intenta clasificar candidato y tipo de fuente
5. guarda todo en `data/state/manual_source_registry.csv`
6. promueve al corpus las URLs que pasan validación mínima
7. deja las válidas pero ambiguas en `Fuentes por clasificar`

Consecuencia práctica:

- si agregas un archivo localmente, tienes que correr el script para verlo reflejado
- si quieres que lo procese una automatización remota, el archivo debe estar committeado/pusheado en la rama que ejecuta esa automatización

## Flujo Operativo Recomendado

### Caso A: Nueva evidencia ya estructurada

1. Crea o actualiza `data/inbox/YYYY-MM-DD/`.
2. Llena `sources.csv`.
3. Agrega `source_texts/` cuando aplique.
4. Corre:

```bash
Rscript scripts/run_daily_update.R
```

5. Revisa outputs y decide si el estado es publicable.

### Caso B: Tienes solo listas de URLs

1. Agrega un archivo nuevo a `data/added_manually/`.
2. Corre:

```bash
Rscript scripts/run_daily_update.R
```

3. Revisa qué URLs:
   - quedaron en el ledger
   - fueron promovidas
   - quedaron como `Fuentes por clasificar`
4. Si alguna merece pasar a evidencia más fuerte, luego la puedes convertir en entrada formal vía `data/inbox/`.

## Qué Revisar Después De Cada Corrida

Revisión mínima:

- `data/public/validation_report.json`
- `data/processed/site_metadata.csv`
- `data/processed/source_records.csv`
- `data/processed/manual_source_registry.csv`
- `data/processed/manual_source_library.csv`
- `docs/fuentes.html`
- `docs/index.html`

Si trabajaste con `data/added_manually/`, revisa además:

- `data/state/manual_source_registry.csv`

Ahí ves el ledger completo, no solo lo promovido.

## Qué Significa Cada Resultado

- `manual_source_registry.csv`: todo lo encontrado y evaluado desde `data/added_manually/`
- `manual_source_library.csv`: URLs válidas pero todavía pendientes de clasificación suficiente
- `source_records.csv`: fuentes públicas integradas al corpus
- `validation_report.json`: decide si el sitio se debe renderizar/publicar

## Regla De Oro De Publicación

Si `validation_report` queda en `block`, no debes considerar la corrida como lista para publicación.

El script [scripts/run_daily_update.R](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/scripts/run_daily_update.R:10) ya corta el flujo antes del render público cuando pasa eso.

## Qué Hace La Automatización Y Qué No Hace

Hoy la automatización diaria:

- corre el pipeline
- renderiza el sitio
- prepara artefactos públicos

Pero no debes asumir que cualquier archivo nuevo en tu máquina será leído por una automatización remota.

Regla práctica:

- si está solo local, solo lo procesa tu corrida local
- si está committeado y pusheado a la rama correspondiente, lo podrá procesar la automatización que use ese checkout

## Qué No Debes Hacer

- no usar `data/added_manually/` como reemplazo total de `data/inbox/`
- no tratar enlaces rotos como evidencia pública
- no asumir que una URL válida ya quedó bien atribuida a un candidato
- no publicar si la validación bloquea
- no trabajar manualmente en `gh-pages`

## Relación Entre `main`, `docs/` Y `gh-pages`

- `main` es la rama real de trabajo.
- `docs/` en `main` es el render auditable local.
- `gh-pages` es solo la rama de deployment.

La secuencia correcta es:

1. trabajar en `main`
2. correr el pipeline
3. revisar `docs/`
4. hacer commit
5. dejar que Actions publique a `gh-pages`

## Checklist Corto Para Tu Uso Diario

1. ¿Tengo información ya estructurada o solo URLs?
2. Si es estructurada: `data/inbox/`. Si son URLs: `data/added_manually/`.
3. Corre `Rscript scripts/run_daily_update.R`.
4. Revisa `validation_report`, `site_metadata`, `source_records` y `manual_source_registry`.
5. Abre `docs/fuentes.html` y valida si quedó bien.
6. Si todo está correcto, commit y push.

## Checklist Corto Para `data/added_manually/`

1. Agrega el archivo en `data/added_manually/`.
2. Usa texto o Markdown; un enlace por línea funciona bien.
3. Corre el pipeline.
4. Revisa:
   - `data/state/manual_source_registry.csv`
   - `data/processed/manual_source_library.csv`
   - `docs/fuentes.html`
5. Si quieres que lo procese una automatización remota, commit y push.

## Documentos Que Debes Leer Cuando Tengas Dudas

- [README.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/README.md:1)
- [PROJECT_CONTEXT.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/PROJECT_CONTEXT.md:1)
- [AGENTS.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/AGENTS.md:1)
- [data/inbox/README.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/data/inbox/README.md:1)
- [data/added_manually/README.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/data/added_manually/README.md:1)

## Resumen Ejecutivo

Si recuerdas una sola idea, que sea esta:

`data/added_manually/` no se procesa “por existir”; se procesa cuando corre el pipeline. Y lo que entra allí no necesariamente pasa completo al corpus público: primero entra a un ledger, luego se valida, luego se promueve o queda `por clasificar`.
