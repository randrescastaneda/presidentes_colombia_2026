# Uso diario mínimo

## Flujo actual

1. Crear lote:

```bash
Rscript scripts/create_daily_batch.R
```

2. Llenar:

- `data/inbox/YYYY-MM-DD/sources.csv`
- `data/inbox/YYYY-MM-DD/source_texts/`

3. Ejecutar:

```bash
Rscript scripts/run_daily_update.R
```

4. Revisar:

- `data/staging/source_packets/`
- `data/staging/extraction/`
- `data/staging/analysis/`
- `data/staging/comparison/`
- `data/staging/editorial/`
- `data/staging/validation/`
- `data/processed/`
- `data/public/`
- `data/state/`
- `docs/`

5. Confirmar:

- si `data/public/validation_report.json` devuelve `pass` o `pass_with_warnings`, la corrida es publicable
- si devuelve `block`, el render debe detenerse y revisarse antes de publicar

## Flujo operativo real del día

1. `sources.csv` y `source_texts/` alimentan `source_packet`.
2. El extractor materializa `extraction_result`.
3. El pipeline deriva `claim_records` homogéneos.
4. Se construye `candidate_analysis` por candidato.
5. Se construye `comparison_report` para la watchlist activa.
6. Se generan `editorial_package` para perfiles, comparativa, update diario y resumen breve.
7. `validation_report` decide si la publicación sigue o se bloquea.
