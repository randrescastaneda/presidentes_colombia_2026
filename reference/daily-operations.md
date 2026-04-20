# Uso diario mínimo

## Flujo actual

1. Crear lote:

```bash
Rscript scripts/create_daily_batch.R
```

2. Llenar:

- `data/inbox/YYYY-MM-DD/sources.csv`
- `data/inbox/YYYY-MM-DD/source_texts/`
- `data/inbox/YYYY-MM-DD/claims.csv` mientras dura la transición

3. Ejecutar:

```bash
Rscript scripts/run_daily_update.R
```

4. Revisar:

- `data/processed/`
- `data/public/`
- `data/staging/`
- `data/state/`
- `docs/`

## Flujo objetivo

Cuando la siguiente fase esté implementada, `claims.csv` dejará de ser el insumo manual principal y pasará a derivarse del extractor estructurado.
