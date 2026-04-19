# Inbox diario

Cada lote diario vive en una carpeta `data/inbox/YYYY-MM-DD/` y debe contener:

- `sources.csv`
- `claims.csv`

La automatización o el editor deben copiar las plantillas base y llenarlas con hallazgos de las últimas 24 horas.

## Reglas mínimas

- Cada `claim` debe apuntar a un `source_id` existente.
- Cada `source` público debe tener `url`, `published_at` y `confidence`.
- Si una fuente no es suficientemente trazable, igual puede cargarse para auditoría, pero el pipeline la dejará por fuera de la capa pública.
- El análisis crítico no se carga manualmente: lo genera el pipeline a partir de los claims y sus fuentes.
