# Inbox diario

Cada lote diario vive en una carpeta `data/inbox/YYYY-MM-DD/` y debe contener:

- `sources.csv`
- `source_texts/` para guardar texto capturado, transcrito o limpiado por fuente cuando exista
- `batch_status.json` para dejar estado explícito cuando el lote siga pendiente o cuando la revisión termine sin hallazgos publicables

La automatización o el editor deben copiar las plantillas base y llenarlas con hallazgos de las últimas 24 horas.

## Estado de transición

- `source_texts/` es ahora el insumo principal para el extractor estructurado.
- Los archivos en `source_texts/` pueden mezclar texto libre con bloques `Structured claims` cuando la curaduría ya haya avanzado.
- Cada archivo en `source_texts/` debería corresponder a un `source_id` y puede partir de `template_source_note.md`.

## Reglas mínimas

- Cada `source` público debe tener `url`, `published_at` y `confidence`.
- Si una fuente no es suficientemente trazable, igual puede cargarse para auditoría, pero el pipeline la dejará por fuera de la capa pública.
- El análisis crítico no se carga manualmente: lo genera el pipeline a partir de la extracción y sus fuentes.
- Un lote vacío solo es válido si `batch_status.json` declara `status = "no_findings"` con una nota breve. Un lote scaffold-only en `pending` se considera una corrida incompleta.
