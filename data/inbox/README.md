# Inbox diario

Cada lote diario vive en una carpeta `data/inbox/YYYY-MM-DD/` y debe contener:

- `sources.csv`
- `source_texts/` para guardar texto capturado, transcrito o limpiado por fuente cuando exista
- `batch_status.json` para dejar estado explícito cuando el lote siga pendiente o cuando la revisión termine sin hallazgos publicables

La automatización o el editor deben copiar las plantillas base y llenarlas con hallazgos de las últimas 24 horas.

## Relación con `data/added_manually/`

- `data/added_manually/` sirve para descubrimiento curado de URLs todavía no estructuradas.
- El pipeline puede promover desde allí algunas fuentes hacia el corpus formal cuando pasan validación mínima.
- Aun así, `data/inbox/` sigue siendo la ruta formal y auditable para la ingesta pública trazable por candidato.
- Si una fuente ya está suficientemente clara y estructurada, debe entrar directamente por `data/inbox/YYYY-MM-DD/sources.csv`.

## Estado de transición

- `source_texts/` es ahora el insumo principal para el extractor estructurado.
- Los archivos en `source_texts/` pueden mezclar texto libre con bloques `Structured claims` cuando la curaduría ya haya avanzado.
- Cada archivo en `source_texts/` debería corresponder a un `source_id` y puede partir de `template_source_note.md`.

## Manual operativo completo

Si necesitas una explicación de extremo a extremo sobre cómo funciona el proyecto y qué pasos te toca ejecutar como operador/editor, usa:

- [docs/manual-operativo.md](/Volumes/FRC%20SSD%20990PRO/projects/Research-topics/presidentes_colombia_2026/docs/manual-operativo.md)

## Reglas mínimas

- Cada `source` público debe tener `url`, `published_at` y `confidence`.
- Si una fuente no es suficientemente trazable, igual puede cargarse para auditoría, pero el pipeline la dejará por fuera de la capa pública.
- El análisis crítico no se carga manualmente: lo genera el pipeline a partir de la extracción y sus fuentes.
- Un lote vacío solo es válido si `batch_status.json` declara `status = "no_findings"` con una nota breve. Un lote scaffold-only en `pending` se considera una corrida incompleta.
