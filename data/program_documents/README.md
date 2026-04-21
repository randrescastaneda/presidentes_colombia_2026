# Corpus de programas oficiales

Este directorio guarda el corpus persistente de programas oficiales usado por el pipeline.

## Estructura

- `program_documents.csv`: registro maestro de documentos oficiales.
- `files/<candidate_id>/`: artefactos descargados y convertidos.

## Convención mínima por fila

- `document_id`: id estable del documento.
- `source_id`: id trazable que también entra al pipeline como fuente pública.
- `candidate_id`: candidato asociado.
- `document_role`: por ejemplo `programa-base`, `anexo`, `plan-sectorial`.
- `is_primary`: `TRUE` si es el documento base para comparación.
- `official_page_url`: página oficial donde se presenta el documento.
- `download_url`: URL final del archivo o página oficial.
- `source_name`: nombre de campaña o sitio oficial.
- `title`: título público del documento.
- `published_at`: fecha/hora de publicación o captura.
- `discovery_method`: por ejemplo `computer_use`, `manual_curated`.
- `download_status`: por ejemplo `downloaded`, `pending`, `failed`.
- `conversion_status`: por ejemplo `converted`, `partial`, `needs_review`.
- `pdf_path`: ruta repo-relativa al PDF local si existe.
- `markdown_path`: ruta repo-relativa al Markdown local.
- `notes`: observaciones editoriales breves.

## Flujo operativo

1. Confirmar la URL oficial del documento.
2. Ejecutar `python3 scripts/prepare_program_document.py ...`.
3. Verificar el Markdown generado en `files/<candidate_id>/`.
4. Ejecutar `Rscript scripts/run_daily_update.R`.
