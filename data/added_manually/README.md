# Ingesta manual de fuentes

`data/added_manually/` es un canal de descubrimiento curado manualmente.

Aquí se pueden dejar archivos de texto con URLs que potencialmente contengan información útil para el monitoreo.

## Qué hace el pipeline

En cada corrida:

1. escanea los archivos dentro de `data/added_manually/`
2. extrae y normaliza URLs
3. valida cuáles siguen respondiendo
4. intenta una clasificación básica por candidato y tipo de fuente
5. guarda el resultado completo en `data/state/manual_source_registry.csv`
6. promueve automáticamente al corpus público solo las URLs que pasan validación mínima
7. deja las URLs válidas pero todavía ambiguas en la biblioteca pública como `Fuentes por clasificar`

## Reglas prácticas

- Este directorio no reemplaza `data/inbox/`.
- `data/inbox/` sigue siendo la ruta formal de entrada al pipeline público trazable por candidato.
- Enlaces rotos o inválidos no se publican.
- Enlaces válidos pero sin metadata suficiente pueden seguir visibles como `por clasificar`.
- Si una URL ya existe en otra lista, el sistema intenta deduplicarla por URL normalizada.

## Formato recomendado

- Un enlace por línea funciona bien.
- También se admiten listas Markdown con encabezados o notas breves.
- No hace falta estructurar metadatos manualmente para entrar al ledger básico.
