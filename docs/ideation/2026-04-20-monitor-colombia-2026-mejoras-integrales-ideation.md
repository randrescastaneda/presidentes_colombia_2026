---
date: 2026-04-20
topic: monitor-colombia-2026-mejoras-integrales
focus: mejoras técnicas, analíticas y de presentación del monitor presidencial
mode: repo-grounded
recovered_from: codex-session-history
---

# Ideation: Mejoras integrales para el monitor Colombia 2026

> Recovery note: este documento fue restaurado desde trazas verificables del historial local de sesiones de Codex el 2026-04-20. El cuerpo principal y la lista de ideas se recuperaron textualmente desde la salida de una sesión previa. Si existía una sección final adicional fuera del fragmento preservado en los logs, hoy no está disponible en el repo ni en Git.

## Grounding Context

### Codebase Context

- El repo ya tiene una arquitectura analítica por etapas bastante madura: `source_packet -> extraction_result -> candidate_analysis -> comparison_report -> editorial_package -> validation_report`.
- El gate metodológico ya es real y está funcionando: `data/public/validation_report.json` está en `pass`, con trazabilidad, separación descripción/inferencia/evaluación, comparación simétrica, manejo explícito de incertidumbre y control de lenguaje partidista.
- La web pública todavía consume una capa relativamente delgada de esa producción:
  - `index.qmd`, `comparador.qmd`, `cronologia.qmd` y `fuentes.qmd` leen sobre todo tablas procesadas (`claim_records.csv`, `source_records.csv`, `analysis_notes.csv`).
  - Las páginas por candidato usan `claim_records.csv` y `analysis_notes.csv`, no el artefacto rico de `candidate_analysis`.
  - Ya existen artefactos públicos infrautilizados: `candidate_analysis_summary.json`, `comparison_report.json`, `editorial_packages.json`, `editorial_package_index.json`, `homepage-brief` y `daily-update`.
- El frontend actual tiene una dirección visual razonable (`docs/styles.css` ya define identidad, tipografía y cards), pero varias páginas siguen resolviendo información compleja con tablas largas y navegación plana.
- `docs/search.json` ya existe, pero `_quarto.yml` no está explotando búsqueda, listings ni navegación más orientada a tareas.
- `Family Brain` deja dos señales estratégicas:
  - la transición operativa del pipeline ya se cerró y el stack actual es estable;
  - sigue habiendo un vacío editorial real en cobertura y backfill de candidatos secundarios, además de densidad desigual por tema y candidato.

### Past Learnings

- No encontré `docs/solutions/` ni learnings locales equivalentes que ya hubieran capturado una ideación reciente de este tema.

### External Context

- Quarto ya soporta búsqueda full-text y listings con filtro, orden y paginación, lo que abre una ruta de mejora clara para fuentes, cronologías, briefs y comparativas sin migrar de stack:
  - [Website Search](https://quarto.org/docs/websites/website-search.html)
  - [Document Listings](https://quarto.org/docs/websites/website-listings.html)
- Datawrapper insiste en que una visualización o tabla pública debe llevar contexto editorial visible: título, descripción, notas, byline, fuente y descripción alternativa para accesibilidad. Esa disciplina aplica bien a este monitor aunque no use Datawrapper directamente:
  - [Annotating your visualization](https://www.datawrapper.de/academy/annotate-tab)
- Datawrapper también advierte contra esconder el hallazgo principal detrás de interacción innecesaria; si algo es importante, debe estar visible sin exigir clicks, hover o filtros:
  - [Our explanatory approach to visualizing data](https://www.datawrapper.de/academy/our-explanatory-approach)
- La literatura de data journalism enfatiza que el dato sin contexto induce errores: hace falta publicar metadata, limitaciones, proceso de recolección y qué mide realmente cada dataset o fuente:
  - [Putting data back into context](https://datajournalism.com/read/longreads/putting-data-back-into-context)

## Ranked Ideas

### 1. Reconstruir la web pública alrededor de los artefactos analíticos ya existentes
**Description:** Hacer que homepage, páginas de candidato y comparador lean directamente `candidate_analysis`, `comparison_report` y `editorial_package`, no solo tablas planas derivadas. La UI debería exponer explícitamente bloques de descripción, inferencia, evaluación, factibilidad e incertidumbre ya presentes en esos contratos.
**Rationale:** Hoy la mayor brecha del repo no es falta de backend sino falta de traducción producto-visible de lo que el pipeline ya genera. Esta idea captura más valor de trabajo ya hecho y reduce duplicación conceptual entre pipeline y frontend.
**Downsides:** Exige refactor de `site_helpers.R`, rediseño de varias páginas y más disciplina para estabilizar view-models públicos sobre esquemas todavía jóvenes.
**Confidence:** 92%
**Complexity:** High
**Status:** Unexplored

### 2. Publicar una matriz de cobertura, frescura y suficiencia de evidencia
**Description:** Crear una vista transversal `candidato x tema núcleo` que muestre cuántas fuentes y claims hay, fecha del último hallazgo, mix de tiers, existencia de propuesta concreta, vacíos de implementación e incertidumbre abierta.
**Rationale:** Resuelve un problema editorial real ya identificado en `Family Brain`: densidad desigual y backfill incompleto. También convierte la ausencia de evidencia en información útil para el lector y para el operador del sistema.
**Downsides:** Si se presenta mal, puede parecer un ranking encubierto o castigar a candidatos con menor exposición mediática en vez de explicar la limitación.
**Confidence:** 90%
**Complexity:** Medium
**Status:** Unexplored

### 3. Añadir un tracker de cambio, contradicción y aumento de especificidad
**Description:** Pasar de cronología plana a cronología interpretada: detectar cuándo un candidato mantiene, refuerza, matiza, vuelve más específica o tensiona una postura previa sobre el mismo tema o `policy_key`.
**Rationale:** Para un monitor electoral, el movimiento de campaña importa tanto como la foto estática. Ya existe suficiente estructura en claims, fechas y análisis para intentar esta capa.
**Downsides:** Riesgo alto de falsos positivos si se fuerza matching semántico débil. Requiere reglas editoriales conservadoras y etiquetado explícito de `potencial tensión` frente a `contradicción confirmada`.
**Confidence:** 84%
**Complexity:** High
**Status:** Unexplored

### 4. Incorporar fichas de contexto de fuente y `data biography` pública
**Description:** Enriquecer la biblioteca de fuentes y las páginas por candidato con tarjetas que expliquen qué tipo de fuente es, qué puede sostener razonablemente, cómo fue capturada, si hay texto completo, sus limitaciones, y por qué entra al sistema.
**Rationale:** El proyecto promete rigor y trazabilidad. Hoy la URL existe, pero falta más contexto sobre el carácter epistemológico de cada fuente. Esto fortalecería la confianza pública y bajaría el riesgo de sobrelectura.
**Downsides:** Añade trabajo editorial y puede volver más pesada la ingestión si no se automatiza parte del metadata scaffold.
**Confidence:** 86%
**Complexity:** Medium
**Status:** Unexplored

### 5. Crear una capa de observabilidad operativa y reruns parciales
**Description:** Exponer un panel técnico y utilidades CLI para ver estado por etapa, conteos por artefacto, fuentes que fallaron extracción, checks de validación por corrida y reruns por `source_id`, `candidate_id` o fecha.
**Rationale:** La arquitectura ya dejó de ser trivial. A medida que crece la cobertura, el costo de depurar una corrida completa sube. Esta idea reduce fricción operativa y prepara mejor el repo para automatización diaria más confiable.
**Downsides:** Valor más interno que público en el corto plazo. Si se diseña mal, puede fragmentar la interfaz operativa en vez de simplificarla.
**Confidence:** 82%
**Complexity:** High
**Status:** Unexplored

### 6. Blindar contratos y semántica pública con una suite de regresión
**Description:** Añadir tests para esquemas JSON, publicación condicionada por `validation_report`, smoke tests de render, golden tests de páginas clave y asserts sobre invariantes públicos importantes.
**Rationale:** El repo serializa muchos artefactos y genera muchas páginas. Ahora mismo la complejidad ya justifica tratar la semántica pública como API estable. Esta es una mejora de baja visibilidad, pero de alto apalancamiento.
**Downsides:** Mantenimiento continuo cuando cambie el copy o evolucionen los contratos. Hay que evitar tests demasiado acoplados al texto exacto.
**Confidence:** 88%
**Complexity:** Medium
**Status:** Unexplored

### 7. Convertir la homepage en un brief editorial y no solo en una portada institucional
**Description:** Promover `homepage-brief` y `daily-update` a la entrada principal del sitio con módulos tipo: `qué cambió hoy`, `dónde falta evidencia`, `comparación rápida de la watchlist`, `estado de validación` y accesos por intención de lectura.
**Rationale:** La home actual es correcta pero genérica. El repo ya genera un resumen ejecutivo y update diario; falta usarlos como producto. Esta idea mejora muchísimo la primera impresión sin exigir una reescritura total del sistema.
**Downsides:** Puede duplicar información si no se define bien la jerarquía entre home, comparador y fichas por candidato.
**Confidence:** 85%
**Complexity:** Medium
**Status:** Unexplored

## Rejection Summary

| # | Idea | Reason Rejected |
|---|------|-----------------|
| 1 | Agregar un chat LLM para conversar con el sitio | Prematuro: la información base todavía necesita mejor estructura pública antes de añadir una capa conversacional. |
| 2 | Integrar encuestas y tracking polling en tiempo real | Añade volatilidad temporal y desplaza el foco del repo desde propuestas trazables hacia señal coyuntural. |
| 3 | Crear un ranking global de candidatos | Choca con las reglas editoriales explícitas del proyecto y empobrece una lectura que quiere ser analítica, no score-driven. |
| 4 | Migrar el frontend completo a Next.js o similar | Demasiado costoso frente al leverage disponible dentro de Quarto + R, que todavía tiene margen claro de mejora. |
| 5 | Añadir comentarios públicos o comunidad | Introduce moderación, ruido y riesgo reputacional sin resolver primero el problema principal de claridad informativa. |
| 6 | Internacionalizar el sitio a múltiples idiomas | Interesante, pero hoy no ataca el cuello de botella principal de producto ni de cobertura. |
| 7 | Hacer dashboards muy interactivos para todo | Choca con el principio de visualización explicativa: esconder lo importante detrás de filtros y hover degradaría comprensión. |
| 8 | Automatizar exports diarios a PDF o WhatsApp | Puede ser útil después, pero ahora sería distribución de una experiencia todavía subóptima. |
| 9 | Reordenar la watchlist automáticamente con una fórmula | Introduce una decisión editorial sensible sin suficiente justificación metodológica ni necesidad inmediata. |
| 10 | Añadir mapas y capas geoespaciales por defecto | No está bien sustentado por el corpus actual; correría el riesgo de producir visualización vistosa con poco valor analítico. |
| 11 | Priorizar social cards, OG images y growth loops | Mejora cosmética aguas abajo; el mayor retorno está en arquitectura pública y legibilidad analítica. |
| 12 | Montar una app móvil o PWA dedicada | Sobredimensionado para el estado actual del producto. Primero hay que resolver navegación, foco y claridad del sitio web. |
| 13 | Añadir login, favoritos y personalización por usuario | No hay evidencia de necesidad y complica innecesariamente un proyecto que hoy debe maximizar auditabilidad pública simple. |
| 14 | Publicar una newsletter separada del sitio | Buena derivada futura, pero antes conviene consolidar el brief diario dentro del propio producto web. |
| 15 | Enriquecer todo con scraping en tiempo real | El cuello de botella no es latencia sino rigor, clasificación y exposición útil del material ya procesado. |

## Recovery Summary

- El documento recuperado deja una priorización bastante clara: el mayor retorno esperado estaba en exponer mejor la capa analítica ya existente, no en cambiar de stack ni en añadir features de growth o interacción social.
- Las 7 ideas supervivientes se ordenan alrededor de tres frentes:
  - producto público basado en artefactos analíticos
  - legibilidad editorial de cobertura, cambio e incertidumbre
  - endurecimiento operativo del pipeline y de la semántica pública
- El slice que ya se ejecutó después de esta ideación encaja directamente con las ideas `1` y `7`: homepage-first, handoff contextual y capa pública derivada de `homepage_brief`, `comparison_report` y `validation_report`.
