Scope: current checkout vs `e6b6289dbf6e6a71807ec9bfaf245bceb118c959`

Intent: migrate the homepage to an artifact-driven public view-model, make comparison the primary reading surface, preserve drill-down context, and avoid leaking internal comparison payload into public copy.

Findings:
- P2: `validation_badge_view_model()` eagerly indexes `validation_status$status[[1]]` and `summary[[1]]`, so a zero-row `validation_status.csv` can abort the homepage instead of degrading safely.
- P2: homepage comparison summaries are exposed from `topic_row$summary` without public normalization, which can leak slug-like or pipeline-shaped wording into the public cards.
- P2: candidate drill-down links preserve `topic` only in the URL; the destination page does not consume that context, so the focused handoff still widens to a generic proposals section.
- P2: when `comparison_report.json` references a candidate missing from `candidate_registry.csv`, the homepage builds a fallback candidate URL from the raw ID, which can emit a broken destination instead of degrading explicitly.

Residual risks:
- Hero note sanitization still depends on regex heuristics rather than an explicit public field in the editorial artifact contract.
- The homepage adapter still reads `comparison_report.json` directly, so future schema drift there can bypass `editorial_packages` stability.

Testing gaps:
- No test covers missing or empty `validation_status.csv` while `validation_report.json` is present.
- No rendered-navigation test proves homepage-to-candidate handoff restores topic context beyond the shared proposals anchor.
- No regression test asserts homepage comparison summaries exclude internal candidate slugs or IDs.
