Scope: current checkout vs `e6b6289dbf6e6a71807ec9bfaf245bceb118c959`

Intent: verify that the homepage-first artifact-driven slice now preserves comparison drill-down context, sanitizes public comparison copy, and degrades safely around missing validation or drifted candidate metadata.

Result:
- No actionable findings remained after re-review.

Coverage:
- Reviewed tracked changes plus manual inspection of untracked `R/site_public_view_models.R` and `tests/testthat/test-site-public-view-models.R`.
- Confirmed rendered output in `docs/index.html` and `docs/candidatos/*.html` reflects contextual drill-down and public-facing comparison copy.
- Re-ran targeted test suites for homepage view-models and site generation.
