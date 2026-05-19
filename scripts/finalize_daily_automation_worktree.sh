#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: scripts/finalize_daily_automation_worktree.sh --worktree PATH [--date YYYY-MM-DD] [--message MESSAGE] [--keep-worktree]

Validates, commits, pushes, and cleans up a daily automation worktree.
The push target is origin main, so successful automation output is integrated
without leaving a remote feature branch behind.
EOF
}

review_date="$(date +%F)"
worktree_path=""
commit_message=""
keep_worktree=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --date)
      review_date="${2:?Missing value for --date}"
      shift 2
      ;;
    --worktree)
      worktree_path="${2:?Missing value for --worktree}"
      shift 2
      ;;
    --message)
      commit_message="${2:?Missing value for --message}"
      shift 2
      ;;
    --keep-worktree)
      keep_worktree=1
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ -z "$worktree_path" ]]; then
  worktree_path="$(git rev-parse --show-toplevel)"
fi

worktree_path="$(cd "$worktree_path" && pwd)"
if ! git -C "$worktree_path" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "Not a git worktree: $worktree_path" >&2
  exit 1
fi

branch_name="$(git -C "$worktree_path" branch --show-current)"
if [[ -z "$branch_name" ]]; then
  echo "Automation worktree must be on a temporary branch, not detached HEAD." >&2
  exit 1
fi

primary_worktree="$(
  git -C "$worktree_path" worktree list --porcelain |
    awk 'BEGIN { RS=""; FS="\n" } NR == 1 { for (i = 1; i <= NF; i++) if ($i ~ /^worktree /) { sub(/^worktree /, "", $i); print $i } }'
)"

if [[ -z "$commit_message" ]]; then
  commit_message="Actualiza fuentes evaluadas ${review_date}"
fi

git -C "$worktree_path" fetch origin main
if ! git -C "$worktree_path" merge-base --is-ancestor origin/main HEAD; then
  git -C "$worktree_path" rebase --autostash origin/main
fi

(
  cd "$worktree_path"
  Rscript scripts/run_daily_update.R
  if git -C "$worktree_path" diff --name-only --diff-filter=D -- docs/solutions | grep -q .; then
    git -C "$worktree_path" restore --source=HEAD --worktree -- docs/solutions
  fi
  Rscript scripts/verify_daily_automation.R --date="$review_date" --notify
)

git -C "$worktree_path" add -A

if git -C "$worktree_path" diff --cached --quiet; then
  echo "No automation changes to commit for $review_date."
else
  git -C "$worktree_path" commit -m "$commit_message"
  git -C "$worktree_path" push origin HEAD:main
fi

git -C "$worktree_path" fetch origin main

if [[ -n "$primary_worktree" && -d "$primary_worktree" ]]; then
  git -C "$primary_worktree" fetch origin main

  if [[ "$(git -C "$primary_worktree" branch --show-current)" == "main" ]] &&
    [[ -z "$(git -C "$primary_worktree" status --porcelain)" ]]; then
    git -C "$primary_worktree" merge --ff-only origin/main
  else
    echo "Primary worktree was not fast-forwarded because it is not clean on main:"
    echo "$primary_worktree"
  fi
fi

if [[ "$keep_worktree" -eq 0 && -n "$primary_worktree" && -d "$primary_worktree" ]]; then
  cd "$primary_worktree"
  git worktree remove "$worktree_path"

  if git show-ref --verify --quiet "refs/heads/$branch_name"; then
    if git merge-base --is-ancestor "$branch_name" origin/main; then
      git branch -d "$branch_name" >/dev/null
    else
      echo "Temporary branch was not deleted because it is not contained in origin/main: $branch_name" >&2
      exit 1
    fi
  fi
fi

echo "Daily automation finalized for $review_date."
echo "Integrated target: origin/main"
