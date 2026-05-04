#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: scripts/prepare_daily_automation_worktree.sh [--date YYYY-MM-DD] [--source-repo PATH]

Creates a clean, dedicated git worktree from origin/main for the daily automation.
The automation should do all source research and file edits inside the printed
WORKTREE_PATH, then finish with scripts/finalize_daily_automation_worktree.sh.
EOF
}

review_date="$(date +%F)"
source_repo=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --date)
      review_date="${2:?Missing value for --date}"
      shift 2
      ;;
    --source-repo)
      source_repo="${2:?Missing value for --source-repo}"
      shift 2
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

if [[ -z "$source_repo" ]]; then
  source_repo="$(git rev-parse --show-toplevel)"
fi

source_repo="$(cd "$source_repo" && pwd)"
repo_name="$(basename "$source_repo")"
safe_repo_name="$(printf '%s' "$repo_name" | tr -c 'A-Za-z0-9._-' '-')"
worktree_base="${DAILY_AUTOMATION_WORKTREE_BASE:-$(dirname "$source_repo")/.automation-worktrees}"
worktree_path="$worktree_base/${safe_repo_name}-${review_date}"
branch_name="codex/daily-automation-${review_date}-$(date +%Y%m%d%H%M%S)"

if ! git -C "$source_repo" remote get-url origin >/dev/null 2>&1; then
  echo "The source repo has no origin remote: $source_repo" >&2
  exit 1
fi

if [[ -e "$worktree_path" ]]; then
  if git -C "$worktree_path" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    if [[ -n "$(git -C "$worktree_path" status --porcelain)" ]]; then
      echo "Refusing to replace non-clean automation worktree: $worktree_path" >&2
      exit 1
    fi

    old_branch="$(git -C "$worktree_path" branch --show-current || true)"
    if [[ -n "$old_branch" ]] && git -C "$source_repo" show-ref --verify --quiet "refs/heads/$old_branch"; then
      git -C "$source_repo" fetch origin main
      if ! git -C "$source_repo" merge-base --is-ancestor "$old_branch" origin/main; then
        echo "Refusing to replace automation worktree with unintegrated branch: $old_branch" >&2
        exit 1
      fi
    fi

    git -C "$source_repo" worktree remove "$worktree_path"

    if [[ -n "$old_branch" ]] && git -C "$source_repo" show-ref --verify --quiet "refs/heads/$old_branch"; then
      git -C "$source_repo" branch -d "$old_branch" >/dev/null
    fi
  else
    echo "Refusing to overwrite non-git path: $worktree_path" >&2
    exit 1
  fi
fi

mkdir -p "$worktree_base"
git -C "$source_repo" fetch origin main
git -C "$source_repo" worktree add -b "$branch_name" "$worktree_path" origin/main

cat <<EOF
WORKTREE_PATH=$worktree_path
AUTOMATION_BRANCH=$branch_name
REVIEW_DATE=$review_date
NEXT_STEP=cd "$worktree_path"
FINALIZE_COMMAND=scripts/finalize_daily_automation_worktree.sh --worktree "$worktree_path" --date "$review_date"
EOF
