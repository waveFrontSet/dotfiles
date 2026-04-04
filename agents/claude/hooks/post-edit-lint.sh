#!/usr/bin/env bash
# Post-edit lint hook for Claude Code.
# Reads tool input JSON from stdin, extracts file path, runs appropriate linters.
# Exit 0 = pass, Exit 2 = block (stderr fed back to Claude).
set -euo pipefail

INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

if [[ -z "$FILE_PATH" || ! -f "$FILE_PATH" ]]; then
  exit 0
fi

EXT="${FILE_PATH##*.}"
ERRORS=""

run_lint() {
  local output
  if output=$("$@" 2>&1); then
    return 0
  else
    ERRORS+="$output"$'\n'
    return 1
  fi
}

case "$EXT" in
py)
  run_lint ruff check --no-fix "$FILE_PATH" || true
  run_lint mypy "$FILE_PATH" || true
  ;;
nix)
  run_lint nixfmt --check "$FILE_PATH" || true
  run_lint statix check "$FILE_PATH" || true
  ;;
go)
  DIR=$(dirname "$FILE_PATH")
  run_lint go vet "$DIR/..." || true
  run_lint staticcheck "$DIR/..." || true
  ;;
hs)
  run_lint hlint "$FILE_PATH" || true
  ;;
rs)
  MANIFEST=$(cd "$(dirname "$FILE_PATH")" && cargo locate-project --message-format=plain 2>/dev/null || true)
  if [[ -n "$MANIFEST" ]]; then
    run_lint cargo clippy --manifest-path "$MANIFEST" --quiet || true
  fi
  ;;
*)
  exit 0
  ;;
esac

if [[ -n "$ERRORS" ]]; then
  echo "$ERRORS" >&2
  exit 2
fi
