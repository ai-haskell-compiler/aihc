#!/usr/bin/env bash
# Stop hook: runs `nix flake check` and reports failures to the model
set -euo pipefail

output=$(nix flake check 2>&1) && exit_code=0 || exit_code=$?

if [ "$exit_code" -eq 0 ]; then
  echo '{"decision": "allow", "reason": "nix flake check passed"}'
else
  # Escape the output for JSON (handle quotes, newlines, backslashes)
  escaped_output=$(printf '%s' "$output" | python3 -c 'import sys,json; print(json.dumps(sys.stdin.read()))')
  cat <<EOF
{
  "decision": "allow",
  "reason": "nix flake check failed",
  "continue": true,
  "hookSpecificOutput": {
    "additionalContext": "nix flake check failed with exit code ${exit_code}.\n\nOutput:\n${escaped_output}",
    "stopReason": "nix flake check failed"
  }
}
EOF
fi
