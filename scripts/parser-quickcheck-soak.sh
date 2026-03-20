#!/usr/bin/env bash
set -euo pipefail

tests_per_property=10000
pull_every=1
failure_log=""

usage() {
  cat <<'EOF'
Usage: parser-quickcheck-soak.sh [--tests-per-property N] [--pull-every N] [--failure-log PATH]
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --tests-per-property)
      tests_per_property="${2:?missing value for --tests-per-property}"
      shift 2
      ;;
    --pull-every)
      pull_every="${2:?missing value for --pull-every}"
      shift 2
      ;;
    --failure-log)
      failure_log="${2:?missing value for --failure-log}"
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

case "$tests_per_property" in
  ''|*[!0-9]*)
    echo "--tests-per-property must be a positive integer" >&2
    exit 1
    ;;
esac
case "$pull_every" in
  ''|*[!0-9]*)
    echo "--pull-every must be a positive integer" >&2
    exit 1
    ;;
esac
if [ "$tests_per_property" -le 0 ] || [ "$pull_every" -le 0 ]; then
  echo "--tests-per-property and --pull-every must be positive integers" >&2
  exit 1
fi

repo_root="$(git rev-parse --show-toplevel 2>/dev/null)" || {
  echo "Run this app from inside the repository." >&2
  exit 1
}
git_dir="$(git rev-parse --absolute-git-dir 2>/dev/null)" || {
  echo "Unable to locate the repository git directory." >&2
  exit 1
}
cd "$repo_root"

state_dir="$git_dir/quickcheck-soak"
mkdir -p "$state_dir"
failure_log="${failure_log:-$state_dir/failures.jsonl}"
reported_fingerprints="$state_dir/reported-fingerprints.txt"
touch "$reported_fingerprints"

stop_requested=0
trap 'stop_requested=1' INT TERM

batch_counter=0

report_failure() {
  local failure_json="$1"
  local title
  local timestamp
  local fingerprint
  local body_file

  fingerprint="$(printf '%s\n' "$failure_json" | jq -r '.fingerprint // empty')"
  if [ -n "$fingerprint" ] && grep -Fqx "$fingerprint" "$reported_fingerprints"; then
    return 0
  fi

  timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  title="$(printf '%s\n' "$failure_json" | jq -r '"test(parser-quickcheck): \(.propertyName) failure [\(.fingerprint)]"')"
  body_file="$(mktemp "$state_dir/issue-body.XXXXXX")"

  printf '%s\n' "$failure_json" \
    | jq -r --arg timestamp "$timestamp" '
        [
          "Timestamp: " + $timestamp,
          "Commit: " + (.commitSha // "unknown"),
          "Property: " + .propertyName,
          "Status: " + .status,
          "Seed: " + (.seed | tostring),
          "Tests per property: " + (.configuredMaxSuccess | tostring),
          "",
          "Reproduction:",
          .reproductionCommand,
          "",
          "Failure transcript:",
          (.failureTranscript // "(missing)")
        ] | join("\n")
      ' >"$body_file"

  if [ "${AIHC_DISABLE_GH:-0}" = "1" ]; then
    printf '%s\n' "$failure_json" >>"$failure_log"
  elif command -v gh >/dev/null 2>&1; then
    if gh issue list --state open --limit 200 --json title \
      | jq -e --arg title "$title" 'any(.[]; .title == $title)' >/dev/null; then
      :
    elif gh issue create --title "$title" --body-file "$body_file" >/dev/null 2>&1; then
      :
    else
      printf '%s\n' "$failure_json" >>"$failure_log"
    fi
  else
    printf '%s\n' "$failure_json" >>"$failure_log"
  fi

  if [ -n "$fingerprint" ]; then
    printf '%s\n' "$fingerprint" >>"$reported_fingerprints"
  fi
  rm -f "$body_file"
}

while :; do
  batch_counter=$((batch_counter + 1))
  batch_output="$(mktemp "$state_dir/batch.XXXXXX.json")"
  batch_status=0

  if nix run .#parser-quickcheck-batch -- --max-success "$tests_per_property" --json >"$batch_output"; then
    batch_status=0
  else
    batch_status=$?
  fi

  if ! jq -e '.results | type == "array"' "$batch_output" >/dev/null 2>&1; then
    echo "parser-quickcheck-batch did not emit valid JSON." >&2
    cat "$batch_output" >&2 || true
    rm -f "$batch_output"
    exit "${batch_status:-1}"
  fi

  jq -c '.results[] | select(.status != "PASS")' "$batch_output" \
    | while IFS= read -r failure_line; do
        failure_payload="$(jq -c --argjson result "$failure_line" '{commitSha, batchSeed, configuredMaxSuccess, generatedAt} + $result' "$batch_output")"
        report_failure "$failure_payload"
      done

  if [ "$stop_requested" -ne 0 ]; then
    rm -f "$batch_output"
    exit 0
  fi

  if [ $((batch_counter % pull_every)) -eq 0 ]; then
    if ! git pull --ff-only; then
      echo "git pull --ff-only failed after batch $batch_counter; stopping." >&2
      rm -f "$batch_output"
      exit 1
    fi
  fi

  rm -f "$batch_output"

  if [ "$stop_requested" -ne 0 ]; then
    exit 0
  fi
done
