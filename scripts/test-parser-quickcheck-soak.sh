#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
soak_script="$repo_root/scripts/parser-quickcheck-soak.sh"
original_path="$PATH"
system_bash="$(command -v bash)"
system_git="$(command -v git)"

make_repo() {
  local repo_dir="$1"
  mkdir -p "$repo_dir/stubs"
  "$system_git" init -q "$repo_dir"
}

write_common_git_stub() {
  local repo_dir="$1"
  local path="$repo_dir/stubs/git"
  cat >"$path" <<EOF
#!$system_bash
set -euo pipefail
repo_dir="$repo_dir"
system_git="$system_git"
case "\$1" in
  rev-parse)
    cd "\$repo_dir"
    exec "\$system_git" "\$@"
    ;;
  pull)
    state_file="\$repo_dir/pull-count"
    count=0
    if [ -f "\$state_file" ]; then
      count="\$(cat "\$state_file")"
    fi
    count=\$((count + 1))
    printf '%s\n' "\$count" >"\$state_file"
    if [ -n "\${FAKE_GIT_PULL_FAIL_ON:-}" ] && [ "\$count" -ge "\${FAKE_GIT_PULL_FAIL_ON}" ]; then
      echo "simulated git pull failure" >&2
      exit 1
    fi
    ;;
  *)
    echo "unexpected git command: \$*" >&2
    exit 1
    ;;
esac
EOF
  chmod +x "$path"
}

write_nix_stub() {
  local repo_dir="$1"
  local json_file="$2"
  local exit_code="$3"
  local path="$repo_dir/stubs/nix"
  cat >"$path" <<EOF
#!$system_bash
set -euo pipefail
cat "$json_file"
exit $exit_code
EOF
  chmod +x "$path"
}

write_gh_stub() {
  local repo_dir="$1"
  local path="$repo_dir/stubs/gh"
  cat >"$path" <<EOF
#!$system_bash
set -euo pipefail
repo_dir="$repo_dir"
case "\$1 \$2" in
  "issue list")
    count_file="\$repo_dir/gh-issue-list-count"
    count=0
    if [ -f "\$count_file" ]; then
      count="\$(cat "\$count_file")"
    fi
    count=\$((count + 1))
    printf '%s\n' "\$count" >"\$count_file"
    printf '[]\n'
    ;;
  "issue create")
    count_file="\$repo_dir/gh-issue-create-count"
    count=0
    if [ -f "\$count_file" ]; then
      count="\$(cat "\$count_file")"
    fi
    count=\$((count + 1))
    printf '%s\n' "\$count" >"\$count_file"
    body_file=""
    while [ "\$#" -gt 0 ]; do
      case "\$1" in
        --body-file)
          body_file="\${2:?missing body file}"
          shift 2
          ;;
        *)
          shift
          ;;
      esac
    done
    if [ -n "\$body_file" ]; then
      cp "\$body_file" "\$repo_dir/gh-issue-body.md"
    fi
    printf 'https://example.invalid/issues/%s\n' "\$count"
    ;;
  *)
    echo "unexpected gh command: \$*" >&2
    exit 1
    ;;
esac
EOF
  chmod +x "$path"
}

assert_file_contains() {
  local path="$1"
  local expected="$2"
  if ! grep -Fq "$expected" "$path"; then
    echo "expected '$expected' in $path" >&2
    exit 1
  fi
}

assert_file_not_contains() {
  local path="$1"
  local unexpected="$2"
  if grep -Fq "$unexpected" "$path"; then
    echo "did not expect '$unexpected' in $path" >&2
    exit 1
  fi
}

run_fallback_scenario() {
  local repo_dir
  repo_dir="$(mktemp -d)"
  make_repo "$repo_dir"
  write_common_git_stub "$repo_dir"

  cat >"$repo_dir/failure.json" <<'EOF'
{"generatedAt":"2026-03-20T00:00:00Z","batchSeed":12345,"commitSha":"deadbeef","configuredMaxSuccess":10000,"selectedProperties":["demo"],"results":[{"propertyName":"demo","status":"FAIL","seed":111,"configuredMaxSuccess":10000,"actualTests":1,"actualDiscarded":0,"failureTranscript":"boom","fingerprint":"f00d","reason":"Falsified","reproductionCommand":"nix run .#parser-quickcheck-batch -- --max-success 10000 --seed 12345 --property 'demo'"}]}
EOF
  write_nix_stub "$repo_dir" "$repo_dir/failure.json" 1

  export PATH="$repo_dir/stubs:$original_path"
  export AIHC_DISABLE_GH=1
  export FAKE_GIT_PULL_FAIL_ON=1
  if (cd "$repo_dir" && bash "$soak_script" --tests-per-property 10000 --failure-log "$repo_dir/failures.jsonl" >"$repo_dir/stdout.log" 2>"$repo_dir/stderr.log"); then
    echo "expected fallback scenario to stop on pull failure" >&2
    exit 1
  fi
  unset FAKE_GIT_PULL_FAIL_ON
  unset AIHC_DISABLE_GH

  assert_file_contains "$repo_dir/failures.jsonl" '"fingerprint":"f00d"'
  assert_file_contains "$repo_dir/stdout.log" 'Completed 1 tests across 1 batch.'
  assert_file_contains "$repo_dir/stderr.log" 'NOTICE: parser-quickcheck found FAIL for property demo (fingerprint: f00d, seed: 111)'
}

run_dedupe_scenario() {
  local repo_dir
  repo_dir="$(mktemp -d)"
  make_repo "$repo_dir"
  write_common_git_stub "$repo_dir"
  write_gh_stub "$repo_dir"

  cat >"$repo_dir/failure.json" <<'EOF'
{"generatedAt":"2026-03-20T00:00:00Z","batchSeed":12345,"commitSha":"deadbeef","configuredMaxSuccess":10000,"selectedProperties":["demo"],"results":[{"propertyName":"demo","status":"FAIL","seed":111,"configuredMaxSuccess":10000,"actualTests":1,"actualDiscarded":0,"failureTranscript":"boom","fingerprint":"f00d","reason":"Falsified","reproductionCommand":"nix run .#parser-quickcheck-batch -- --max-success 10000 --seed 12345 --property 'demo'"}]}
EOF
  write_nix_stub "$repo_dir" "$repo_dir/failure.json" 1

  export PATH="$repo_dir/stubs:$original_path"
  export FAKE_GIT_PULL_FAIL_ON=2
  if (cd "$repo_dir" && bash "$soak_script" --tests-per-property 10000 >"$repo_dir/stdout.log" 2>"$repo_dir/stderr.log"); then
    echo "expected dedupe scenario to stop on the second pull failure" >&2
    exit 1
  fi
  unset FAKE_GIT_PULL_FAIL_ON

  assert_file_contains "$repo_dir/gh-issue-create-count" '1'
  if [ -f "$repo_dir/gh-issue-list-count" ]; then
    assert_file_contains "$repo_dir/gh-issue-list-count" '1'
  fi
  assert_file_contains "$repo_dir/gh-issue-body.md" $'Reproduction:\n```'
  assert_file_contains "$repo_dir/gh-issue-body.md" $'Failure transcript:\n```'
  assert_file_contains "$repo_dir/stdout.log" 'Completed 1 tests across 1 batch.'
  assert_file_contains "$repo_dir/stdout.log" 'Completed 2 tests across 2 batches.'
  assert_file_contains "$repo_dir/stderr.log" 'NOTICE: parser-quickcheck found FAIL for property demo (fingerprint: f00d, seed: 111)'
}

run_pull_failure_scenario() {
  local repo_dir
  repo_dir="$(mktemp -d)"
  make_repo "$repo_dir"
  write_common_git_stub "$repo_dir"

  cat >"$repo_dir/pass.json" <<'EOF'
{"generatedAt":"2026-03-20T00:00:00Z","batchSeed":12345,"commitSha":"deadbeef","configuredMaxSuccess":10000,"selectedProperties":["demo"],"results":[{"propertyName":"demo","status":"PASS","seed":111,"configuredMaxSuccess":10000,"actualTests":10000,"actualDiscarded":0,"failureTranscript":null,"fingerprint":null,"reason":null,"reproductionCommand":"nix run .#parser-quickcheck-batch -- --max-success 10000 --seed 12345 --property 'demo'"}]}
EOF
  write_nix_stub "$repo_dir" "$repo_dir/pass.json" 0

  export PATH="$repo_dir/stubs:$original_path"
  export AIHC_DISABLE_GH=1
  export FAKE_GIT_PULL_FAIL_ON=1
  if (cd "$repo_dir" && bash "$soak_script" --tests-per-property 10000 >"$repo_dir/stdout.log" 2>"$repo_dir/stderr.log"); then
    echo "expected pull failure scenario to exit nonzero" >&2
    exit 1
  fi
  unset FAKE_GIT_PULL_FAIL_ON
  unset AIHC_DISABLE_GH

  assert_file_contains "$repo_dir/stdout.log" 'Completed 10000 tests across 1 batch.'
  assert_file_not_contains "$repo_dir/stderr.log" 'Already up to date.'
}

run_fallback_scenario
run_dedupe_scenario
run_pull_failure_scenario
