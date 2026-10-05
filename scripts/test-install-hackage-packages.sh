#!/usr/bin/env bash
# Test the install script with local package fixtures.
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
fixture="$repo_root/test/Test/Fixtures/hackage-install"
work_directory="$(mktemp -d)"
trap 'rm -rf "$work_directory"' EXIT
mkdir -p "$work_directory/bin"
export FIXTURE_DIRECTORY="$fixture"
export FIXTURE_INSTALL_LOG="$work_directory/installed.txt"

cat >"$work_directory/bin/curl" <<'CURL'
#!/usr/bin/env bash
set -euo pipefail
output=""
url=""
while [ "$#" -gt 0 ]; do
  case "$1" in
  --output) output="$2"; shift 2 ;;
  --*) shift ;;
  *) url="$1"; shift ;;
  esac
done
release="${url#*/package/}"
release="${release%%/*}"
if [[ "$url" == *.tar.gz ]]; then
  if [ "$release" = "${FIXTURE_SKIP_RELEASE:-}" ]; then
    echo "Download failure: $release" >&2
    exit 1
  fi
  tar -czf "$output" -C "$FIXTURE_DIRECTORY" "$release"
else
  cp "$FIXTURE_DIRECTORY/$release/demo.cabal" "$output"
fi
CURL

cat >"$work_directory/bin/aihc" <<'AIHC'
#!/usr/bin/env bash
set -euo pipefail
source="$2"
if [ "$source" = core-libs/aihc-base ]; then
  exit 0
fi
version="$(awk '/^version:/ {print $2}' "$source/demo.cabal")"
release="demo-$version"
echo "$release" >>"$FIXTURE_INSTALL_LOG"
shift 2
workspace=""
while [ "$#" -gt 0 ]; do
  case "$1" in
  --workspace) workspace="$2"; shift 2 ;;
  *) shift ;;
  esac
done
test "$(basename "$source")" = "$release"
pinned_version="$(awk '/^version:/ {print $2}' "$workspace/demo/demo.cabal")"
test "$pinned_version" = "${FIXTURE_PINNED_VERSION:-1.0}"
if [ "${FIXTURE_FAIL_INSTALL:-0}" = 1 ]; then
  echo "Install failure: $release"
  exit 1
fi
AIHC

chmod +x "$work_directory/bin/curl" "$work_directory/bin/aihc"
export PATH="$work_directory/bin:$PATH"
export AIHC="$work_directory/bin/aihc"

run_fixture() {
  : >"$FIXTURE_INSTALL_LOG"
  bash "$repo_root/scripts/install-hackage-packages.sh" \
    --list "$fixture/packages.md" --report-dir "$work_directory/reports" \
    >"$work_directory/output.txt" 2>&1
}

run_fixture
diff -u "$fixture/installed.txt" "$FIXTURE_INSTALL_LOG"
test ! -s "$work_directory/reports/failed.txt"

export FIXTURE_FAIL_INSTALL=1
if run_fixture; then
  echo "The fixture did not report install failures." >&2
  exit 1
fi
diff -u "$fixture/installed.txt" "$FIXTURE_INSTALL_LOG"
diff -u "$fixture/installed.txt" "$work_directory/reports/failed.txt"
while read -r release; do
  grep -Fq "Install failure: $release" "$work_directory/reports/$release.md"
done <"$fixture/installed.txt"

export FIXTURE_FAIL_INSTALL=0
export FIXTURE_SKIP_RELEASE=demo-1.0
export FIXTURE_PINNED_VERSION=2.0
if run_fixture; then
  echo "The fixture did not report the download failure." >&2
  exit 1
fi
printf 'demo-2.0\n' >"$work_directory/expected-installed.txt"
printf 'demo-1.0\n' >"$work_directory/expected-failed.txt"
diff -u "$work_directory/expected-installed.txt" "$FIXTURE_INSTALL_LOG"
diff -u "$work_directory/expected-failed.txt" "$work_directory/reports/failed.txt"
grep -Fq 'Download failure: demo-1.0' "$work_directory/reports/demo-1.0.md"

echo "Hackage install fixtures passed."
