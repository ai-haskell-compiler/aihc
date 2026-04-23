#!/usr/bin/env bash
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/update-generated-content.sh [--update|--check]

  --update  Rewrite generated files/sections in place
  --check   Exit non-zero if generated files/sections are out of date
USAGE
}

if [ "$#" -ne 1 ]; then
	usage >&2
	exit 2
fi

mode="$1"
case "$mode" in
--update | --check) ;;
*)
	usage >&2
	exit 2
	;;
esac

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if [ ! -f flake.nix ]; then
	echo "Run this script from inside the repository." >&2
	exit 1
fi

run_cmd() {
	local cmd="$1"
	bash -c "$cmd"
}

parser_cmd="${PARSER_PROGRESS_CMD:-nix run .#parser-progress}"
lexer_cmd="${LEXER_PROGRESS_CMD:-nix run .#lexer-progress}"
extension_markdown_cmd="${PARSER_EXTENSION_PROGRESS_CMD:-nix run .#parser-extension-progress -- --markdown}"
extension_progress_cmd="${PARSER_EXTENSION_PROGRESS_TEXT_CMD:-nix run .#parser-extension-progress}"
cpp_cmd="${CPP_PROGRESS_CMD:-nix run .#cpp-progress}"
resolve_cmd="${RESOLVE_PROGRESS_CMD:-nix run .#resolve-progress}"
resolve_extension_markdown_cmd="${RESOLVE_EXTENSION_PROGRESS_CMD:-nix run .#resolve-extension-progress -- --markdown}"
tc_cmd="${TC_PROGRESS_CMD:-nix run .#tc-progress}"
tc_extension_markdown_cmd="${TC_EXTENSION_PROGRESS_CMD:-nix run .#tc-extension-progress}"
stackage_cmd="${PARSER_STACKAGE_PROGRESS_CMD:-nix run .#stackage-progress -- --snapshot lts-24.33 --jobs 1}"
line_counts_cmd="${LINE_COUNTS_CMD:-nix run .#line-counts}"

tmpdir="$(mktemp -d)"
cleanup() {
	rm -rf "$tmpdir"
}
trap cleanup EXIT

parser_out="$tmpdir/parser-progress.txt"
lexer_out="$tmpdir/lexer-progress.txt"
extension_out="$tmpdir/extension-progress.md"
extension_progress_out="$tmpdir/extension-progress.txt"
cpp_out="$tmpdir/cpp-progress.txt"
resolve_out="$tmpdir/resolve-progress.txt"
resolve_extension_out="$tmpdir/resolve-extension-progress.md"
tc_out="$tmpdir/tc-progress.txt"
tc_extension_out="$tmpdir/tc-extension-progress.md"
stackage_out="$tmpdir/stackage-progress.txt"
line_counts_out="$tmpdir/line-counts.txt"

run_cmd "$parser_cmd" >"$parser_out"
run_cmd "$lexer_cmd" >"$lexer_out"
run_cmd "$extension_markdown_cmd" | sed -n '/^# Haskell Parser Extension Support Status/,$p' >"$extension_out"
run_cmd "$extension_progress_cmd" >"$extension_progress_out"
run_cmd "$cpp_cmd" >"$cpp_out"
run_cmd "$resolve_cmd" >"$resolve_out"
run_cmd "$resolve_extension_markdown_cmd" | sed -n '/^# Name Resolver Extension Support Status/,$p' >"$resolve_extension_out"
run_cmd "$tc_cmd" >"$tc_out"
run_cmd "$tc_extension_markdown_cmd" | sed -n '/^# Type Checker Extension Support Status/,$p' >"$tc_extension_out"
run_cmd "$stackage_cmd" >"$stackage_out" || true
run_cmd "$line_counts_cmd" >"$line_counts_out"

parse_progress() {
	local infile="$1"
	awk '
    /^PASS[[:space:]]+/ { pass=$2 }
    /^XFAIL[[:space:]]+/ { xfail=$2 }
    /^XPASS[[:space:]]+/ { xpass=$2 }
    /^FAIL[[:space:]]+/ { fail=$2 }
    /^TOTAL[[:space:]]+/ { total=$2 }
    /^COMPLETE[[:space:]]+/ {
      gsub(/%/, "", $2)
      complete=$2
    }
    END {
      if (total == "" || pass == "" || xfail == "" || xpass == "" || fail == "" || complete == "") {
        exit 2
      }
      implemented = pass + xpass
      printf "%d\n%d\n%d\n%d\n%d\n%d\n%.2f\n", pass, xfail, xpass, fail, total, implemented, complete
    }
  ' "$infile"
}

parse_extension_summary() {
	local infile="$1"
	awk '
    /^- Total Extensions:/ { total=$4 }
    /^- Supported:/ { supported=$3 }
    /^- In Progress:/ { in_progress=$4 }
    END {
      if (total == "" || supported == "" || in_progress == "") {
        exit 2
      }
      printf "%d\n%d\n%d\n", total, supported, in_progress
    }
  ' "$infile"
}

parse_extension_progress() {
	local infile="$1"
	awk '
    /^SUPPORTED[[:space:]]+[0-9]+$/ { supported = $2 + 0 }
    /^IN_PROGRESS[[:space:]]+[0-9]+$/ { in_progress = $2 + 0 }
    /^TOTAL[[:space:]]+[0-9]+$/ { declared_total = $2 + 0 }
    {
      line_has_counts = 0
      line_pass = 0
      line_xfail = 0
      line_xpass = 0
      line_fail = 0

      for (i = 1; i <= NF; i++) {
        if ($i ~ /^PASS=[0-9]+$/) {
          value = $i
          sub(/^PASS=/, "", value)
          line_pass = value + 0
          line_has_counts = 1
        } else if ($i ~ /^XFAIL=[0-9]+$/) {
          value = $i
          sub(/^XFAIL=/, "", value)
          line_xfail = value + 0
          line_has_counts = 1
        } else if ($i ~ /^XPASS=[0-9]+$/) {
          value = $i
          sub(/^XPASS=/, "", value)
          line_xpass = value + 0
          line_has_counts = 1
        } else if ($i ~ /^FAIL=[0-9]+$/) {
          value = $i
          sub(/^FAIL=/, "", value)
          line_fail = value + 0
          line_has_counts = 1
        }
      }

      if (line_has_counts) {
        pass += line_pass
        xfail += line_xfail
        xpass += line_xpass
        fail += line_fail
      }
    }
    END {
      total = pass + xfail + xpass + fail
      if (total <= 0 || declared_total == "") {
        exit 2
      }
      complete = ((pass + xpass) * 100.0) / total
      printf "%d\n%d\n%d\n%d\n%d\n%d\n%.2f\n", pass, xfail, xpass, fail, total, pass + xpass, complete
    }
  ' "$infile"
}

parse_stackage_progress() {
	local infile="$1"
	tr '\r' '\n' <"$infile" | awk '
    {
      for (i=1; i<=NF; i++) {
        if ($i == "/" && $(i-2) == "AIHC:") {
          implemented = $(i-1) + 0
          total = $(i+1) + 0
        }
      }
    }
    END {
      if (total == "" || total <= 0) {
        exit 2
      }
      complete = (implemented * 100.0) / total
      printf "%d\n%d\n%.2f\n", implemented, total, complete
    }
  '
}

parser_vals=($(parse_progress "$parser_out")) || {
	echo "update-generated-content.sh: could not parse parser-progress summary (expected PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE on stdout)." >&2
	exit 2
}
parser_pass="${parser_vals[0]}"
parser_xfail="${parser_vals[1]}"
parser_xpass="${parser_vals[2]}"
parser_fail="${parser_vals[3]}"
parser_total="${parser_vals[4]}"
parser_implemented="${parser_vals[5]}"
parser_complete="${parser_vals[6]}"

lexer_vals=($(parse_progress "$lexer_out")) || {
	echo "update-generated-content.sh: could not parse lexer-progress summary (expected PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE on stdout)." >&2
	exit 2
}
lexer_pass="${lexer_vals[0]}"
lexer_xfail="${lexer_vals[1]}"
lexer_xpass="${lexer_vals[2]}"
lexer_fail="${lexer_vals[3]}"
lexer_total="${lexer_vals[4]}"
lexer_implemented="${lexer_vals[5]}"
lexer_complete="${lexer_vals[6]}"

cpp_vals=($(parse_progress "$cpp_out")) || {
	echo "update-generated-content.sh: could not parse cpp-progress summary (expected PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE on stdout)." >&2
	exit 2
}
cpp_pass="${cpp_vals[0]}"
cpp_xfail="${cpp_vals[1]}"
cpp_xpass="${cpp_vals[2]}"
cpp_fail="${cpp_vals[3]}"
cpp_total="${cpp_vals[4]}"
cpp_implemented="${cpp_vals[5]}"
cpp_complete="${cpp_vals[6]}"

resolve_vals=($(parse_progress "$resolve_out")) || {
	echo "update-generated-content.sh: could not parse resolve-progress summary (expected PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE on stdout)." >&2
	exit 2
}
resolve_pass="${resolve_vals[0]}"
resolve_xfail="${resolve_vals[1]}"
resolve_xpass="${resolve_vals[2]}"
resolve_fail="${resolve_vals[3]}"
resolve_total="${resolve_vals[4]}"
resolve_implemented="${resolve_vals[5]}"
resolve_complete="${resolve_vals[6]}"

tc_vals=($(parse_progress "$tc_out")) || {
	echo "update-generated-content.sh: could not parse tc-progress summary (expected PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE on stdout)." >&2
	exit 2
}
tc_pass="${tc_vals[0]}"
tc_xfail="${tc_vals[1]}"
tc_xpass="${tc_vals[2]}"
tc_fail="${tc_vals[3]}"
tc_total="${tc_vals[4]}"
tc_implemented="${tc_vals[5]}"
tc_complete="${tc_vals[6]}"

ext_vals=($(parse_extension_summary "$extension_out")) || {
	echo "update-generated-content.sh: could not parse extension markdown summary (expected Total Extensions, Supported, In Progress lines after --markdown)." >&2
	exit 2
}
ext_total="${ext_vals[0]}"
ext_supported="${ext_vals[1]}"
ext_in_progress="${ext_vals[2]}"

ext_progress_vals=($(parse_extension_progress "$extension_progress_out")) || {
	echo "update-generated-content.sh: could not parse parser-extension-progress text (expected PASS=/XFAIL=/ lines)." >&2
	exit 2
}
ext_test_total="${ext_progress_vals[4]}"
ext_implemented="${ext_progress_vals[5]}"

stackage_vals=($(parse_stackage_progress "$stackage_out")) || {
	echo "update-generated-content.sh: could not parse stackage-progress output (expected 'AIHC: N / M' line on stdout)." >&2
	exit 2
}
stackage_implemented="${stackage_vals[0]}"
stackage_total="${stackage_vals[1]}"
stackage_complete="${stackage_vals[2]}"

parser_total_tests=$((parser_total + ext_test_total))
parser_passing_tests=$((parser_implemented + ext_implemented))
parser_total_complete="$(awk -v passing="$parser_passing_tests" -v total="$parser_total_tests" 'BEGIN { if (total <= 0) { printf "0.00" } else { printf "%.2f", (passing * 100.0) / total } }')"

# extract extension name lists (alphabetically sorted, comma-separated) from the markdown table if present
ext_supported_names="$(awk -F'|' 'BEGIN{names=""} /^\|/ { status=$3; name=$2; gsub(/^[ \t]+|[ \t]+$/, "", name); gsub(/^[ \t]+|[ \t]+$/, "", status); if (status == "Supported") { if (names=="") names=name; else names=names ", " name } } END{ print names }' "$extension_out")"
ext_in_progress_names="$(awk -F'|' 'BEGIN{names=""} /^\|/ { status=$3; name=$2; gsub(/^[ \t]+|[ \t]+$/, "", name); gsub(/^[ \t]+|[ \t]+$/, "", status); if (status == "In Progress") { if (names=="") names=name; else names=names ", " name } } END{ print names }' "$extension_out")"

cat >"$tmpdir/readme-root-parser.txt" <<EOF2
\`${parser_passing_tests}/${parser_total_tests}\` (\`${parser_total_complete}%\`)
EOF2

cat >"$tmpdir/readme-root-cpp.txt" <<EOF2
\`${cpp_implemented}/${cpp_total}\` (\`${cpp_complete}%\`)
EOF2

cat >"$tmpdir/readme-root-stackage.txt" <<EOF2
\`${stackage_implemented}/${stackage_total}\` (\`${stackage_complete}%\`)
EOF2

cat >"$tmpdir/readme-root-lexer.txt" <<EOF2
\`${lexer_implemented}/${lexer_total}\` (\`${lexer_complete}%\`)
EOF2

cat >"$tmpdir/readme-root-resolve.txt" <<EOF2
\`${resolve_implemented}/${resolve_total}\` (\`${resolve_complete}%\`)
EOF2

cat >"$tmpdir/readme-root-tc.txt" <<EOF2
\`${tc_implemented}/${tc_total}\` (\`${tc_complete}%\`)
EOF2

cat >"$tmpdir/readme-parser-h2010.txt" <<EOF2
- \`${parser_implemented}/${parser_total}\` implemented (\`${parser_complete}%\` complete)
EOF2

cat >"$tmpdir/readme-parser-extension.txt" <<EOF2
- Total tracked extensions: \`${ext_total}\`
- Supported: \`${ext_supported}\`
- In Progress: \`${ext_in_progress}\`
EOF2

cat >"$tmpdir/readme-cpp.txt" <<EOF2
- \`${cpp_implemented}/${cpp_total}\` implemented (\`${cpp_complete}%\` complete)
EOF2

replace_marker_block() {
	local file="$1"
	local marker="$2"
	local content_file="$3"
	local start="<!-- AUTO-GENERATED: START ${marker} -->"
	local end="<!-- AUTO-GENERATED: END ${marker} -->"
	local tmp_out="$tmpdir/$(basename "$file").${marker}.out"

	local start_count
	local end_count
	start_count="$(grep -Fxc "$start" "$file" || true)"
	end_count="$(grep -Fxc "$end" "$file" || true)"
	if [ "$start_count" -ne 1 ] || [ "$end_count" -ne 1 ]; then
		echo "Expected exactly one marker pair for '${marker}' in ${file}" >&2
		exit 1
	fi

	awk -v start="$start" -v end="$end" -v content_file="$content_file" '
    $0 == start {
      print
      while ((getline line < content_file) > 0) {
        print line
      }
      close(content_file)
      in_block = 1
      next
    }
    $0 == end {
      in_block = 0
      print
      next
    }
    !in_block { print }
  ' "$file" >"$tmp_out"

	if [ "$mode" = "--update" ]; then
		if ! cmp -s "$file" "$tmp_out"; then
			cat "$tmp_out" >"$file"
		fi
	else
		if ! cmp -s "$file" "$tmp_out"; then
			echo "Generated block out of date: ${file} (${marker})" >&2
			stale=1
		fi
	fi
}

replace_marker_inline() {
	local file="$1"
	local marker="$2"
	local content_file="$3"
	local start="<!-- AUTO-GENERATED: START ${marker} -->"
	local end="<!-- AUTO-GENERATED: END ${marker} -->"
	local tmp_out="$tmpdir/$(basename "$file").${marker}.inline.out"

	local start_count
	local end_count
	start_count="$(grep -Foc "$start" "$file" || true)"
	end_count="$(grep -Foc "$end" "$file" || true)"
	if [ "$start_count" -ne 1 ] || [ "$end_count" -ne 1 ]; then
		echo "Expected exactly one inline marker pair for '${marker}' in ${file}" >&2
		exit 1
	fi

	local content
	content="$(tr -d '\n' <"$content_file")"

	awk -v start="$start" -v end="$end" -v content="$content" '
    {
      s = index($0, start)
      e = index($0, end)
      if (s > 0 && e > s) {
        prefix = substr($0, 1, s + length(start) - 1)
        suffix = substr($0, e)
        print prefix " " content " " suffix
      } else {
        print
      }
    }
  ' "$file" >"$tmp_out"

	if [ "$mode" = "--update" ]; then
		if ! cmp -s "$file" "$tmp_out"; then
			cat "$tmp_out" >"$file"
		fi
	else
		if ! cmp -s "$file" "$tmp_out"; then
			echo "Generated inline block out of date: ${file} (${marker})" >&2
			stale=1
		fi
	fi
}

stale=0

if [ "$mode" = "--update" ]; then
	cp "$extension_out" docs/aihc-parser-supported-extensions.md
else
	if ! cmp -s docs/aihc-parser-supported-extensions.md "$extension_out"; then
		echo "Generated file out of date: docs/aihc-parser-supported-extensions.md" >&2
		stale=1
	fi
fi

if [ "$mode" = "--update" ]; then
	cp "$tc_extension_out" docs/aihc-tc-supported-extensions.md
else
	if ! cmp -s docs/aihc-tc-supported-extensions.md "$tc_extension_out"; then
		echo "Generated file out of date: docs/aihc-tc-supported-extensions.md" >&2
		stale=1
	fi
fi

if [ "$mode" = "--update" ]; then
	cp "$resolve_extension_out" docs/aihc-resolve-supported-extensions.md
else
	if ! cmp -s docs/aihc-resolve-supported-extensions.md "$resolve_extension_out"; then
		echo "Generated file out of date: docs/aihc-resolve-supported-extensions.md" >&2
		stale=1
	fi
fi

replace_marker_inline README.md "parser-progress" "$tmpdir/readme-root-parser.txt"
replace_marker_inline README.md "lexer-progress" "$tmpdir/readme-root-lexer.txt"
replace_marker_inline README.md "parser-stackage-progress" "$tmpdir/readme-root-stackage.txt"
replace_marker_inline README.md "cpp-progress" "$tmpdir/readme-root-cpp.txt"
replace_marker_inline README.md "resolve-progress" "$tmpdir/readme-root-resolve.txt"
replace_marker_inline README.md "tc-progress" "$tmpdir/readme-root-tc.txt"
replace_marker_block README.md "line-counts" "$line_counts_out"
replace_marker_block components/aihc-parser/README.md "haskell2010-progress" "$tmpdir/readme-parser-h2010.txt"
replace_marker_block components/aihc-parser/README.md "extension-progress" "$tmpdir/readme-parser-extension.txt"
replace_marker_block components/aihc-cpp/README.md "cpp-progress" "$tmpdir/readme-cpp.txt"

if [ "$mode" = "--check" ] && [ "$stale" -ne 0 ]; then
	exit 1
fi
