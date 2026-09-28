#!/usr/bin/env bash
# Write the newest blog post and the newest benchmark highlights into README.md.
# The blog is https://blog.aihc.app/ and the benchmarks are https://perf.aihc.app/.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/update-readme-news.sh [README]

Rewrites the latest-news and perf-highlights sections of README (default: README.md).
Set BLOG_URL or PERF_URL to use a different site.
USAGE
}

if [ "$#" -gt 1 ]; then
	usage >&2
	exit 2
fi
case "${1:-}" in
-h | --help)
	usage
	exit 0
	;;
esac

readme="${1:-README.md}"
blog_url="${BLOG_URL:-https://blog.aihc.app}"
perf_url="${PERF_URL:-https://perf.aihc.app}"
github_url="https://github.com/ai-haskell-compiler/aihc"

tmpdir="$(mktemp -d)"
cleanup() {
	rm -rf "$tmpdir"
}
trap cleanup EXIT

curl -fsSL --retry 3 "$blog_url/rss.xml" -o "$tmpdir/rss.xml"
curl -fsSL --retry 3 "$perf_url/api/overview" -o "$tmpdir/overview.json"

# The feed lists the newest post first.
first_item="$(tr '\n' ' ' <"$tmpdir/rss.xml" | grep -o '<item>.*</item>' | sed 's|</item>.*||')"
rss_field() {
	printf '%s' "$first_item" | sed -n "s|.*<$1>\(.*\)</$1>.*|\1|p" |
		sed -e 's/<!\[CDATA\[\(.*\)\]\]>/\1/' -e 's/&lt;/</g' -e 's/&gt;/>/g' \
			-e 's/&quot;/"/g' -e "s/&apos;/'/g" -e "s/&#39;/'/g" -e 's/&amp;/\&/g'
}
post_title="$(rss_field title)"
post_link="$(rss_field link)"
post_description="$(rss_field description)"
post_date="$(rss_field pubDate | awk '{ print $2, $3, $4 }')"

if [ -z "$post_title" ] || [ -z "$post_link" ]; then
	echo "Cannot find a post in $blog_url/rss.xml" >&2
	exit 1
fi

{
	printf '**[%s](%s)** (%s)\n\n' "$post_title" "$post_link" "$post_date"
	if [ -n "$post_description" ]; then
		printf '%s\n\n' "$post_description"
	fi
	printf 'Read all posts at [blog.aihc.app](%s/).\n' "$blog_url"
} >"$tmpdir/latest-news.md"

# Use the machine that has results for the most commits.
jq -r --arg perf "$perf_url" --arg github "$github_url" '
  def geomean: map(select(. > 0) | log) | if length == 0 then null else add / length | exp end;
  # Two decimals below 10 and one decimal from 10, like perf.aihc.app.
  def fixed($digits): (pow(10; $digits)) as $scale | (. * $scale | round) as $n
    | "\($n / $scale | floor).\($n % $scale | tostring | ("0" * ($digits - length)) + .)";
  def fmt: if . == null then "—" elif . >= 10 then fixed(1) + "×" else fixed(2) + "×" end;
  (.machines | map(select(.latest != null)) | max_by(.measured + .inherited)) as $m
  | if $m == null then error("no machine has a measured commit") else . end
  | [.benchmarks | keys[]] as $benchmarks
  | def cell($backend; $profile; $metric):
      [$m.ratios[] | select(.backend == $backend and .optimization == $profile and .metric == $metric and .ratio) | .ratio]
      | geomean | fmt;
    def row($label; $flag; $profile; $metric):
      "| \($label) `\($flag)` | \(cell("native"; $profile; $metric)) | \(cell("llvm"; $profile; $metric)) | \(cell("wasm"; $profile; $metric)) |";
    "Each number is the AIHC value divided by the GHC value, as a geometric mean over \($benchmarks | length) benchmarks. Lower is better. 1.00× is parity.",
    "",
    "| Metric | Native | LLVM | Wasm |",
    "| --- | ---: | ---: | ---: |",
    row("Compile time"; "-O0"; "O0"; "compile_time"),
    row("Artifact size"; "-Os"; "Os"; "artifact_size"),
    row("Runtime"; "-O1"; "O1"; "wall_time"),
    row("Runtime"; "-O2"; "O2"; "wall_time"),
    "",
    "Machine [`\($m.machine_id)`](\($perf)/timeline.html?machine=\($m.machine_id | @uri)), commit [`\($m.latest.sha[0:9])`](\($github)/commit/\($m.latest.sha)) (\($m.latest.committed_at[0:10])). Get all results at [perf.aihc.app](\($perf)/)."
' "$tmpdir/overview.json" >"$tmpdir/perf-highlights.md"

replace_block() {
	local marker="$1"
	local content_file="$2"
	local start="<!-- AUTO-GENERATED: START ${marker} -->"
	local end="<!-- AUTO-GENERATED: END ${marker} -->"
	if [ "$(grep -Fxc "$start" "$readme" || true)" -ne 1 ] || [ "$(grep -Fxc "$end" "$readme" || true)" -ne 1 ]; then
		echo "Expected exactly one block marker pair for '${marker}' in ${readme}" >&2
		exit 1
	fi
	awk -v start="$start" -v end="$end" -v content_file="$content_file" '
    $0 == start {
      print
      while ((getline line < content_file) > 0) print line
      skipping = 1
      next
    }
    $0 == end { skipping = 0 }
    !skipping { print }
  ' "$readme" >"$tmpdir/readme.out"
	cp "$tmpdir/readme.out" "$readme"
}

replace_block latest-news "$tmpdir/latest-news.md"
replace_block perf-highlights "$tmpdir/perf-highlights.md"
