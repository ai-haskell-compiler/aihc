#!/usr/bin/env bash
# Compile each pipeline example of the AIHC Manual and keep its intermediate
# programs. The manual includes the output with pymdownx.snippets.
#
# Usage:
#   generate-pipeline-examples.sh --aihc CMD --target TARGET --output DIR
#     [--store DIR] [--optimization LEVEL] EXAMPLES_DIR
#
# Without --optimization, the compiler uses its default level. The level is
# part of the store key, so the store must have the core libraries at the
# same level.
#
# EXAMPLES_DIR has one directory for each example. Each example directory
# has Main.hs and description.md. The directory names give the order.
#
# DIR gets this layout:
#   DIR/<example>/Main.hs    the Haskell source
#   DIR/<example>/core       the System FC program of Main
#   DIR/<example>/grin       the GRIN program of Main
#   DIR/<example>/lir        the Lir program of Main
#   DIR/examples.md          a section with tabs for each example
#
# The script stops with an error when a build fails or a dump is missing.
# Thus, a compiler change that breaks an example also breaks the manual build.
set -euo pipefail

aihc=""
target=""
store=""
output=""
optimization=""
examples=""

usage() {
  sed -n '5,11p' "$0" | sed 's/^# \{0,1\}//' >&2
  exit 2
}

while [[ $# -gt 0 ]]; do
  case "$1" in
  --aihc)
    aihc=$2
    shift 2
    ;;
  --target)
    target=$2
    shift 2
    ;;
  --store)
    store=$2
    shift 2
    ;;
  --output)
    output=$2
    shift 2
    ;;
  --optimization)
    optimization=$2
    shift 2
    ;;
  -*)
    usage
    ;;
  *)
    examples=$1
    shift
    ;;
  esac
done

if [[ -z "$aihc" || -z "$target" || -z "$output" || -z "$examples" ]]; then
  usage
fi

examples=$(cd "$examples" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

rm -rf "$output"
mkdir -p "$output"
output=$(cd "$output" && pwd)
page="$output/examples.md"

# A fence line in a dump would end the code block too early.
check_fences() {
  local file=$1
  if grep -qE '^[[:space:]]*(```|~~~)' "$file"; then
    echo "error: $file has a Markdown fence line" >&2
    exit 1
  fi
}

# Write one tab of a tabbed block. The tab content has an indent of 4 spaces.
write_tab() {
  local title=$1 language=$2 file=$3
  check_fences "$file"
  {
    printf '=== "%s"\n\n' "$title"
    printf '    ```%s\n' "$language"
    sed 's/^/    /' "$file"
    printf '    ```\n\n'
  } >>"$page"
}

# Give the one file that matches a pattern, or stop with an error.
single_file() {
  local description=$1
  shift
  if [[ $# -ne 1 || ! -f "$1" ]]; then
    echo "error: expected one $description, found: $*" >&2
    exit 1
  fi
  printf '%s\n' "$1"
}

store_flags=()
if [[ -n "$store" ]]; then
  store_flags=(--store "$store")
fi
optimization_flags=()
level_text="the default optimization level"
if [[ -n "$optimization" ]]; then
  optimization_flags=(-O "$optimization")
  level_text="\`-O$optimization\`"
fi

found=0
for example_dir in "$examples"/*/; do
  example_dir=${example_dir%/}
  name=$(basename "$example_dir")
  for required in Main.hs description.md; do
    if [[ ! -f "$example_dir/$required" ]]; then
      echo "error: example $name has no $required" >&2
      exit 1
    fi
  done
  found=$((found + 1))

  echo "Compiling pipeline example $name" >&2
  source_dir="$work/$name/source"
  build_root="$work/$name/build"
  mkdir -p "$source_dir"
  cp "$example_dir/Main.hs" "$source_dir/Main.hs"
  (
    cd "$source_dir"
    # shellcheck disable=SC2086 # $aihc can be a command with arguments.
    $aihc build Main.hs \
      --target "$target" \
      ${store_flags[@]+"${store_flags[@]}"} \
      --build-root "$build_root" \
      --keep-core --keep-grin --keep-lir \
      ${optimization_flags[@]+"${optimization_flags[@]}"} \
      --no-link \
      --output "$work/$name/bundle" >&2
  )

  shopt -s nullglob
  core=$(single_file "System FC dump for $name" "$build_root"/*/Main/core)
  grin=$(single_file "GRIN dump for $name" "$build_root"/*/Main/grin)
  lir=$(single_file "Lir dump for $name" "$build_root"/*/Main/*.lir)
  shopt -u nullglob

  mkdir -p "$output/$name"
  cp "$example_dir/Main.hs" "$output/$name/Main.hs"
  cp "$core" "$output/$name/core"
  cp "$grin" "$output/$name/grin"
  cp "$lir" "$output/$name/lir"

  {
    cat "$example_dir/description.md"
    printf '\n'
  } >>"$page"
  write_tab Haskell haskell "$output/$name/Main.hs"
  write_tab "System FC" text "$output/$name/core"
  write_tab GRIN text "$output/$name/grin"
  write_tab Lir text "$output/$name/lir"
done

if [[ $found -eq 0 ]]; then
  echo "error: no examples in $examples" >&2
  exit 1
fi

cat >"$output/settings.md" <<EOF
The compiler made these programs for the target \`$target\` at $level_text.
EOF

echo "Wrote $found pipeline examples to $output" >&2
