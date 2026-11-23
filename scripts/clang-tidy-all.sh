#!/usr/bin/env bash
# Run clang-tidy on each C source file of the project.
# The files are independent, so one clang-tidy process runs for each processor.
set -euo pipefail

if [[ "${1:-}" == "--file" ]]; then
  file=$2
  if [[ "$file" == *core-libs/aihc-rts/wasm/*.c || "$file" == *aihc_host_wasip3.c ]]; then
    exec clang-tidy-unwrapped --quiet "$file" -- --target=wasm32-wasip3 --sysroot="$AIHC_WASM_SYSROOT" -std=c11 -Wall -Wextra -Wpedantic -Icore-libs/aihc-rts/wasm -Icore-libs/aihc-rts/native -isystem core-libs/aihc-rts/wasm/generated
  fi
  # tooling/aihc-testing/cbits/foreign_call.c includes ffi.h. A system
  # include directory keeps the checks off the libffi header.
  read -r -a ffi_flags < <(pkg-config --cflags-only-I libffi)
  ffi_flags=("${ffi_flags[@]/#-I/-isystem}")
  exec clang-tidy --quiet "$file" -- -std=c11 -Wall -Wextra -Wpedantic -Icore-libs/aihc-rts/native "${ffi_flags[@]}"
fi

jobs=$(getconf _NPROCESSORS_ONLN)
find components tooling bin core-libs scripts test -type f -name "*.c" \
  -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*" -not -path "*/generated/*" -print0 |
  xargs -0 -r -n 1 -P "$jobs" "$0" --file
