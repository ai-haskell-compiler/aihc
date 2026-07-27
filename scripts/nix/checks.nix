{
  sources,
  mkHsPkgsForChecks,
}: pkgs: let
  hsPkgs = mkHsPkgsForChecks pkgs;
  wasmLd = pkgs.writeShellScriptBin "wasm-ld" ''
    exec ${pkgs.lld}/bin/wasm-ld "$@"
  '';
  wasmOpt = pkgs.writeShellScriptBin "wasm-opt" ''
    printf 'invoked\n' >> "''${AIHC_WASM_OPT_MARKER:?}"
    exec ${pkgs.binaryen}/bin/wasm-opt "$@"
  '';
  examplesSource = sources.examplesSrc pkgs;
  exampleEntries = builtins.readDir "${examplesSource}/examples";
  exampleNames = builtins.filter (
    name:
      exampleEntries.${name}
      == "directory"
      && builtins.pathExists "${examplesSource}/examples/${name}/Main.hs"
  ) (builtins.attrNames exampleEntries);
  cTidyCompilerFlags =
    ["-std=c11" "-Wall" "-Wextra" "-Wpedantic"]
    ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
      "-isysroot"
      "${pkgs.apple-sdk}/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"
    ];

  mkSourceCheck = name: src: nativeBuildInputs: text:
    pkgs.runCommand name {
      inherit src nativeBuildInputs;
    } ''
      cd "$src"
      ${text}
      touch "$out"
    '';

  testExecutable = drv: "${drv.intermediates}/share/haskell/${hsPkgs.ghc.version}/${drv.pname}-${drv.version}/dist/build/spec/spec";

  mkPackageTest = {
    drv,
    src,
    nativeBuildInputs ? [],
    environment ? "",
  }:
    mkSourceCheck "${drv.pname}-tests" src nativeBuildInputs ''
      ${environment}
      ${testExecutable drv} --hide-successes
    '';

  coreLibraryEnvironment = ''
    export AIHC_BASE_SRC=${sources.baseSrc pkgs}
    export AIHC_PRIM_SRC=${sources.primSrc pkgs}
  '';

  evalFixtureEnvironment =
    coreLibraryEnvironment
    + ''
      export AIHC_EVAL_FIXTURES=${sources.evalFixturesSrc pkgs}
    '';

  renderExampleTest = ''
    executable="$batch_output/$example_name"
    actual_stdout="$executable.stdout"
    actual_stderr="$executable.stderr"
    timeout_stderr="$executable.timeout-stderr"
    run_directory="$executable.run"
    stdin_file=/dev/null
    if [[ -f "$example_directory/stdin" ]]; then
      stdin_file="$example_directory/stdin"
    fi
    expected_stderr="$empty_stderr"
    if [[ -f "$example_directory/stderr" ]]; then
      expected_stderr="$example_directory/stderr"
    fi
    expected_exit=0
    if [[ -f "$example_directory/exit" ]]; then
      expected_exit=$(<"$example_directory/exit")
    fi
    mkdir -p "$run_directory"
    if timeout --foreground --kill-after=5s 10s \
      bash -c 'cd "$1"; exec "$2" 2> "$3"' \
      bash "$run_directory" "$executable" "$actual_stderr" \
      < "$stdin_file" > "$actual_stdout" 2> "$timeout_stderr"; then
      actual_exit=0
    else
      actual_exit=$?
    fi
    if [[ "$actual_exit" -eq 124 || "$actual_exit" -eq 137 ]]; then
      echo "Timed out running $example_name/portable-c-incremental-calloc" >&2
      cat "$timeout_stderr" >&2
      exit 1
    fi
    if [[ "$expected_exit" == nonzero ]]; then
      if [[ "$actual_exit" -eq 0 ]]; then
        echo "Expected $example_name/portable-c-incremental-calloc to fail" >&2
        exit 1
      fi
    elif [[ "$expected_exit" =~ ^[0-9]+$ ]]; then
      if [[ "$actual_exit" -ne "$expected_exit" ]]; then
        echo "Expected $example_name/portable-c-incremental-calloc to exit with $expected_exit, got $actual_exit" >&2
        exit 1
      fi
    else
      echo "Invalid expected exit status for $example_name: $expected_exit" >&2
      exit 1
    fi
    diff --unified \
      --label "$example_name/stdout-expected" \
      --label "$example_name/stdout-portable-c-incremental-calloc" \
      "$expected_stdout" "$actual_stdout"
    diff --unified \
      --label "$example_name/stderr-expected" \
      --label "$example_name/stderr-portable-c-incremental-calloc" \
      "$expected_stderr" "$actual_stderr"
  '';

  renderWasip3ExampleTest = ''
    executable="$batch_output/$example_name"
    actual_stdout="$executable.stdout"
    actual_stderr="$executable.stderr"
    run_directory="$executable.run"
    stdin_file=/dev/null
    if [[ -f "$example_directory/stdin" ]]; then
      stdin_file="$example_directory/stdin"
    fi
    expected_stderr="$empty_stderr"
    if [[ -f "$example_directory/stderr" ]]; then
      expected_stderr="$example_directory/stderr"
    fi
    expected_exit=0
    if [[ -f "$example_directory/exit" ]]; then
      expected_exit=$(<"$example_directory/exit")
    fi
    mkdir -p "$run_directory"
    if timeout --foreground --kill-after=5s 10s wasmtime run -C cache=n -S cli \
      --dir "$run_directory::." \
      "$executable" \
      < "$stdin_file" > "$actual_stdout" 2> "$actual_stderr"; then
      actual_exit=0
    else
      actual_exit=$?
    fi
    if [[ "$actual_exit" -eq 124 || "$actual_exit" -eq 137 ]]; then
      echo "Timed out running $example_name/wasm32-wasip3-incremental" >&2
      exit 1
    fi
    if [[ "$expected_exit" == nonzero ]]; then
      if [[ "$actual_exit" -eq 0 ]]; then
        echo "Expected $example_name/wasm32-wasip3-incremental to fail" >&2
        exit 1
      fi
    elif [[ "$expected_exit" =~ ^[0-9]+$ ]]; then
      if [[ "$actual_exit" -ne "$expected_exit" ]]; then
        echo "Expected $example_name/wasm32-wasip3-incremental to exit with $expected_exit, got $actual_exit" >&2
        exit 1
      fi
    else
      echo "Invalid expected exit status for $example_name: $expected_exit" >&2
      exit 1
    fi
    diff --unified \
      --label "$example_name/stdout-expected" \
      --label "$example_name/stdout-wasm32-wasip3-incremental" \
      "$expected_stdout" "$actual_stdout"
    if [[ "$expected_exit" != nonzero ]]; then
      diff --unified \
        --label "$example_name/stderr-expected" \
        --label "$example_name/stderr-wasm32-wasip3-incremental" \
        "$expected_stderr" "$actual_stderr"
    fi
  '';

  aihcExe = pkgs.lib.getExe' hsPkgs.aihc "aihc";

  amd64Tests = mkPackageTest {
    drv = hsPkgs.aihc-amd64;
    src = sources.amd64Src pkgs;
    nativeBuildInputs = [pkgs.llvmPackages.clang];
    environment = evalFixtureEnvironment;
  };
  arm64Tests = mkPackageTest {
    drv = hsPkgs.aihc-arm64;
    src = sources.arm64Src pkgs;
    nativeBuildInputs = [pkgs.llvmPackages.clang];
    environment = evalFixtureEnvironment;
  };
  cBackendTests = mkPackageTest {
    drv = hsPkgs.aihc-c;
    src = sources.cBackendSrc pkgs;
    nativeBuildInputs = [pkgs.llvmPackages.clang];
    environment = evalFixtureEnvironment;
  };
  nativeTests = mkPackageTest {
    drv = hsPkgs.aihc-native;
    src = sources.nativeSrc pkgs;
  };
  wasmTests = mkPackageTest {
    drv = hsPkgs.aihc-wasm;
    src = sources.wasmSrc pkgs;
  };
  fcTests = mkPackageTest {
    drv = hsPkgs.aihc-fc;
    src = sources.fcSrc pkgs;
    environment = evalFixtureEnvironment;
  };
  grinTests = mkPackageTest {
    drv = hsPkgs.aihc-grin;
    src = sources.grinSrc pkgs;
    environment = evalFixtureEnvironment;
  };
  resolveTests = mkPackageTest {
    drv = hsPkgs.aihc-resolve;
    src = sources.resolveSrc pkgs;
  };
  tcTests = mkPackageTest {
    drv = hsPkgs.aihc-tc;
    src = sources.tcSrc pkgs;
  };
  testingTests = mkPackageTest {
    drv = hsPkgs.aihc-testing;
    src = sources.testingSrc pkgs;
  };
  devTests = mkPackageTest {
    drv = hsPkgs.aihc-dev;
    src = sources.devSrc pkgs;
    nativeBuildInputs = [
      (hsPkgs.ghcWithPackages (p: [p.aihc-internal]))
    ];
  };
  aihcTests = mkPackageTest {
    drv = hsPkgs.aihc;
    src = sources.aihcSrc pkgs;
    nativeBuildInputs = [pkgs.llvmPackages.clang];
    environment = coreLibraryEnvironment;
  };
  fmtTests = mkPackageTest {
    drv = hsPkgs.aihc-fmt;
    src = sources.fmtSrc pkgs;
  };
  unicode = import ./unicode.nix {inherit pkgs;};
  unicodeGenerated =
    pkgs.runCommand "aihc-unicode-generated" {
      nativeBuildInputs = [pkgs.diffutils pkgs.ormolu];
    } ''
      generated="$TMPDIR/generated/GHC/Prim/Unicode.hs"
      UNICODE_VERSION=${unicode.version} ${unicode.generator} \
        --input=${unicode.ucd}/ \
        --output="$TMPDIR/generated" \
        --core-prop=Uppercase \
        --core-prop=Lowercase
      ormolu --mode inplace "$generated"
      diff --unified ${sources.primSrc pkgs}/src/GHC/Prim/Unicode.hs "$generated"
      touch "$out"
    '';

  nixLint = mkSourceCheck "aihc-nix-lint" (sources.nixSrc pkgs) [pkgs.statix] ''
    statix check flake.nix
  '';

  nixFormat = mkSourceCheck "aihc-nix-format" (sources.nixSrc pkgs) [pkgs.alejandra] ''
    alejandra --check .
  '';

  haskellLint = mkSourceCheck "aihc-haskell-lint" (sources.haskellSrc pkgs) [pkgs.hlint pkgs.findutils] ''
    find . -type f -name '*.hs' -print0 \
      | xargs -0 -r hlint
  '';

  haskellFormat = mkSourceCheck "aihc-haskell-format" (sources.haskellSrc pkgs) [pkgs.ormolu pkgs.findutils] ''
    find . -type f -name '*.hs' -print0 \
      | xargs -0 -r ormolu --mode check
  '';

  cLint = mkSourceCheck "aihc-c-lint" (sources.cSrc pkgs) [pkgs.clang-tools pkgs.findutils pkgs.wit-bindgen] ''
    bindings_directory="$TMPDIR/aihc-wasip3-bindings"
    mkdir -p "$bindings_directory"
    wit-bindgen c --world command --out-dir "$bindings_directory" components/aihc-wasm/runtime/wit
    while IFS= read -r -d "" file; do
      if [[ "$file" == *components/aihc-wasm/runtime/*.c || "$file" == *aihc_host_wasip3.c ]]; then
        clang-tidy-unwrapped --quiet "$file" -- \
          --target=wasm32-unknown-unknown \
          -std=c11 -ffreestanding -Wall -Wextra -Wpedantic \
          -Icomponents/aihc-wasm/runtime/include \
          -Icomponents/aihc-wasm/runtime \
          -Icomponents/aihc-native/runtime \
          -isystem "$bindings_directory"
      else
        clang-tidy --quiet "$file" -- ${pkgs.lib.escapeShellArgs cTidyCompilerFlags}
      fi
    done < <(find . -type f -name '*.c' -print0)
  '';

  cFormat = mkSourceCheck "aihc-c-format" (sources.cSrc pkgs) [pkgs.clang-tools pkgs.findutils] ''
    find . -type f \( -name '*.c' -o -name '*.h' \) -print0 \
      | xargs -0 -r clang-format --dry-run --Werror
  '';

  cabalFormat = mkSourceCheck "aihc-cabal-format" (sources.cabalSrc pkgs) [pkgs.haskellPackages.cabal-gild pkgs.findutils] ''
    failed=0
    while IFS= read -r -d "" file; do
      cabal-gild --mode check --input "$file" || failed=1
    done < <(find . -type f -name '*.cabal' -print0)
    test "$failed" -eq 0
  '';

  exampleTestInputs = [
    pkgs.coreutils
    pkgs.diffutils
    pkgs.findutils
    pkgs.llvmPackages.clang
  ];

  renderExampleCase = exampleName: ''
    (
      source="examples/${exampleName}/Main.hs"
      example_directory=$(dirname "$source")
      example_name=${pkgs.lib.escapeShellArg exampleName}
      expected_stdout="$example_directory/stdout"
      if [[ ! -f "$expected_stdout" ]]; then
        echo "Missing expected stdout for $source: $expected_stdout" >&2
        exit 1
      fi

      ${renderExampleTest}
    ) &
    pids+=("$!")
  '';

  # Compile every example in one portable-C smoke batch. Backend, compilation
  # mode, and collector matrices belong to their focused component tests; the
  # example check proves the end-to-end programs without repeating that matrix.
  examplesTests = assert exampleNames != [];
    mkSourceCheck "aihc-examples-tests" examplesSource exampleTestInputs ''
            set -euo pipefail
            export GHCRTS=-N1
            empty_stderr="$TMPDIR/empty-stderr"
            touch "$empty_stderr"

      export XDG_CACHE_HOME="$TMPDIR/cache"
      batch_output="$TMPDIR/batch-portable-c"
      batch_sources=()
      for example_name in ${pkgs.lib.escapeShellArgs exampleNames}; do
        batch_sources+=("examples/$example_name/Main.hs")
      done
      timeout --foreground --kill-after=5s 120s ${aihcExe} compile-batch \
        --output-directory "$batch_output" \
        --target portable-c \
        --gc calloc \
        "''${batch_sources[@]}"

      pids=()
            ${pkgs.lib.concatMapStringsSep "\n" renderExampleCase exampleNames}
            failed=0
            for pid in "''${pids[@]}"; do
              wait "$pid" || failed=1
            done
            test "$failed" -eq 0
            touch "$out"
    '';

  wasip3ExampleInputs = [
    pkgs.coreutils
    pkgs.diffutils
    pkgs.findutils
    pkgs.llvmPackages.bintools
    pkgs.llvmPackages.clang-unwrapped
    pkgs.wasm-tools
    pkgs.wasmtime
    pkgs.wit-bindgen
    wasmLd
    wasmOpt
  ];

  renderWasip3ExampleCase = exampleName: ''
    (
      source="examples/${exampleName}/Main.hs"
      example_directory=$(dirname "$source")
      example_name=${pkgs.lib.escapeShellArg exampleName}
      expected_stdout="$example_directory/stdout"
      if [[ ! -f "$expected_stdout" ]]; then
        echo "Missing expected stdout for $source: $expected_stdout" >&2
        exit 1
      fi

      ${renderWasip3ExampleTest}
    ) &
    pids+=("$!")
  '';

  # Compile every example in one incremental WASI smoke batch. The shared
  # dependency closure is compiled once and reused in memory by the whole batch.
  wasip3ExampleTest = assert exampleNames != [];
    mkSourceCheck "aihc-wasip3-example-test" examplesSource wasip3ExampleInputs ''
            set -euo pipefail
            export GHCRTS=-N1
            export AIHC_WASM_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
            export AIHC_WASM_OPT_MARKER="$TMPDIR/wasm-opt-invocations"
            empty_stderr="$TMPDIR/empty-stderr"
            touch "$empty_stderr"

      export XDG_CACHE_HOME="$TMPDIR/cache-wasip3"
      batch_output="$TMPDIR/batch-wasip3"
      batch_sources=()
      for example_name in ${pkgs.lib.escapeShellArgs exampleNames}; do
        batch_sources+=("examples/$example_name/Main.hs")
      done
      timeout --foreground --kill-after=5s 120s ${aihcExe} compile-batch \
        --output-directory "$batch_output" \
        --target wasm32-wasip3 \
        --gc calloc \
        --use-wasm-opt \
        "''${batch_sources[@]}"

      pids=()
            ${pkgs.lib.concatMapStringsSep "\n" renderWasip3ExampleCase exampleNames}
            failed=0
            for pid in "''${pids[@]}"; do
              wait "$pid" || failed=1
            done
            test "$failed" -eq 0

            test -n "$(find "$TMPDIR" -path '*/aihc/libraries/*' -type f -name '*.o' -print -quit)"
            test -n "$(find "$TMPDIR" -path '*/aihc/libraries/*' -type f -name '*.a' -print -quit)"
            test "$(wc -l < "$AIHC_WASM_OPT_MARKER")" -eq ${toString (builtins.length exampleNames)}
            touch "$out"
    '';
in {
  amd64-tests = amd64Tests;
  arm64-tests = arm64Tests;
  c-tests = cBackendTests;
  native-tests = nativeTests;
  wasm-tests = wasmTests;
  fc-tests = fcTests;
  grin-tests = grinTests;
  resolve-tests = resolveTests;
  tc-tests = tcTests;
  testing-tests = testingTests;
  dev-tests = devTests;
  aihc-tests = aihcTests;
  fmt-tests = fmtTests;
  unicode-generated = unicodeGenerated;
  nix-lint = nixLint;
  nix-format = nixFormat;
  haskell-lint = haskellLint;
  haskell-format = haskellFormat;
  c-lint = cLint;
  c-format = cFormat;
  cabal-format = cabalFormat;
  examples-tests = examplesTests;
  wasip3-example-test = wasip3ExampleTest;
}
