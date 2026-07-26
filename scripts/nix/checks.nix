{
  projectHsPackages,
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

  addHiddenSuccesses = old: {
    # Replace the package-build sentinel and hide passing test output.
    testFlags =
      builtins.filter
      (flag: !builtins.elem flag ["--pattern" "__nix-build-tests-without-running__"])
      (old.testFlags or [])
      ++ ["--hide-successes"];
  };

  addCheckSettings = drv: old:
    addHiddenSuccesses old
    // pkgs.lib.optionalAttrs (drv ? intermediates) {
      # Reuse the optimized build, including its already-compiled test components.
      doInstallIntermediates = false;
      enableSeparateIntermediatesOutput = false;
      previousIntermediates = drv.intermediates;
    };

  mkPackageTest = drv:
    pkgs.haskell.lib.doCheck (
      pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.overrideCabal drv (addCheckSettings drv))
    );

  mkEvalPackageTest = drv:
    pkgs.haskell.lib.doCheck (
      pkgs.haskell.lib.dontHaddock (
        pkgs.haskell.lib.overrideCabal drv (
          old:
            addCheckSettings drv old
            // {
              preCheck =
                (old.preCheck or "")
                + ''
                  export AIHC_BASE_SRC=${sources.baseSrc pkgs}
                  export AIHC_PRIM_SRC=${sources.primSrc pkgs}
                  export AIHC_EVAL_FIXTURES=${sources.evalFixturesSrc pkgs}
                '';
            }
        )
      )
    );

  mkAihcPackageTest = drv:
    pkgs.haskell.lib.doCheck (
      pkgs.haskell.lib.dontHaddock (
        pkgs.haskell.lib.overrideCabal drv (
          old:
            addCheckSettings drv old
            // {
              testToolDepends = (old.testToolDepends or []) ++ [pkgs.llvmPackages.clang];
              preCheck =
                (old.preCheck or "")
                + ''
                  export AIHC_BASE_SRC=${sources.baseSrc pkgs}
                  export AIHC_PRIM_SRC=${sources.primSrc pkgs}
                '';
            }
        )
      )
    );

  mkSourceCheck = name: src: nativeBuildInputs: text:
    pkgs.runCommand name {
      inherit src nativeBuildInputs;
    } ''
      cd "$src"
      ${text}
      touch "$out"
    '';

  compilationModes = [
    {
      name = "incremental";
      flags = [];
    }
    {
      name = "whole-program";
      flags = ["--whole-program"];
    }
  ];
  garbageCollectors = ["calloc" "semispace"];
  nativeBackendBySystem = {
    "aarch64-darwin" = "apple-arm64";
    "x86_64-linux" = "linux-amd64";
  };
  nativeBackend = nativeBackendBySystem.${pkgs.stdenv.hostPlatform.system} or null;
  backends = ["portable-c"] ++ pkgs.lib.optional (nativeBackend != null) nativeBackend;
  compilationMatrix = builtins.concatLists (
    map (
      backend:
        builtins.concatLists (
          map (compilation: map (gc: {inherit backend compilation gc;}) garbageCollectors) compilationModes
        )
    )
    backends
  );
  smokeCompilation = builtins.head compilationModes;
  smokeCompilationMatrix = [
    {
      backend = "portable-c";
      compilation = smokeCompilation;
      gc = "calloc";
    }
  ];
  exampleCompilationMatrix = exampleName:
    if exampleName == "hello-world"
    then compilationMatrix
    else smokeCompilationMatrix;
  wasip3CompilationModes = exampleName:
    if exampleName == "hello-world"
    then compilationModes
    else [smokeCompilation];

  renderExampleTest = {
    backend,
    compilation,
    gc,
  }: ''
    executable="$TMPDIR/$example_name-${backend}-${compilation.name}-${gc}"
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
    if timeout --foreground --kill-after=5s 120s ${aihcExe} compile "$source" \
      --target ${backend} \
      --gc ${gc} \
      ${pkgs.lib.escapeShellArgs compilation.flags} \
      --output "$executable"; then
      :
    else
      compile_exit=$?
      if [[ "$compile_exit" -eq 124 || "$compile_exit" -eq 137 ]]; then
        echo "Timed out compiling $example_name/${backend}-${compilation.name}-${gc}" >&2
      else
        echo "Compiler failed for $example_name/${backend}-${compilation.name}-${gc} with exit $compile_exit" >&2
      fi
      exit "$compile_exit"
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
      echo "Timed out running $example_name/${backend}-${compilation.name}-${gc}" >&2
      cat "$timeout_stderr" >&2
      exit 1
    fi
    if [[ "$expected_exit" == nonzero ]]; then
      if [[ "$actual_exit" -eq 0 ]]; then
        echo "Expected $example_name/${backend}-${compilation.name}-${gc} to fail" >&2
        exit 1
      fi
    elif [[ "$expected_exit" =~ ^[0-9]+$ ]]; then
      if [[ "$actual_exit" -ne "$expected_exit" ]]; then
        echo "Expected $example_name/${backend}-${compilation.name}-${gc} to exit with $expected_exit, got $actual_exit" >&2
        exit 1
      fi
    else
      echo "Invalid expected exit status for $example_name: $expected_exit" >&2
      exit 1
    fi
    diff --unified \
      --label "$example_name/stdout-expected" \
      --label "$example_name/stdout-${backend}-${compilation.name}-${gc}" \
      "$expected_stdout" "$actual_stdout"
    diff --unified \
      --label "$example_name/stderr-expected" \
      --label "$example_name/stderr-${backend}-${compilation.name}-${gc}" \
      "$expected_stderr" "$actual_stderr"
  '';

  renderWasip3ExampleTest = compilation: ''
    executable="$TMPDIR/$example_name-wasm32-wasip3-${compilation.name}.wasm"
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
    if timeout --foreground --kill-after=5s 120s ${aihcExe} compile "$source" \
      --target wasm32-wasip3 \
      --use-wasm-opt \
      ${pkgs.lib.escapeShellArgs compilation.flags} \
      --output "$executable"; then
      :
    else
      compile_exit=$?
      if [[ "$compile_exit" -eq 124 || "$compile_exit" -eq 137 ]]; then
        echo "Timed out compiling $example_name/wasm32-wasip3-${compilation.name}" >&2
      else
        echo "Compiler failed for $example_name/wasm32-wasip3-${compilation.name} with exit $compile_exit" >&2
      fi
      exit "$compile_exit"
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
      echo "Timed out running $example_name/wasm32-wasip3-${compilation.name}" >&2
      exit 1
    fi
    if [[ "$expected_exit" == nonzero ]]; then
      if [[ "$actual_exit" -eq 0 ]]; then
        echo "Expected $example_name/wasm32-wasip3-${compilation.name} to fail" >&2
        exit 1
      fi
    elif [[ "$expected_exit" =~ ^[0-9]+$ ]]; then
      if [[ "$actual_exit" -ne "$expected_exit" ]]; then
        echo "Expected $example_name/wasm32-wasip3-${compilation.name} to exit with $expected_exit, got $actual_exit" >&2
        exit 1
      fi
    else
      echo "Invalid expected exit status for $example_name: $expected_exit" >&2
      exit 1
    fi
    diff --unified \
      --label "$example_name/stdout-expected" \
      --label "$example_name/stdout-wasm32-wasip3-${compilation.name}" \
      "$expected_stdout" "$actual_stdout"
    if [[ "$expected_exit" != nonzero ]]; then
      diff --unified \
        --label "$example_name/stderr-expected" \
        --label "$example_name/stderr-wasm32-wasip3-${compilation.name}" \
        "$expected_stderr" "$actual_stderr"
    fi
  '';

  cppProgressEnv = hsPkgs.ghcWithPackages (p: [
    p.aihc-cpp
    p.cpphs
  ]);
  parserProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser-tooling-common "parser-progress";
  lexerProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser-tooling-common "lexer-progress";
  parserExtensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser-tooling-common "parser-extension-progress";
  aihcExe = pkgs.lib.getExe' hsPkgs.aihc "aihc";

  parserTests = mkPackageTest hsPkgs.aihc-parser;
  cppTests = mkPackageTest hsPkgs.aihc-cpp;
  amd64Tests = mkEvalPackageTest (
    pkgs.haskell.lib.overrideCabal hsPkgs.aihc-amd64 (old: {
      testToolDepends = (old.testToolDepends or []) ++ [pkgs.llvmPackages.clang];
    })
  );
  arm64Tests = mkEvalPackageTest hsPkgs.aihc-arm64;
  cBackendTests = mkEvalPackageTest (
    pkgs.haskell.lib.overrideCabal hsPkgs.aihc-c (old: {
      testToolDepends = (old.testToolDepends or []) ++ [pkgs.llvmPackages.clang];
    })
  );
  nativeTests = mkPackageTest hsPkgs.aihc-native;
  wasmTests = mkPackageTest hsPkgs.aihc-wasm;
  fcTests = mkEvalPackageTest hsPkgs.aihc-fc;
  grinTests = mkEvalPackageTest hsPkgs.aihc-grin;
  resolveTests = mkPackageTest hsPkgs.aihc-resolve;
  tcTests = mkPackageTest hsPkgs.aihc-tc;
  testingTests = mkPackageTest hsPkgs.aihc-testing;
  devTests = mkPackageTest hsPkgs.aihc-dev;
  aihcTests = mkAihcPackageTest hsPkgs.aihc;
  fmtTests = mkPackageTest hsPkgs.aihc-fmt;
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
    pkgs.ghc
    pkgs.llvmPackages.clang
  ];

  mkExampleTest = exampleName:
    mkSourceCheck "aihc-example-${exampleName}" (sources.exampleSrc exampleName pkgs) exampleTestInputs ''
      set -euo pipefail
      export XDG_CACHE_HOME="$TMPDIR/cache"
      export GHCRTS=-N1
      empty_stderr="$TMPDIR/empty-stderr"
      touch "$empty_stderr"

      source="examples/${exampleName}/Main.hs"
      example_directory=$(dirname "$source")
      example_name=${pkgs.lib.escapeShellArg exampleName}
      expected_stdout="$example_directory/stdout"
      if [[ ! -f "$expected_stdout" ]]; then
        echo "Missing expected stdout for $source: $expected_stdout" >&2
        exit 1
      fi

      if [[ "$example_name" == mvars ]]; then
        ghc_executable="$TMPDIR/$example_name-ghc"
        ghc_output_directory="$TMPDIR/$example_name-ghc-output"
        mkdir -p "$ghc_output_directory"
        ghc -v0 \
          -outputdir "$ghc_output_directory" \
          -o "$ghc_executable" \
          "$source"
        env -u GHCRTS timeout --foreground --kill-after=5s 10s "$ghc_executable" > "$ghc_executable.stdout"
        diff --unified \
          --label "$example_name/expected" \
          --label "$example_name/ghc-non-threaded" \
          "$expected_stdout" "$ghc_executable.stdout"
      fi

      ${pkgs.lib.concatMapStringsSep "\n" renderExampleTest (exampleCompilationMatrix exampleName)}
      touch "$out"
    '';

  exampleCases =
    map (exampleName: {
      name = exampleName;
      path = mkExampleTest exampleName;
    })
    exampleNames;

  # Every example gets one portable-C smoke test. Hello World also carries the
  # complete backend/mode/GC matrix, avoiding a cross-product for every program.
  # Nix schedules the independent examples in parallel.
  examplesTests = assert exampleNames != [];
    pkgs.linkFarm "aihc-examples-tests" exampleCases;

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

  mkWasip3ExampleTest = exampleName:
    mkSourceCheck "aihc-wasip3-example-${exampleName}" (sources.exampleSrc exampleName pkgs) wasip3ExampleInputs ''
      set -euo pipefail
      export XDG_CACHE_HOME="$TMPDIR/cache"
      export GHCRTS=-N1
      export AIHC_WASM_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
      export AIHC_WASM_OPT_MARKER="$TMPDIR/wasm-opt-invocations"
      empty_stderr="$TMPDIR/empty-stderr"
      touch "$empty_stderr"

      source="examples/${exampleName}/Main.hs"
      example_directory=$(dirname "$source")
      example_name=${pkgs.lib.escapeShellArg exampleName}
      expected_stdout="$example_directory/stdout"
      if [[ ! -f "$expected_stdout" ]]; then
        echo "Missing expected stdout for $source: $expected_stdout" >&2
        exit 1
      fi

      ${pkgs.lib.concatMapStringsSep "\n" renderWasip3ExampleTest (wasip3CompilationModes exampleName)}

      test -n "$(find "$XDG_CACHE_HOME/aihc/libraries" -type f -name '*.o' -print -quit)"
      test -n "$(find "$XDG_CACHE_HOME/aihc/libraries" -type f -name '*.a' -print -quit)"
      test "$(wc -l < "$AIHC_WASM_OPT_MARKER")" -eq ${toString (builtins.length (wasip3CompilationModes exampleName))}
      touch "$out"
    '';

  wasip3ExampleCases =
    map (exampleName: {
      name = exampleName;
      path = mkWasip3ExampleTest exampleName;
    })
    exampleNames;

  # Every example gets one incremental WASI smoke test. Hello World additionally
  # covers whole-program mode. Nix schedules these derivations in parallel.
  wasip3ExampleTest = assert exampleNames != [];
    pkgs.linkFarm "aihc-wasip3-example-test" wasip3ExampleCases;

  parserProgressStrict = mkSourceCheck "aihc-parser-progress-strict" (sources.parserSrc pkgs) [] ''
    ${parserProgressExe} --strict
  '';

  lexerProgressStrict = mkSourceCheck "aihc-lexer-progress-strict" (sources.parserSrc pkgs) [] ''
    ${lexerProgressExe} --strict
  '';

  parserExtensionProgressStrict = mkSourceCheck "aihc-parser-extension-progress-strict" (sources.parserSrc pkgs) [] ''
    ${parserExtensionProgressExe} --strict
  '';

  cppProgressStrict = mkSourceCheck "aihc-cpp-progress-strict" (sources.cppSrc pkgs) [cppProgressEnv] ''
    runghc -package-env - -itest app/cpp-progress/Main.hs --strict
  '';

  cppDoctest =
    mkSourceCheck "aihc-cpp-doctest" (sources.cppSrc pkgs) [
      (projectHsPackages pkgs).doctest
      (projectHsPackages pkgs).ghc
      hsPkgs.aihc-cpp
    ] ''
      # Run doctest on the Aihc.Cpp module.
      doctest -XGHC2021 -isrc src/Aihc/Cpp.hs
    '';

  parserDoctest = let
    ghcEnv = hsPkgs.ghcWithPackages (p: [
      p.aihc-parser
      p.doctest
    ]);
  in
    mkSourceCheck "aihc-parser-doctest" (sources.parserSrc pkgs) [ghcEnv] ''
      # Find the GHC package database from ghcWithPackages.
      PKGDB=$(ghc --print-global-package-db)
      # Include all source files so imports between modules work.
      doctest -XGHC2021 -package-db="$PKGDB" -isrc \
        src/Aihc/Parser/Parens.hs \
        src/Aihc/Parser/Pretty.hs \
        src/Aihc/Parser/Shorthand.hs \
        src/Aihc/Parser.hs
    '';
in {
  parser-tests = parserTests;
  cpp-tests = cppTests;
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
  cpp-doctest = cppDoctest;
  parser-doctest = parserDoctest;
  parser-progress-strict = parserProgressStrict;
  lexer-progress-strict = lexerProgressStrict;
  parser-extension-progress-strict = parserExtensionProgressStrict;
  cpp-progress-strict = cppProgressStrict;
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
