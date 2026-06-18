{
  projectHsPackages,
  sources,
  mkHsPkgsForChecks,
}: pkgs: let
  hsPkgs = mkHsPkgsForChecks pkgs;

  addHiddenSuccesses = old: {
    # Hide passing tests so failures are visible in Nix's truncated output.
    testFlags = (old.testFlags or []) ++ ["--hide-successes"];
  };

  mkPackageTest = drv:
    pkgs.haskell.lib.doCheck (
      pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.overrideCabal drv addHiddenSuccesses)
    );

  mkFcPackageTest = drv:
    pkgs.haskell.lib.doCheck (
      pkgs.haskell.lib.dontHaddock (
        pkgs.haskell.lib.overrideCabal drv (
          old:
            addHiddenSuccesses old
            // {
              preCheck =
                (old.preCheck or "")
                + ''
                  export AIHC_BASE_SRC=${sources.baseSrc pkgs}
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
            addHiddenSuccesses old
            // {
              preCheck =
                (old.preCheck or "")
                + ''
                  export AIHC_BASE_SRC=${sources.baseSrc pkgs}
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

  cppProgressEnv = hsPkgs.ghcWithPackages (p: [
    p.aihc-cpp
    p.cpphs
  ]);
  parserProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser-tooling-common "parser-progress";
  lexerProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser-tooling-common "lexer-progress";
  parserExtensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser-tooling-common "parser-extension-progress";

  parserTests = mkPackageTest hsPkgs.aihc-parser;
  cppTests = mkPackageTest hsPkgs.aihc-cpp;
  fcTests = mkFcPackageTest hsPkgs.aihc-fc;
  resolveTests = mkPackageTest hsPkgs.aihc-resolve;
  tcTests = mkPackageTest hsPkgs.aihc-tc;
  testingTests = mkPackageTest hsPkgs.aihc-testing;
  devTests = mkPackageTest hsPkgs.aihc-dev;
  aihcTests = mkAihcPackageTest hsPkgs.aihc;
  fmtTests = mkPackageTest hsPkgs.aihc-fmt;

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

  cabalFormat = mkSourceCheck "aihc-cabal-format" (sources.haskellSrc pkgs) [pkgs.haskellPackages.cabal-gild pkgs.findutils] ''
    failed=0
    while IFS= read -r -d "" file; do
      cabal-gild --mode check --input "$file" || failed=1
    done < <(find . -type f -name '*.cabal' -print0)
    test "$failed" -eq 0
  '';

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
  fc-tests = fcTests;
  resolve-tests = resolveTests;
  tc-tests = tcTests;
  testing-tests = testingTests;
  dev-tests = devTests;
  aihc-tests = aihcTests;
  fmt-tests = fmtTests;
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
  cabal-format = cabalFormat;

  all-tests = pkgs.linkFarm "aihc-all-tests" [
    {
      name = "parser-tests";
      path = parserTests;
    }
    {
      name = "cpp-tests";
      path = cppTests;
    }
    {
      name = "fc-tests";
      path = fcTests;
    }
    {
      name = "resolve-tests";
      path = resolveTests;
    }
    {
      name = "tc-tests";
      path = tcTests;
    }
    {
      name = "testing-tests";
      path = testingTests;
    }
    {
      name = "dev-tests";
      path = devTests;
    }
    {
      name = "aihc-tests";
      path = aihcTests;
    }
    {
      name = "fmt-tests";
      path = fmtTests;
    }
    {
      name = "cpp-doctest";
      path = cppDoctest;
    }
    {
      name = "parser-doctest";
      path = parserDoctest;
    }
    {
      name = "parser-progress-strict";
      path = parserProgressStrict;
    }
    {
      name = "lexer-progress-strict";
      path = lexerProgressStrict;
    }
    {
      name = "parser-extension-progress-strict";
      path = parserExtensionProgressStrict;
    }
    {
      name = "cpp-progress-strict";
      path = cppProgressStrict;
    }
    {
      name = "nix-lint";
      path = nixLint;
    }
    {
      name = "nix-format";
      path = nixFormat;
    }
    {
      name = "haskell-lint";
      path = haskellLint;
    }
    {
      name = "haskell-format";
      path = haskellFormat;
    }
    {
      name = "cabal-format";
      path = cabalFormat;
    }
  ];
}
