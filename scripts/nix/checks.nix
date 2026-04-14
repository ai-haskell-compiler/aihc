{
  projectHsPackages,
  sources,
  mkHsPkgsForChecks,
  mkHsPkgsWithTestsForChecks,
  mkCombinedDocsForChecks,
}: pkgs: let
  hsPkgs = mkHsPkgsForChecks pkgs;
  hsPkgsWithTests = mkHsPkgsWithTestsForChecks pkgs;

  mkSourceCheck = name: src: nativeBuildInputs: text:
    pkgs.runCommand name {
      inherit src nativeBuildInputs;
    } ''
      cd "$src"
      ${text}
      touch "$out"
    '';

  mkProgressCheck = name: src: package: command:
    mkSourceCheck name src [package] command;

  parserTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgsWithTests.aihc-parser);
  parserCliTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgsWithTests.aihc-parser-cli);
  cppTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgsWithTests.aihc-cpp);
  resolveTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgsWithTests.aihc-resolve);
  tcTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgsWithTests.aihc-tc);

  nixLint = mkSourceCheck "aihc-nix-lint" (sources.nixSrc pkgs) [pkgs.statix] ''
    statix check flake.nix
  '';

  nixFormat = mkSourceCheck "aihc-nix-format" (sources.nixSrc pkgs) [pkgs.alejandra] ''
    alejandra --check .
  '';

  haskellLint = mkSourceCheck "aihc-haskell-lint" (sources.haskellSrc pkgs) [(projectHsPackages pkgs).hlint pkgs.findutils] ''
    find . -type f -name '*.hs' -print0 \
      | xargs -0 -r hlint
  '';

  haskellFormat = mkSourceCheck "aihc-haskell-format" (sources.haskellSrc pkgs) [(projectHsPackages pkgs).ormolu pkgs.findutils] ''
    find . -type f -name '*.hs' -print0 \
      | xargs -0 -r ormolu --mode check
  '';

  parserProgressStrict = mkProgressCheck "aihc-parser-progress-strict" (sources.parserSrc pkgs) hsPkgs.aihc-parser ''
    parser-progress --strict
  '';

  lexerProgressStrict = mkProgressCheck "aihc-lexer-progress-strict" (sources.parserSrc pkgs) hsPkgs.aihc-parser ''
    lexer-progress --strict
  '';

  parserExtensionProgressStrict = mkProgressCheck "aihc-parser-extension-progress-strict" (sources.parserSrc pkgs) hsPkgs.aihc-parser ''
    extension-progress --strict
  '';

  cppProgressStrict = mkProgressCheck "aihc-cpp-progress-strict" (sources.cppSrc pkgs) hsPkgs.aihc-cpp ''
    cpp-progress --strict
  '';

  haddockDocs = mkCombinedDocsForChecks pkgs;

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
        src/Aihc/Parser/Shorthand.hs \
        src/Aihc/Parser.hs
    '';
in {
  parser-tests = parserTests;
  parser-cli-tests = parserCliTests;
  cpp-tests = cppTests;
  resolve-tests = resolveTests;
  tc-tests = tcTests;
  cpp-doctest = cppDoctest;
  parser-doctest = parserDoctest;
  haddock-docs = haddockDocs;
  parser-progress-strict = parserProgressStrict;
  lexer-progress-strict = lexerProgressStrict;
  parser-extension-progress-strict = parserExtensionProgressStrict;
  cpp-progress-strict = cppProgressStrict;
  nix-lint = nixLint;
  nix-format = nixFormat;
  haskell-lint = haskellLint;
  haskell-format = haskellFormat;

  all-tests = pkgs.linkFarm "aihc-all-tests" [
    {
      name = "parser-tests";
      path = parserTests;
    }
    {
      name = "parser-cli-tests";
      path = parserCliTests;
    }
    {
      name = "cpp-tests";
      path = cppTests;
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
      name = "cpp-doctest";
      path = cppDoctest;
    }
    {
      name = "parser-doctest";
      path = parserDoctest;
    }
    {
      name = "haddock-docs";
      path = haddockDocs;
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
  ];
}
