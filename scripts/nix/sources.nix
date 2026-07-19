{root}: let
  matchesSuffix = pkgs: suffixes: path: let
    baseName = baseNameOf path;
  in
    builtins.any (suffix: pkgs.lib.hasSuffix suffix baseName) suffixes;

  mkComponentSrc = subpath: suffixes: pkgs:
    pkgs.lib.cleanSourceWith {
      src = root + subpath;
      filter = path: type: let
        baseName = baseNameOf path;
        matchesSourceSuffix = matchesSuffix pkgs suffixes path;
      in
        type == "directory" || matchesSourceSuffix || baseName == "LICENSE" || baseName == "CHANGELOG.md";
    };

  mkRootSubsetSrc = prefixes: suffixes: pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
        relPath = pkgs.lib.removePrefix ((toString root) + "/") (toString path);
        inSubset = builtins.any (prefix: pkgs.lib.hasPrefix prefix relPath) prefixes;
        matchesSourceSuffix = matchesSuffix pkgs suffixes path;
      in
        type == "directory" || (inSubset && (matchesSourceSuffix || baseName == "LICENSE" || baseName == "CHANGELOG.md"));
    };
in rec {
  # Source filtering: only include relevant files for each component.
  # This prevents rebuilds when unrelated files change.
  parserSrc = mkComponentSrc "/components/aihc-parser" [
    ".hs"
    ".hs-boot"
    ".cabal"
    ".yaml"
    ".yml"
    ".tsv"
    ".json"
  ];

  parserCompatSrc = mkComponentSrc "/components/aihc-parser-compat" [
    ".hs"
    ".cabal"
  ];

  cppSrc = mkComponentSrc "/components/aihc-cpp" [
    ".hs"
    ".cabal"
    ".yaml"
    ".yml"
    ".tsv"
    ".inc"
  ];

  fcSrc = mkRootSubsetSrc ["components/aihc-fc/" "test/support/"] [
    ".hs"
    ".cabal"
    ".yaml"
    ".yml"
  ];

  arm64Src = mkRootSubsetSrc ["components/aihc-arm64/" "test/support/"] [
    ".hs"
    ".cabal"
    ".c"
    ".h"
    ".yaml"
    ".yml"
  ];

  grinSrc = mkRootSubsetSrc ["components/aihc-grin/" "test/support/"] [
    ".hs"
    ".cabal"
  ];

  evalFixturesSrc = mkComponentSrc "/test/Test/Fixtures/eval" [
    ".yaml"
    ".yml"
  ];

  resolveSrc = mkComponentSrc "/components/aihc-resolve" [
    ".hs"
    ".cabal"
    ".yaml"
    ".yml"
  ];

  tcSrc = mkComponentSrc "/components/aihc-tc" [
    ".hs"
    ".cabal"
    ".yaml"
    ".yml"
  ];

  baseSrc = mkComponentSrc "/core-libs/aihc-base" [
    ".hs"
    ".cabal"
  ];

  hackageSrc = mkComponentSrc "/tooling/aihc-hackage" [
    ".hs"
    ".cabal"
  ];

  testingSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
        relPath = pkgs.lib.removePrefix ((toString root) + "/") (toString path);
        inTesting = pkgs.lib.hasPrefix "tooling/aihc-testing/" relPath;
        matchesSourceSuffix = matchesSuffix pkgs [".hs" ".cabal"] path;
      in
        type == "directory" || (inTesting && (matchesSourceSuffix || baseName == "LICENSE" || baseName == "CHANGELOG.md"));
    };

  primSrc = mkComponentSrc "/core-libs/aihc-prim" [
    ".hs"
    ".cabal"
  ];

  internalSrc = mkComponentSrc "/core-libs/aihc-internal" [
    ".hs"
    ".cabal"
  ];

  devSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
        relPath = pkgs.lib.removePrefix ((toString root) + "/") (toString path);
        inDev = pkgs.lib.hasPrefix "tooling/aihc-dev/" relPath;
        inTcCommon = pkgs.lib.hasPrefix "components/aihc-tc/common/" relPath;
        inResolveCommon = pkgs.lib.hasPrefix "components/aihc-resolve/common/" relPath;
        matchesSourceSuffix = matchesSuffix pkgs [".hs" ".cabal" ".yaml" ".yml"] path;
      in
        type == "directory" || ((inDev || inTcCommon || inResolveCommon) && (matchesSourceSuffix || baseName == "LICENSE" || baseName == "CHANGELOG.md"));
    };

  parserToolingCommonSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
        relPath = pkgs.lib.removePrefix ((toString root) + "/") (toString path);
        inToolingCommon = pkgs.lib.hasPrefix "tooling/aihc-parser-tooling-common/" relPath;
        inParserCommon = pkgs.lib.hasPrefix "components/aihc-parser/common/" relPath;
        inParserApp = pkgs.lib.hasPrefix "components/aihc-parser/app/" relPath;
        matchesSourceSuffix = matchesSuffix pkgs [".hs" ".cabal"] path;
      in
        type == "directory" || ((inToolingCommon || inParserCommon || inParserApp) && matchesSourceSuffix);
    };

  resolveToolingCommonSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        relPath = pkgs.lib.removePrefix ((toString root) + "/") (toString path);
        inToolingCommon = pkgs.lib.hasPrefix "tooling/aihc-resolve-tooling-common/" relPath;
        inResolveCommon = pkgs.lib.hasPrefix "components/aihc-resolve/common/" relPath;
        matchesSourceSuffix = matchesSuffix pkgs [".hs" ".cabal"] path;
      in
        type == "directory" || ((inToolingCommon || inResolveCommon) && matchesSourceSuffix);
    };

  aihcSrc = mkRootSubsetSrc ["bin/aihc/" "core-libs/" "examples/hello-world/"] [
    ".hs"
    ".cabal"
  ];

  fmtSrc = mkComponentSrc "/bin/aihc-fmt" [
    ".hs"
    ".cabal"
    ".yaml"
    ".yml"
  ];

  # Filtered source for nix linting - only nix files.
  nixSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
      in
        type == "directory" || pkgs.lib.hasSuffix ".nix" baseName;
    };

  # Filtered source for Haskell linting/formatting - .hs files and .cabal files in components, tooling, and bin.
  # (.cabal files needed for ormolu to detect language settings like GHC2021)
  haskellSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
        pathStr = toString path;
        isHaskell = pkgs.lib.hasSuffix ".hs" baseName;
        isCabal = pkgs.lib.hasSuffix ".cabal" baseName;
        isFixture = pkgs.lib.hasInfix "/test/Test/Fixtures/" pathStr;
        inComponents = pkgs.lib.hasInfix "/components/" pathStr;
        inTooling = pkgs.lib.hasInfix "/tooling/" pathStr;
        inBin = pkgs.lib.hasInfix "/bin/" pathStr;
        inCoreLibs = pkgs.lib.hasInfix "/core-libs/" pathStr;
        inNixHaskell = pkgs.lib.hasInfix "/scripts/nix/ucd2haskell-aihc/" pathStr;
        inTestSupport = pkgs.lib.hasInfix "/test/support/" pathStr;
      in
        type == "directory" || ((inComponents || inTooling || inBin || inCoreLibs || inNixHaskell || inTestSupport) && (isCabal || (isHaskell && !isFixture)));
    };

  # Filtered source for C linting/formatting, including tool configuration.
  cSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
        relPath = pkgs.lib.removePrefix ((toString root) + "/") (toString path);
        isBuildOutput = relPath == "dist-newstyle" || pkgs.lib.hasPrefix "dist-newstyle/" relPath;
        isCSource = pkgs.lib.hasSuffix ".c" baseName || pkgs.lib.hasSuffix ".h" baseName;
        isCConfig = baseName == ".clang-format" || baseName == ".clang-tidy";
      in
        !isBuildOutput && (type == "directory" || isCSource || isCConfig);
    };

  # Filtered source for scripts - only shell scripts.
  scriptsSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        baseName = baseNameOf path;
      in
        type == "directory" || (pkgs.lib.hasSuffix ".sh" baseName && pkgs.lib.hasInfix "/scripts" (toString path));
    };
}
