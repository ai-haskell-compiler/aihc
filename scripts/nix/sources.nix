{root}: let
  mkComponentSrc = subpath: suffixes: pkgs:
    pkgs.lib.cleanSourceWith {
      src = root + subpath;
      filter = path: type: let
        baseName = baseNameOf path;
        matchesSuffix = builtins.any (suffix: pkgs.lib.hasSuffix suffix baseName) suffixes;
      in
        type == "directory" || matchesSuffix || baseName == "LICENSE";
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

  cppSrc = mkComponentSrc "/components/aihc-cpp" [
    ".hs"
    ".cabal"
    ".yaml"
    ".yml"
    ".tsv"
    ".inc"
  ];

  fcSrc = mkComponentSrc "/components/aihc-fc" [
    ".hs"
    ".cabal"
    ".yaml"
    ".yml"
  ];

  parserCliSrc = mkComponentSrc "/components/aihc-parser-cli" [
    ".hs"
    ".cabal"
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

  hackageSrc = mkComponentSrc "/tooling/aihc-hackage" [
    ".hs"
    ".cabal"
  ];

  devSrc = mkComponentSrc "/tooling/aihc-dev" [
    ".hs"
    ".cabal"
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

  # Filtered source for Haskell linting/formatting - .hs files and .cabal files in components and tooling.
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
      in
        type == "directory" || ((inComponents || inTooling) && (isCabal || (isHaskell && !isFixture)));
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

  # Filtered source for WASM builds - include all packages plus cabal.project.
  wasmBuildSrc = pkgs:
    pkgs.lib.cleanSourceWith {
      src = root;
      filter = path: type: let
        pathStr = toString path;
        baseName = baseNameOf path;
        inComponents =
          pkgs.lib.hasInfix "/components/aihc-parser/" pathStr
          || pkgs.lib.hasInfix "/components/aihc-parser-cli/" pathStr
          || pkgs.lib.hasInfix "/components/aihc-cpp/" pathStr
          || pkgs.lib.hasInfix "/tooling/aihc-hackage/" pathStr;
        isAllowedComponentFile = builtins.any (suffix: pkgs.lib.hasSuffix suffix baseName) [
          ".hs"
          ".cabal"
          ".yaml"
          ".yml"
          ".tsv"
          ".json"
          ".inc"
        ];
        isProject = baseName == "cabal.project";
        isWasmProjectFreeze = pkgs.lib.hasInfix "/scripts/nix/cabal.project.freeze" pathStr;
      in
        type
        == "directory"
        || isProject
        || isWasmProjectFreeze
        || (inComponents && (isAllowedComponentFile || baseName == "LICENSE"));
    };
}
