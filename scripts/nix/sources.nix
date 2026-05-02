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

  # aihc-dev's cabal file uses relative ../../ paths to reach into components/.
  # callCabal2nix copies the source into a build sandbox where those paths break,
  # so we vendor the external directories and rewrite the cabal file to match.
  devSrc = pkgs: let
    devFiles = mkComponentSrc "/tooling/aihc-dev" [".hs" ".cabal"];
    parserCommon = mkComponentSrc "/components/aihc-parser/common" [".hs"];
    parserApp = mkComponentSrc "/components/aihc-parser/app/stackage-progress" [".hs"];
    resolveApp = mkComponentSrc "/components/aihc-resolve/app/resolve-stackage-progress" [".hs"];
  in
    pkgs.runCommand "aihc-dev-src" {} ''
      mkdir -p "$out/vendor/aihc-parser" "$out/vendor/aihc-resolve"

      cp -r ${devFiles pkgs}/. "$out"
      ln -s ${parserCommon pkgs} "$out/vendor/aihc-parser/common"
      ln -s ${parserApp pkgs} "$out/vendor/aihc-parser/stackage-progress"
      ln -s ${resolveApp pkgs} "$out/vendor/aihc-resolve/resolve-stackage-progress"

      substituteInPlace "$out/aihc-dev.cabal" \
        --replace-fail '../../components/aihc-parser/common' 'vendor/aihc-parser/common' \
        --replace-fail '../../components/aihc-parser/app/stackage-progress' 'vendor/aihc-parser/stackage-progress' \
        --replace-fail '../../components/aihc-resolve/app/resolve-stackage-progress' 'vendor/aihc-resolve/resolve-stackage-progress'
    '';

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
