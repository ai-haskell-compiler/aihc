{
  ghc-wasm-meta,
  wasmBuildSrc,
}: {
  mkParserWasmWasi = pkgs: let
    wasmTools = ghc-wasm-meta.packages.${pkgs.stdenv.hostPlatform.system}.all_9_10;
    wasmCabalCache = pkgs.stdenvNoCC.mkDerivation {
      pname = "aihc-parser-wasm-cabal-cache";
      version = "0.1.0.0";
      src = wasmBuildSrc pkgs;
      nativeBuildInputs = [wasmTools];
      buildPhase = ''
        runHook preBuild
        export HOME="$out/home"
        mkdir -p "$HOME"
        chmod -R u+w .

        wasm32-wasi-cabal update
        wasm32-wasi-cabal build \
          --only-download \
          aihc-parser:exe:aihc-parser \
          --project-file=cabal.project

        mkdir -p "$out"
        cp -R "$HOME/.ghc-wasm/.cabal/packages" "$out/packages"
        runHook postBuild
      '';
      outputHashMode = "recursive";
      outputHashAlgo = "sha256";
      outputHash = "sha256-pZRfPkvtGkvpy8SRcD7uWaEyC3JVww3op7uabc6Zsas=";
    };
  in
    pkgs.stdenvNoCC.mkDerivation {
      pname = "aihc-parser-wasm-wasi";
      version = "0.1.0.0";
      src = wasmBuildSrc pkgs;
      nativeBuildInputs = [
        wasmTools
        pkgs.findutils
      ];
      buildPhase = ''
        runHook preBuild
        export HOME="$TMPDIR/home"
        mkdir -p "$HOME"
        mkdir -p "$HOME/.ghc-wasm/.cabal"
        mkdir -p "$HOME/.ghc-wasm/.cabal/packages"
        cp -R ${wasmCabalCache}/packages/. "$HOME/.ghc-wasm/.cabal/packages/"
        chmod -R u+w "$HOME/.ghc-wasm/.cabal"
        chmod -R u+w .

        mkdir -p .wasm-offline-deps
        find ${wasmCabalCache}/packages -type f -name '*.tar.gz' ! -name '01-index.tar.gz' -exec cp {} .wasm-offline-deps/ \;
        printf '%s\n' \
          "import: cabal.project" \
          "" \
          "packages:" \
          "  ./.wasm-offline-deps/*.tar.gz" \
          > cabal.project.offline
        cp scripts/nix/cabal.project.freeze cabal.project.offline.freeze

        wasm32-wasi-cabal build \
          --offline \
          aihc-parser:exe:aihc-parser \
          --project-file=cabal.project.offline
        runHook postBuild
      '';
      installPhase = ''
        runHook preInstall
        mkdir -p "$out/bin"
        find dist-newstyle -type f -name 'aihc-parser.wasm' -exec cp {} "$out/bin/aihc-parser.wasm" \;
        test -f "$out/bin/aihc-parser.wasm"
        runHook postInstall
      '';
      meta.description = "WASM-WASI build of aihc-parser using ghc-wasm-meta";
    };
}
