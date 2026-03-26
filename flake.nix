{
  description = "aihc development flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    ghc-wasm-meta.url = "git+https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta.git";
  };

  outputs = {
    self,
    nixpkgs,
    ghc-wasm-meta,
  }: let
    systems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs {inherit system;}));

    # Source filtering: only include relevant files for each component
    # This prevents rebuilds when unrelated files change
    parserSrc = pkgs:
      pkgs.lib.cleanSourceWith {
        src = ./components/aihc-parser;
        filter = path: type: let
          baseName = baseNameOf path;
          # Include Haskell sources, cabal file, and test fixtures
          isHaskell = pkgs.lib.hasSuffix ".hs" baseName;
          isCabal = pkgs.lib.hasSuffix ".cabal" baseName;
          isYaml = pkgs.lib.hasSuffix ".yaml" baseName || pkgs.lib.hasSuffix ".yml" baseName;
          isTsv = pkgs.lib.hasSuffix ".tsv" baseName;
          isJson = pkgs.lib.hasSuffix ".json" baseName;
          isLicense = baseName == "LICENSE";
          isDir = type == "directory";
        in
          isDir || isHaskell || isCabal || isYaml || isTsv || isJson || isLicense;
      };

    cppSrc = pkgs:
      pkgs.lib.cleanSourceWith {
        src = ./components/aihc-cpp;
        filter = path: type: let
          baseName = baseNameOf path;
          isHaskell = pkgs.lib.hasSuffix ".hs" baseName;
          isCabal = pkgs.lib.hasSuffix ".cabal" baseName;
          isYaml = pkgs.lib.hasSuffix ".yaml" baseName || pkgs.lib.hasSuffix ".yml" baseName;
          isTsv = pkgs.lib.hasSuffix ".tsv" baseName;
          isInc = pkgs.lib.hasSuffix ".inc" baseName; # Include files for CPP tests
          isLicense = baseName == "LICENSE";
          isDir = type == "directory";
        in
          isDir || isHaskell || isCabal || isYaml || isTsv || isInc || isLicense;
      };

    # Filtered source for nix linting - only nix files
    nixSrc = pkgs:
      pkgs.lib.cleanSourceWith {
        src = ./.;
        filter = path: type: let
          baseName = baseNameOf path;
          isNix = pkgs.lib.hasSuffix ".nix" baseName;
        in
          type == "directory" || isNix;
      };

    # Filtered source for Haskell linting/formatting - .hs files and .cabal files in components
    # (.cabal files needed for ormolu to detect language settings like GHC2021)
    haskellSrc = pkgs:
      pkgs.lib.cleanSourceWith {
        src = ./components;
        filter = path: type: let
          baseName = baseNameOf path;
          isHaskell = pkgs.lib.hasSuffix ".hs" baseName;
          isCabal = pkgs.lib.hasSuffix ".cabal" baseName;
          # Exclude test fixtures from linting
          pathStr = toString path;
          isFixture = pkgs.lib.hasInfix "/test/Test/Fixtures/" pathStr;
        in
          type == "directory" || isCabal || (isHaskell && !isFixture);
      };

    # Filtered source for scripts - only shell scripts
    scriptsSrc = pkgs:
      pkgs.lib.cleanSourceWith {
        src = ./.;
        filter = path: type: let
          baseName = baseNameOf path;
          isSh = pkgs.lib.hasSuffix ".sh" baseName;
          isScriptsDir = pkgs.lib.hasInfix "/scripts" (toString path);
        in
          type == "directory" || (isSh && isScriptsDir);
      };

    # Filtered source for WASM builds - include both packages plus cabal.project
    wasmBuildSrc = pkgs:
      pkgs.lib.cleanSourceWith {
        src = ./.;
        filter = path: type: let
          pathStr = toString path;
          baseName = baseNameOf path;
          isDir = type == "directory";
          inComponents =
            pkgs.lib.hasInfix "/components/aihc-parser/" pathStr
            || pkgs.lib.hasInfix "/components/aihc-cpp/" pathStr;
          isHaskell = pkgs.lib.hasSuffix ".hs" baseName;
          isCabal = pkgs.lib.hasSuffix ".cabal" baseName;
          isYaml = pkgs.lib.hasSuffix ".yaml" baseName || pkgs.lib.hasSuffix ".yml" baseName;
          isTsv = pkgs.lib.hasSuffix ".tsv" baseName;
          isJson = pkgs.lib.hasSuffix ".json" baseName;
          isInc = pkgs.lib.hasSuffix ".inc" baseName;
          isLicense = baseName == "LICENSE";
          isProject = baseName == "cabal.project";
          isWasmProjectFreeze = pkgs.lib.hasInfix "/scripts/nix/cabal.project.freeze" pathStr;
        in
          isDir
          || isProject
          || isWasmProjectFreeze
          || (inComponents && (isHaskell || isCabal || isYaml || isTsv || isJson || isInc || isLicense));
      };

    mkHsPkgs = pkgs:
      pkgs.haskellPackages.override {
        overrides = final: prev: {
          ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
          # Disable tests by default - tests are run explicitly via the checks
          aihc-parser = pkgs.haskell.lib.dontCheck (
            pkgs.haskell.lib.disableExecutableProfiling (
              pkgs.haskell.lib.disableLibraryProfiling (final.callCabal2nix "aihc-parser" (parserSrc pkgs) {})
            )
          );
          aihc-cpp = pkgs.haskell.lib.dontCheck (final.callCabal2nix "aihc-cpp" (cppSrc pkgs) {});
        };
      };
    mkHsPkgsForChecks = pkgs:
      pkgs.haskellPackages.override {
        overrides = final: prev: {
          ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
          # Checks should compile quickly; keep profiling disabled and optimization off.
          aihc-parser = pkgs.haskell.lib.dontCheck (
            pkgs.haskell.lib.disableOptimization (
              pkgs.haskell.lib.disableExecutableProfiling (
                pkgs.haskell.lib.disableLibraryProfiling (final.callCabal2nix "aihc-parser" (parserSrc pkgs) {})
              )
            )
          );
          aihc-cpp = pkgs.haskell.lib.dontCheck (final.callCabal2nix "aihc-cpp" (cppSrc pkgs) {});
        };
      };
    # Haskell packages with tests enabled and CLI executables available via env vars
    mkHsPkgsWithTests = pkgs:
      pkgs.haskellPackages.override {
        overrides = final: prev: {
          ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
          aihc-parser =
            pkgs.haskell.lib.overrideCabal
            (pkgs.haskell.lib.disableExecutableProfiling (
              pkgs.haskell.lib.disableLibraryProfiling (final.callCabal2nix "aihc-parser" (parserSrc pkgs) {})
            ))
            (old: {
              preCheck =
                (old.preCheck or "")
                + ''
                  export AIHC_LEXER_EXE="$PWD/dist/build/aihc-lexer/aihc-lexer"
                  export AIHC_PARSER_EXE="$PWD/dist/build/aihc-parser/aihc-parser"
                '';
              # Hide passing tests so failures are visible in Nix's truncated output
              testFlags = (old.testFlags or []) ++ ["--hide-successes"];
            });
          aihc-cpp =
            pkgs.haskell.lib.overrideCabal
            (final.callCabal2nix "aihc-cpp" (cppSrc pkgs) {})
            (old: {
              # Hide passing tests so failures are visible in Nix's truncated output
              testFlags = (old.testFlags or []) ++ ["--hide-successes"];
            });
        };
      };
    mkHsPkgsWithTestsForChecks = pkgs:
      pkgs.haskellPackages.override {
        overrides = final: prev: {
          ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
          aihc-parser =
            pkgs.haskell.lib.overrideCabal
            (pkgs.haskell.lib.disableOptimization (
              pkgs.haskell.lib.disableExecutableProfiling (
                pkgs.haskell.lib.disableLibraryProfiling (final.callCabal2nix "aihc-parser" (parserSrc pkgs) {})
              )
            ))
            (old: {
              preCheck =
                (old.preCheck or "")
                + ''
                  export AIHC_LEXER_EXE="$PWD/dist/build/aihc-lexer/aihc-lexer"
                  export AIHC_PARSER_EXE="$PWD/dist/build/aihc-parser/aihc-parser"
                '';
              # Hide passing tests so failures are visible in Nix's truncated output
              testFlags = (old.testFlags or []) ++ ["--hide-successes"];
            });
          aihc-cpp =
            pkgs.haskell.lib.overrideCabal
            (final.callCabal2nix "aihc-cpp" (cppSrc pkgs) {})
            (old: {
              # Hide passing tests so failures are visible in Nix's truncated output
              testFlags = (old.testFlags or []) ++ ["--hide-successes"];
            });
        };
      };
    # Haskell packages with Haddock enabled for documentation generation
    mkHsPkgsWithHaddock = pkgs:
      pkgs.haskellPackages.override {
        overrides = final: prev: {
          ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
          aihc-parser = pkgs.haskell.lib.dontCheck (
            pkgs.haskell.lib.doHaddock (
              pkgs.haskell.lib.disableExecutableProfiling (
                pkgs.haskell.lib.disableLibraryProfiling (final.callCabal2nix "aihc-parser" (parserSrc pkgs) {})
              )
            )
          );
          aihc-cpp = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doHaddock (final.callCabal2nix "aihc-cpp" (cppSrc pkgs) {}));
        };
      };
    mkHsPkgsWithHaddockForChecks = pkgs:
      pkgs.haskellPackages.override {
        overrides = final: prev: {
          ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
          aihc-parser = pkgs.haskell.lib.dontCheck (
            pkgs.haskell.lib.doHaddock (
              pkgs.haskell.lib.disableOptimization (
                pkgs.haskell.lib.disableExecutableProfiling (
                  pkgs.haskell.lib.disableLibraryProfiling (final.callCabal2nix "aihc-parser" (parserSrc pkgs) {})
                )
              )
            )
          );
          aihc-cpp = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doHaddock (final.callCabal2nix "aihc-cpp" (cppSrc pkgs) {}));
        };
      };
    # Combined Haddock documentation derivation
    mkCombinedDocs = pkgs: let
      hsPkgsHaddock = mkHsPkgsWithHaddock pkgs;
      parserDoc = hsPkgsHaddock.aihc-parser.doc;
      cppDoc = hsPkgsHaddock.aihc-cpp.doc;
      # Use haddock from ghc package (bundled with GHC)
      haddock = pkgs.haskellPackages.ghc;
    in
      pkgs.runCommand "aihc-docs" {
        nativeBuildInputs = [haddock];
        inherit parserDoc cppDoc;
      } ''
        mkdir -p "$out"

        # Copy individual package docs (discover html directory dynamically)
        parserHtml=$(find "$parserDoc/share/doc" -type d -name html | head -1)
        cppHtml=$(find "$cppDoc/share/doc" -type d -name html | head -1)
        cp -r "$parserHtml" "$out/aihc-parser"
        cp -r "$cppHtml" "$out/aihc-cpp"

        # Generate combined index and contents
        haddock \
          --gen-index \
          --gen-contents \
          -o "$out" \
          --read-interface=aihc-parser,"$out/aihc-parser/aihc-parser.haddock" \
          --read-interface=aihc-cpp,"$out/aihc-cpp/aihc-cpp.haddock"
      '';
    mkCombinedDocsForChecks = pkgs: let
      hsPkgsHaddock = mkHsPkgsWithHaddockForChecks pkgs;
      parserDoc = hsPkgsHaddock.aihc-parser.doc;
      cppDoc = hsPkgsHaddock.aihc-cpp.doc;
      # Use haddock from ghc package (bundled with GHC)
      haddock = pkgs.haskellPackages.ghc;
    in
      pkgs.runCommand "aihc-docs" {
        nativeBuildInputs = [haddock];
        inherit parserDoc cppDoc;
      } ''
        mkdir -p "$out"

        # Copy individual package docs (discover html directory dynamically)
        parserHtml=$(find "$parserDoc/share/doc" -type d -name html | head -1)
        cppHtml=$(find "$cppDoc/share/doc" -type d -name html | head -1)
        cp -r "$parserHtml" "$out/aihc-parser"
        cp -r "$cppHtml" "$out/aihc-cpp"

        # Generate combined index and contents
        haddock \
          --gen-index \
          --gen-contents \
          -o "$out" \
          --read-interface=aihc-parser,"$out/aihc-parser/aihc-parser.haddock" \
          --read-interface=aihc-cpp,"$out/aihc-cpp/aihc-cpp.haddock"
      '';
    # Haskell packages with HPC coverage enabled and coverage data exported
    mkHsPkgsWithCoverage = pkgs: let
      # Enable coverage and export HPC artifacts
      enableCoverageWithExport = drv:
        pkgs.haskell.lib.overrideCabal drv (old: {
          configureFlags = (old.configureFlags or []) ++ ["--enable-coverage"];
          # Hide passing tests so failures are visible in Nix's truncated output
          testFlags = (old.testFlags or []) ++ ["--hide-successes"];
          postInstall =
            (old.postInstall or "")
            + ''
              # Export HPC coverage data
              if [ -d dist/hpc ]; then
                mkdir -p "$out/hpc"
                cp -r dist/hpc/* "$out/hpc/"
              fi
            '';
        });
      # Set up CLI executable environment for tests (same as mkHsPkgsWithTests) + export HPC
      enableCoverageWithTests = drv:
        pkgs.haskell.lib.overrideCabal drv (old: {
          configureFlags = (old.configureFlags or []) ++ ["--enable-coverage"];
          # Hide passing tests so failures are visible in Nix's truncated output
          testFlags = (old.testFlags or []) ++ ["--hide-successes"];
          preCheck =
            (old.preCheck or "")
            + ''
                              export AIHC_LEXER_EXE="$PWD/dist/build/aihc-lexer/aihc-lexer"
                              export AIHC_PARSER_EXE="$PWD/dist/build/aihc-parser/aihc-parser"
                              # Create wrapper scripts that disable HPC for spawned executables
                              # This prevents .tix file corruption when test-spawned executables
                              # try to write to the same coverage file as the test runner
                              # Each invocation creates its own unique temp .tix file to avoid
                              # race conditions when tests run in parallel
                              mkdir -p "$PWD/hpc-wrappers"
                              for exe in aihc-lexer aihc-parser; do
                                cat > "$PWD/hpc-wrappers/$exe" << 'WRAPPER'
              #!/bin/sh
              # Create a unique temp .tix file for this invocation
              tix_file=$(mktemp --suffix=.tix)
              echo "Tix []" > "$tix_file"
              HPCTIXFILE="$tix_file" exec "$0.real" "$@"
              WRAPPER
                                chmod +x "$PWD/hpc-wrappers/$exe"
                                # Create symlink to actual executable
                                ln -sf "$PWD/dist/build/$exe/$exe" "$PWD/hpc-wrappers/$exe.real"
                              done
                              export AIHC_LEXER_EXE="$PWD/hpc-wrappers/aihc-lexer"
                              export AIHC_PARSER_EXE="$PWD/hpc-wrappers/aihc-parser"
            '';
          postInstall =
            (old.postInstall or "")
            + ''
              # Export HPC coverage data
              if [ -d dist/hpc ]; then
                mkdir -p "$out/hpc"
                cp -r dist/hpc/* "$out/hpc/"
              fi
            '';
        });
    in
      pkgs.haskellPackages.override {
        overrides = final: prev: {
          ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
          # Parser needs test setup for CLI executables
          aihc-parser = enableCoverageWithTests (
            pkgs.haskell.lib.disableExecutableProfiling (
              pkgs.haskell.lib.disableLibraryProfiling (final.callCabal2nix "aihc-parser" (parserSrc pkgs) {})
            )
          );
          # CPP doesn't need the CLI setup
          aihc-cpp = enableCoverageWithExport (final.callCabal2nix "aihc-cpp" (cppSrc pkgs) {});
        };
      };
    # Combined coverage report derivation
    mkCoverageReport = pkgs: let
      hsPkgsCoverage = mkHsPkgsWithCoverage pkgs;
      parserWithCoverage = pkgs.haskell.lib.doCheck hsPkgsCoverage.aihc-parser;
      cppWithCoverage = pkgs.haskell.lib.doCheck hsPkgsCoverage.aihc-cpp;
    in
      pkgs.runCommand "aihc-coverage" {
        nativeBuildInputs = [pkgs.haskellPackages.ghc pkgs.gnused pkgs.gnugrep];
        inherit parserWithCoverage cppWithCoverage;
      } ''
        mkdir -p "$out"

        echo "=== Collecting HPC Coverage Data ==="

        # Function to extract expression coverage percentage from HPC HTML
        # and generate a shields.io compatible JSON badge file
        extract_coverage_badge() {
          local html_file="$1"
          local output_json="$2"
          local package_name="$3"

          if [ -f "$html_file" ]; then
            # Extract the "Program Coverage Total" row and get expression coverage (last percentage)
            # The totals are on the line following "Program Coverage Total"
            local expr_pct
            expr_pct=$(grep -A1 'Program Coverage Total' "$html_file" | \
                       tail -1 | \
                       grep -oE '[0-9]+%</td>' | \
                       tail -1 | \
                       sed 's/%<\/td>//' || true)

            if [ -n "$expr_pct" ]; then
              # Determine badge color based on coverage percentage
              local color
              if [ "$expr_pct" -ge 80 ]; then
                color="brightgreen"
              elif [ "$expr_pct" -ge 60 ]; then
                color="green"
              elif [ "$expr_pct" -ge 40 ]; then
                color="yellow"
              elif [ "$expr_pct" -ge 20 ]; then
                color="orange"
              else
                color="red"
              fi

              # Generate shields.io compatible JSON using printf
              printf '{\n  "schemaVersion": 1,\n  "label": "%s coverage",\n  "message": "%s%%",\n  "color": "%s"\n}\n' \
                "$package_name" "$expr_pct" "$color" > "$output_json"
              echo "Generated badge for $package_name: $expr_pct% ($color)"
            else
              echo "Could not extract coverage percentage for $package_name"
            fi
          else
            echo "HTML file not found: $html_file"
          fi
        }

        # Copy parser coverage data
        if [ -d "$parserWithCoverage/hpc" ]; then
          echo "Found parser coverage data"
          cp -r "$parserWithCoverage/hpc" "$out/aihc-parser-hpc"
          # Copy HTML report if it exists
          if [ -d "$parserWithCoverage/hpc/vanilla/html" ]; then
            cp -r "$parserWithCoverage/hpc/vanilla/html" "$out/aihc-parser-html"
            extract_coverage_badge \
              "$out/aihc-parser-html/hpc_index.html" \
              "$out/aihc-parser-badge.json" \
              "aihc-parser"
          fi
        else
          echo "No parser coverage data found at $parserWithCoverage/hpc"
          ls -la "$parserWithCoverage/" || true
        fi

        # Copy cpp coverage data
        if [ -d "$cppWithCoverage/hpc" ]; then
          echo "Found cpp coverage data"
          cp -r "$cppWithCoverage/hpc" "$out/aihc-cpp-hpc"
          if [ -d "$cppWithCoverage/hpc/vanilla/html" ]; then
            cp -r "$cppWithCoverage/hpc/vanilla/html" "$out/aihc-cpp-html"
            extract_coverage_badge \
              "$out/aihc-cpp-html/hpc_index.html" \
              "$out/aihc-cpp-badge.json" \
              "aihc-cpp"
          fi
        else
          echo "No cpp coverage data found at $cppWithCoverage/hpc"
          ls -la "$cppWithCoverage/" || true
        fi

        # Create a summary
        echo "Coverage report generated at: $out" > "$out/README.txt"
        echo "" >> "$out/README.txt"
        echo "Contents:" >> "$out/README.txt"
        ls -la "$out" >> "$out/README.txt"
      '';
  in {
    packages = forAllSystems (pkgs: {
      parser-wasm-wasi = let
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
              aihc-parser:exe:aihc-lexer \
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
                          cat > cabal.project.offline <<'EOF'
            import: cabal.project

            packages:
              ./.wasm-offline-deps/*.tar.gz
            EOF
                          cp scripts/nix/cabal.project.freeze cabal.project.offline.freeze

                          wasm32-wasi-cabal build \
                            --offline \
                            aihc-parser:exe:aihc-parser \
                            aihc-parser:exe:aihc-lexer \
                            --project-file=cabal.project.offline
                          runHook postBuild
          '';
          installPhase = ''
            runHook preInstall
            mkdir -p "$out/bin"
            find dist-newstyle -type f -name 'aihc-parser.wasm' -exec cp {} "$out/bin/aihc-parser.wasm" \;
            find dist-newstyle -type f -name 'aihc-lexer.wasm' -exec cp {} "$out/bin/aihc-lexer.wasm" \;
            test -f "$out/bin/aihc-parser.wasm"
            test -f "$out/bin/aihc-lexer.wasm"
            runHook postInstall
          '';
          meta.description = "WASM-WASI builds of aihc-parser and aihc-lexer using ghc-wasm-meta";
        };
      docs = mkCombinedDocs pkgs;
      coverage = mkCoverageReport pkgs;
      default = mkCombinedDocs pkgs;
    });

    formatter = forAllSystems (pkgs: pkgs.alejandra);

    apps = forAllSystems (pkgs: let
      hsPkgs = mkHsPkgs pkgs;
      parserQuickcheckBatchExe = pkgs.lib.getExe' hsPkgs.aihc-parser "parser-quickcheck-batch";
      parserProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "parser-progress";
      lexerProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "lexer-progress";
      extensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "extension-progress";
      parserFuzzExe = pkgs.lib.getExe' hsPkgs.aihc-parser "parser-fuzz";
      cppProgressExe = pkgs.lib.getExe' hsPkgs.aihc-cpp "cpp-progress";
      hackageTesterExe = pkgs.lib.getExe' hsPkgs.aihc-parser "hackage-tester";
      stackageProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "stackage-progress";
      aihcLexerExe = pkgs.lib.getExe' hsPkgs.aihc-parser "aihc-lexer";
      aihcParserExe = pkgs.lib.getExe' hsPkgs.aihc-parser "aihc-parser";
      mkAppWithInputs = name: runtimeInputs: text: {
        type = "app";
        program = "${pkgs.writeShellApplication {
          inherit name;
          inherit runtimeInputs;
          inherit text;
        }}/bin/${name}";
        meta.description = "aihc app: ${name}";
      };
      mkApp = name: text: mkAppWithInputs name [pkgs.bash pkgs.cabal-install pkgs.ghc] text;
      mkFmtApp = name: text: mkAppWithInputs name [pkgs.bash pkgs.git pkgs.findutils pkgs.alejandra pkgs.haskellPackages.ormolu] text;
      mkReportsApp = name: text: {
        type = "app";
        program = "${pkgs.writeShellApplication {
          inherit name;
          runtimeInputs = [pkgs.bash pkgs.nix];
          inherit text;
        }}/bin/${name}";
        meta.description = "aihc app: ${name}";
      };
    in {
      parser-quickcheck-batch = mkAppWithInputs "parser-quickcheck-batch" [pkgs.bash pkgs.git] ''
        set -euo pipefail

        repo_root="$(git rev-parse --show-toplevel 2>/dev/null)" || {
          echo "Run this app from inside the repository." >&2
          exit 1
        }
        test -d "$repo_root/components/aihc-parser" || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd "$repo_root"
        AIHC_COMMIT_SHA="$(git rev-parse HEAD 2>/dev/null || printf '%s' unknown)"
        export AIHC_COMMIT_SHA
        exec ${parserQuickcheckBatchExe} "$@"
      '';

      parser-quickcheck-soak = mkAppWithInputs "parser-quickcheck-soak" [pkgs.bash pkgs.git pkgs.nix pkgs.jq] ''
        set -euo pipefail
        test -f scripts/parser-quickcheck-soak.sh || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        exec bash ./scripts/parser-quickcheck-soak.sh "$@"
      '';

      fmt = mkFmtApp "fmt" ''
        set -euo pipefail

        repo_root="$(git rev-parse --show-toplevel 2>/dev/null)" || {
          echo "Run this app from inside the repository." >&2
          exit 1
        }
        cd "$repo_root"

        git ls-files -z -- '*.nix' | xargs -0 -r alejandra
        git ls-files -z -- '*.hs' | grep -vz '/Fixtures/' | xargs -0 -r ormolu -m inplace
      '';

      line-counts = mkAppWithInputs "line-counts" [pkgs.tokei pkgs.jq pkgs.bash] ''
        set -euo pipefail

        # Output header
        printf "| %-30s | %10s | %10s | %10s |\n" "Component" "Code" "Tests" "Total"
        printf "| :%-30s | %10s: | %10s: | %10s: |\n" "------------------------------" "----------" "----------" "----------"

        total_code=0
        total_tests=0

        for comp_path in components/*; do
          [ -d "$comp_path" ] || continue
          comp=$(basename "$comp_path")

          # Skip aihc-name-resolution (empty stub)
          [ "$comp" = "aihc-name-resolution" ] && continue

          comp_all_lines=$(tokei "$comp_path" --output json | jq '.Total.code // 0')
          test_lines=0
          if [ -d "$comp_path/test" ]; then
            test_lines=$(tokei "$comp_path/test" --output json | jq '.Total.code // 0')
          fi
          # Apps are testing tools (fuzz, progress reports, etc.), count as test code
          if [ -d "$comp_path/app" ]; then
            app_lines=$(tokei "$comp_path/app" --output json | jq '.Total.code // 0')
            test_lines=$((test_lines + app_lines))
          fi
          # Common contains shared test infrastructure (golden, quickcheck, oracle, etc.)
          if [ -d "$comp_path/common" ]; then
            common_lines=$(tokei "$comp_path/common" --output json | jq '.Total.code // 0')
            test_lines=$((test_lines + common_lines))
          fi
          code_lines=$((comp_all_lines - test_lines))
          if [ $code_lines -lt 0 ]; then code_lines=0; fi

          comp_total=$comp_all_lines
          printf "| %-30s | %10d | %10d | %10d |\n" "$comp" "$code_lines" "$test_lines" "$comp_total"

          total_code=$((total_code + code_lines))
          total_tests=$((total_tests + test_lines))
        done

        total_all=$((total_code + total_tests))
        printf "| %-30s | %10d | %10d | %10d |\n" "**Total**" "$total_code" "$total_tests" "$total_all"
      '';

      parser-test = mkApp "parser-test" ''
        set -euo pipefail
        test -d components/aihc-parser || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-parser
        cabal test --test-show-details=direct
      '';

      parser-progress = mkApp "parser-progress" ''
        set -euo pipefail
        test -d components/aihc-parser || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-parser
        ${parserProgressExe}
      '';

      lexer-progress = mkApp "lexer-progress" ''
        set -euo pipefail
        test -d components/aihc-parser || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-parser
        ${lexerProgressExe}
      '';

      parser-extension-progress = mkApp "parser-extension-progress" ''
        set -euo pipefail
        test -d components/aihc-parser || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-parser
        ${extensionProgressExe} "$@"
      '';

      parser-fuzz = mkApp "parser-fuzz" ''
        set -euo pipefail
        ${parserFuzzExe} "$@"
      '';

      hackage-tester = mkApp "hackage-tester" ''
        set -euo pipefail
        ${hackageTesterExe} "$@"
      '';

      stackage-progress = mkApp "stackage-progress" ''
        set -euo pipefail
        ${stackageProgressExe} "$@"
      '';

      prompt = mkApp "prompt" ''
        set -euo pipefail
        ${stackageProgressExe} --prompt "$@"
      '';

      parser-progress-strict = mkApp "parser-progress-strict" ''
        set -euo pipefail
        test -d components/aihc-parser || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-parser
        ${parserProgressExe} --strict
      '';

      lexer-progress-strict = mkApp "lexer-progress-strict" ''
        set -euo pipefail
        test -d components/aihc-parser || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-parser
        ${lexerProgressExe} --strict
      '';

      parser-extension-progress-strict = mkApp "parser-extension-progress-strict" ''
        set -euo pipefail
        test -d components/aihc-parser || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-parser
        ${extensionProgressExe} --strict "$@"
      '';

      cpp-test = mkApp "cpp-test" ''
        set -euo pipefail
        test -d components/aihc-cpp || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-cpp
        cabal test --test-show-details=direct
      '';

      cpp-progress = mkApp "cpp-progress" ''
        set -euo pipefail
        test -d components/aihc-cpp || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-cpp
        ${cppProgressExe} "$@"
      '';

      cpp-progress-strict = mkApp "cpp-progress-strict" ''
        set -euo pipefail
        test -d components/aihc-cpp || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-cpp
        ${cppProgressExe} --strict "$@"
      '';

      generate-reports = mkReportsApp "generate-reports" ''
        set -euo pipefail
        test -d components/aihc-parser || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        bash ./scripts/update-generated-content.sh --update
      '';

      check-reports = mkReportsApp "check-reports" ''
        set -euo pipefail
        test -d components/aihc-parser || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        bash ./scripts/update-generated-content.sh --check
      '';

      aihc-lexer = mkAppWithInputs "aihc-lexer" [pkgs.bash] ''
        exec ${aihcLexerExe} "$@"
      '';

      aihc-parser = mkAppWithInputs "aihc-parser" [pkgs.bash] ''
        exec ${aihcParserExe} "$@"
      '';

      # Coverage app: builds and displays the coverage report
      coverage = let
        coverageReport = mkCoverageReport pkgs;
      in
        mkAppWithInputs "coverage" [
          pkgs.bash
        ] ''
          set -euo pipefail

          echo "=== HPC Coverage Report ==="
          echo ""
          echo "Coverage report location: ${coverageReport}"
          echo ""

          # Show contents
          echo "Report contents:"
          ls -la "${coverageReport}/"
          echo ""

          # Show README
          if [ -f "${coverageReport}/README.txt" ]; then
            cat "${coverageReport}/README.txt"
          fi

          echo ""
          echo "To view HTML reports, open:"
          if [ -d "${coverageReport}/aihc-parser-html" ]; then
            echo "  ${coverageReport}/aihc-parser-html/hpc_index.html"
          fi
          if [ -d "${coverageReport}/aihc-cpp-html" ]; then
            echo "  ${coverageReport}/aihc-cpp-html/hpc_index.html"
          fi
        '';

      default = mkApp "default" ''
        set -euo pipefail
        test -d components/aihc-parser || {
          echo "Run this app from the repository root." >&2
          exit 1
        }
        cd components/aihc-parser
        cabal test --test-show-details=direct
      '';
    });

    checks = forAllSystems (pkgs: let
      hsPkgs = mkHsPkgsForChecks pkgs;
      hsPkgsWithTests = mkHsPkgsWithTestsForChecks pkgs;
      # Parser tests with CLI executables available from the local build
      parserTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgsWithTests.aihc-parser);
      cppTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgsWithTests.aihc-cpp);
      nixLint =
        pkgs.runCommand "aihc-nix-lint" {
          src = nixSrc pkgs;
          nativeBuildInputs = [pkgs.statix];
        } ''
          cd "$src"
          statix check flake.nix
          touch "$out"
        '';
      nixFormat =
        pkgs.runCommand "aihc-nix-format" {
          src = nixSrc pkgs;
          nativeBuildInputs = [pkgs.alejandra];
        } ''
          cd "$src"
          alejandra --check .
          touch "$out"
        '';
      haskellLint =
        pkgs.runCommand "aihc-haskell-lint" {
          src = haskellSrc pkgs;
          nativeBuildInputs = [pkgs.haskellPackages.hlint pkgs.findutils];
        } ''
          cd "$src"
          find . -type f -name '*.hs' -print0 \
            | xargs -0 -r hlint
          touch "$out"
        '';
      haskellFormat =
        pkgs.runCommand "aihc-haskell-format" {
          src = haskellSrc pkgs;
          nativeBuildInputs = [pkgs.haskellPackages.ormolu pkgs.findutils];
        } ''
          cd "$src"
          find . -type f -name '*.hs' -print0 \
            | xargs -0 -r ormolu --mode check
          touch "$out"
        '';
      parserProgressStrict =
        pkgs.runCommand "aihc-parser-progress-strict" {
          src = parserSrc pkgs;
          nativeBuildInputs = [hsPkgs.aihc-parser];
        } ''
          cd "$src"
          parser-progress --strict
          touch "$out"
        '';
      lexerProgressStrict =
        pkgs.runCommand "aihc-lexer-progress-strict" {
          src = parserSrc pkgs;
          nativeBuildInputs = [hsPkgs.aihc-parser];
        } ''
          cd "$src"
          lexer-progress --strict
          touch "$out"
        '';
      parserExtensionProgressStrict =
        pkgs.runCommand "aihc-parser-extension-progress-strict" {
          src = parserSrc pkgs;
          nativeBuildInputs = [hsPkgs.aihc-parser];
        } ''
          cd "$src"
          extension-progress --strict
          touch "$out"
        '';
      parserQuickcheckSmoke =
        pkgs.runCommand "aihc-parser-quickcheck-smoke" {
          nativeBuildInputs = [hsPkgs.aihc-parser pkgs.jq];
        } ''
          first_json="$(mktemp)"
          second_json="$(mktemp)"
          parser-quickcheck-batch --max-success 200 --seed 123 --property "generated module AST pretty-printer round-trip" >"$first_json"
          parser-quickcheck-batch --max-success 200 --seed 123 --property "generated module AST pretty-printer round-trip" >"$second_json"
          jq -e '
            .selectedProperties == ["generated module AST pretty-printer round-trip"]
            and (.results | length == 1)
            and (.results[0].configuredMaxSuccess == 200)
            and (.results[0].status == "PASS")
          ' "$first_json" >/dev/null
          first_seed="$(jq -r '.results[0].seed' "$first_json")"
          second_seed="$(jq -r '.results[0].seed' "$second_json")"
          if [ "$first_seed" != "$second_seed" ]; then
            echo "expected deterministic property seeds" >&2
            exit 1
          fi
          touch "$out"
        '';
      cppProgressStrict =
        pkgs.runCommand "aihc-cpp-progress-strict" {
          src = cppSrc pkgs;
          nativeBuildInputs = [hsPkgs.aihc-cpp];
        } ''
          cd "$src"
          cpp-progress --strict
          touch "$out"
        '';
      parserQuickcheckSoakCheck =
        pkgs.runCommand "aihc-parser-quickcheck-soak-check" {
          src = scriptsSrc pkgs;
          nativeBuildInputs = [pkgs.bash pkgs.git pkgs.jq];
        } ''
          cd "$src"
          bash ./scripts/test-parser-quickcheck-soak.sh
          touch "$out"
        '';
      # Haddock documentation check - ensures docs build without errors
      haddockDocs = mkCombinedDocsForChecks pkgs;
      # Doctest for aihc-cpp documentation examples
      cppDoctest =
        pkgs.runCommand "aihc-cpp-doctest" {
          src = cppSrc pkgs;
          nativeBuildInputs = [
            pkgs.haskellPackages.doctest
            pkgs.haskellPackages.ghc
            hsPkgs.aihc-cpp
          ];
        } ''
          cd "$src"
          # Run doctest on the Aihc.Cpp module
          doctest -XGHC2021 -isrc src/Aihc/Cpp.hs
          touch "$out"
        '';
      # Doctest for aihc-parser documentation examples
      # Uses ghcWithPackages to create a GHC environment with all dependencies
      # and runs doctest with the correct package database
      parserDoctest = let
        # Create a GHC environment with all parser dependencies + doctest
        ghcEnv = hsPkgs.ghcWithPackages (p: [
          p.aihc-parser
          p.doctest
        ]);
      in
        pkgs.runCommand "aihc-parser-doctest" {
          src = parserSrc pkgs;
          nativeBuildInputs = [ghcEnv];
        } ''
          cd "$src"
          # Find the GHC package database from ghcWithPackages
          PKGDB=$(ghc --print-global-package-db)
          # Run doctest with explicit package database
          # Include all source files so imports between modules work
          doctest -XGHC2021 -package-db="$PKGDB" -isrc \
            src/Aihc/Parser/Shorthand.hs \
            src/Aihc/Parser.hs
          touch "$out"
        '';
    in {
      parser-tests = parserTests;
      cpp-tests = cppTests;
      cpp-doctest = cppDoctest;
      parser-doctest = parserDoctest;
      haddock-docs = haddockDocs;
      parser-progress-strict = parserProgressStrict;
      lexer-progress-strict = lexerProgressStrict;
      parser-extension-progress-strict = parserExtensionProgressStrict;
      parser-quickcheck-smoke = parserQuickcheckSmoke;
      parser-quickcheck-soak-check = parserQuickcheckSoakCheck;
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
          name = "cpp-tests";
          path = cppTests;
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
          name = "parser-quickcheck-smoke";
          path = parserQuickcheckSmoke;
        }
        {
          name = "parser-quickcheck-soak-check";
          path = parserQuickcheckSoakCheck;
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
    });

    # Development shells
    devShells = forAllSystems (pkgs: let
      hsPkgs = mkHsPkgs pkgs;
    in {
      default = pkgs.mkShell {
        buildInputs = [
          # GHC with all project dependencies
          (hsPkgs.ghcWithPackages (p: [
            p.aihc-parser
            p.aihc-cpp
          ]))
          pkgs.cabal-install
        ];
        shellHook = ''
          echo "aihc development shell"
          echo "  - GHC with project dependencies"
          echo "  - cabal-install"
        '';
      };
    });
  };
}
