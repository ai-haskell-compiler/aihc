{
  description = "aihc development flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

   outputs = { self, nixpkgs }:
      let
        systems = [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ];
        forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
        mkHsPkgs = pkgs:
          pkgs.haskellPackages.override {
             overrides = final: prev: {
               ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
               aihc-parser = final.callCabal2nix "aihc-parser" ./components/aihc-parser { };
               aihc-cpp = final.callCabal2nix "aihc-cpp" ./components/aihc-cpp { };
             };
           };
        # Haskell packages with Haddock enabled for documentation generation
        mkHsPkgsWithHaddock = pkgs:
          pkgs.haskellPackages.override {
            overrides = final: prev: {
              ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
              aihc-parser = pkgs.haskell.lib.doHaddock (final.callCabal2nix "aihc-parser" ./components/aihc-parser { });
              aihc-cpp = pkgs.haskell.lib.doHaddock (final.callCabal2nix "aihc-cpp" ./components/aihc-cpp { });
            };
          };
        # Combined Haddock documentation derivation
        mkCombinedDocs = pkgs:
          let
            hsPkgsHaddock = mkHsPkgsWithHaddock pkgs;
            parserDoc = hsPkgsHaddock.aihc-parser.doc;
            cppDoc = hsPkgsHaddock.aihc-cpp.doc;
            # Use haddock from ghc package (bundled with GHC)
            haddock = pkgs.haskellPackages.ghc;
          in pkgs.runCommand "aihc-docs" {
            nativeBuildInputs = [ haddock ];
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
        # Haskell packages with HIE file generation enabled for stan analysis
        mkHsPkgsWithHie = pkgs:
          let
            hsPkgs = mkHsPkgs pkgs;
            enableHie = drv: pkgs.haskell.lib.overrideCabal drv (old: {
              configureFlags = (old.configureFlags or []) ++ [
                "--ghc-option=-fwrite-ide-info"
                "--ghc-option=-hiedir=.hie"
              ];
              postInstall = (old.postInstall or "") + ''
                if [ -d .hie ]; then
                  mkdir -p "$out/.hie"
                  cp -r .hie/* "$out/.hie/"
                fi
              '';
            });
          in hsPkgs.override {
            overrides = final: prev: {
              ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
              aihc-parser = enableHie (final.callCabal2nix "aihc-parser" ./components/aihc-parser { });
              aihc-cpp = enableHie (final.callCabal2nix "aihc-cpp" ./components/aihc-cpp { });
            };
          };
     in {
      packages = forAllSystems (pkgs: {
        docs = mkCombinedDocs pkgs;
        default = mkCombinedDocs pkgs;
      });

      apps = forAllSystems (pkgs:
        let
          hsPkgs = mkHsPkgs pkgs;
          parserQuickcheckBatchExe = pkgs.lib.getExe' hsPkgs.aihc-parser "parser-quickcheck-batch";
          parserProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "parser-progress";
          lexerProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "lexer-progress";
          extensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "extension-progress";
          parserFuzzExe = pkgs.lib.getExe' hsPkgs.aihc-parser "parser-fuzz";
          cppProgressExe = pkgs.lib.getExe' hsPkgs.aihc-cpp "cpp-progress";
          hackageTesterExe = pkgs.lib.getExe' hsPkgs.aihc-parser "hackage-tester";
          stackageProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "stackage-progress";
          mkAppWithInputs = name: runtimeInputs: text: {
            type = "app";
            program = "${pkgs.writeShellApplication {
              inherit name;
              inherit runtimeInputs;
              inherit text;
            }}/bin/${name}";
            meta.description = "aihc app: ${name}";
          };
          mkApp = name: text: mkAppWithInputs name [ pkgs.bash pkgs.cabal-install pkgs.ghc ] text;
          mkReportsApp = name: text: {
            type = "app";
            program = "${pkgs.writeShellApplication {
              inherit name;
              runtimeInputs = [ pkgs.bash pkgs.nix ];
              inherit text;
            }}/bin/${name}";
            meta.description = "aihc app: ${name}";
          };
        in {
          parser-quickcheck-batch =
            mkAppWithInputs "parser-quickcheck-batch" [ pkgs.bash pkgs.git ] ''
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

          parser-quickcheck-soak =
            mkAppWithInputs "parser-quickcheck-soak" [ pkgs.bash pkgs.git pkgs.nix pkgs.jq ] ''
              set -euo pipefail
              test -f scripts/parser-quickcheck-soak.sh || {
                echo "Run this app from the repository root." >&2
                exit 1
              }
              exec bash ./scripts/parser-quickcheck-soak.sh "$@"
            '';

          line-counts = mkAppWithInputs "line-counts" [ pkgs.tokei pkgs.jq pkgs.bash ] ''
            set -euo pipefail

            # Output header
            printf "| %-30s | %10s | %10s | %10s |\n" "Component" "Code" "Tests" "Total"
            printf "| :%-30s | %10s: | %10s: | %10s: |\n" "------------------------------" "----------" "----------" "----------"

            total_code=0
            total_tests=0

            for comp_path in components/*; do
              [ -d "$comp_path" ] || continue
              comp=$(basename "$comp_path")

              comp_all_lines=$(tokei "$comp_path" --output json | jq '[.[] | .code] | add // 0')
              test_lines=0
              if [ -d "$comp_path/test" ]; then
                test_lines=$(tokei "$comp_path/test" --output json | jq '[.[] | .code] | add // 0')
              fi
              # Apps are testing tools (fuzz, progress reports, etc.), count as test code
              if [ -d "$comp_path/app" ]; then
                app_lines=$(tokei "$comp_path/app" --output json | jq '[.[] | .code] | add // 0')
                test_lines=$((test_lines + app_lines))
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

      checks = forAllSystems (pkgs:
        let
          hsPkgs = mkHsPkgs pkgs;
          parserTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgs.aihc-parser);
          cppTests = pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgs.aihc-cpp);
          nixLint = pkgs.runCommand "aihc-nix-lint" {
            src = ./.;
            nativeBuildInputs = [ pkgs.statix ];
          } ''
            cd "$src"
            statix check flake.nix
            touch "$out"
          '';
          haskellLint = pkgs.runCommand "aihc-haskell-lint" {
            src = ./.;
            nativeBuildInputs = [ pkgs.haskellPackages.hlint ];
          } ''
            cd "$src"
            find components -type f -name '*.hs' ! -path '*/test/Test/Fixtures/*' -print0 \
              | xargs -0 -r hlint
            touch "$out"
          '';
          haskellFormat = pkgs.runCommand "aihc-haskell-format" {
            src = ./.;
            nativeBuildInputs = [ pkgs.haskellPackages.ormolu ];
          } ''
            cd "$src"
            find components -type f -name '*.hs' ! -path '*/test/Test/Fixtures/*' -print0 \
              | xargs -0 -r ormolu --mode check
            touch "$out"
          '';
          parserProgressStrict = pkgs.runCommand "aihc-parser-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-parser ];
          } ''
            cd "$src/components/aihc-parser"
            parser-progress --strict
            touch "$out"
          '';
          lexerProgressStrict = pkgs.runCommand "aihc-lexer-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-parser ];
          } ''
            cd "$src/components/aihc-parser"
            lexer-progress --strict
            touch "$out"
          '';
          parserExtensionProgressStrict = pkgs.runCommand "aihc-parser-extension-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-parser ];
          } ''
            cd "$src/components/aihc-parser"
            extension-progress --strict
            touch "$out"
          '';
          parserQuickcheckSmoke = pkgs.runCommand "aihc-parser-quickcheck-smoke" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-parser pkgs.jq ];
          } ''
            cd "$src"
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
          cppProgressStrict = pkgs.runCommand "aihc-cpp-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-cpp ];
          } ''
            cd "$src/components/aihc-cpp"
            cpp-progress --strict
            touch "$out"
          '';
          parserQuickcheckSoakCheck = pkgs.runCommand "aihc-parser-quickcheck-soak-check" {
            src = ./.;
            nativeBuildInputs = [ pkgs.bash pkgs.git pkgs.jq ];
          } ''
            cd "$src"
            bash ./scripts/test-parser-quickcheck-soak.sh
            touch "$out"
          '';
          # Stan static analysis
          hsPkgsHie = mkHsPkgsWithHie pkgs;
          stanAnalysis = pkgs.runCommand "aihc-stan-analysis" {
            nativeBuildInputs = [ pkgs.haskellPackages.stan ];
            # Build packages with HIE files
            aihcParser = hsPkgsHie.aihc-parser;
            aihcCpp = hsPkgsHie.aihc-cpp;
          } ''
            mkdir -p "$out"
            # Collect all HIE files from built packages
            hie_dir="$(mktemp -d)"
            if [ -d "$aihcParser/.hie" ]; then
              cp -r "$aihcParser/.hie"/* "$hie_dir/" 2>/dev/null || true
            fi
            if [ -d "$aihcCpp/.hie" ]; then
              cp -r "$aihcCpp/.hie"/* "$hie_dir/" 2>/dev/null || true
            fi
            # Check if we have any HIE files
            hie_count=$(find "$hie_dir" -name '*.hie' 2>/dev/null | wc -l)
            if [ "$hie_count" -eq 0 ]; then
              echo "No HIE files found, skipping stan analysis"
              touch "$out/skipped"
              exit 0
            fi
            echo "Found $hie_count HIE files"
            # Run stan on the HIE files
            # Stan returns non-zero if it finds issues, so we capture output
            # Using || true to make this advisory (non-blocking) initially
            # Remove '|| true' once the codebase is clean to enforce stan checks
            stan --hiedir="$hie_dir" 2>&1 | tee "$out/stan-output.txt" || true
            touch "$out/completed"
          '';
          # Haddock documentation check - ensures docs build without errors
          haddockDocs = mkCombinedDocs pkgs;
        in {
          parser-tests = parserTests;
          cpp-tests = cppTests;
          haddock-docs = haddockDocs;
          parser-progress-strict = parserProgressStrict;
          lexer-progress-strict = lexerProgressStrict;
          parser-extension-progress-strict = parserExtensionProgressStrict;
          parser-quickcheck-smoke = parserQuickcheckSmoke;
          parser-quickcheck-soak-check = parserQuickcheckSoakCheck;
          cpp-progress-strict = cppProgressStrict;
          stan-analysis = stanAnalysis;
           nix-lint = nixLint;
           haskell-lint = haskellLint;
           haskell-format = haskellFormat;
           all-tests =
              pkgs.linkFarm "aihc-all-tests" [
                { name = "parser-tests"; path = parserTests; }
                { name = "cpp-tests"; path = cppTests; }
                { name = "haddock-docs"; path = haddockDocs; }
                { name = "parser-progress-strict"; path = parserProgressStrict; }
                { name = "lexer-progress-strict"; path = lexerProgressStrict; }
                { name = "parser-extension-progress-strict"; path = parserExtensionProgressStrict; }
                { name = "parser-quickcheck-smoke"; path = parserQuickcheckSmoke; }
                { name = "parser-quickcheck-soak-check"; path = parserQuickcheckSoakCheck; }
                { name = "cpp-progress-strict"; path = cppProgressStrict; }
                { name = "stan-analysis"; path = stanAnalysis; }
                 { name = "nix-lint"; path = nixLint; }
                { name = "haskell-lint"; path = haskellLint; }
                { name = "haskell-format"; path = haskellFormat; }
             ];
        });
    };
}
