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
               aihc-name-resolution =
                 final.callCabal2nix "aihc-name-resolution" ./components/aihc-name-resolution { };
             };
           };
     in {
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
          nameResolutionProgressExe =
            pkgs.lib.getExe' hsPkgs.aihc-name-resolution "name-resolution-progress";
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

          name-resolution-test = mkApp "name-resolution-test" ''
            set -euo pipefail
            test -d components/aihc-name-resolution || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/aihc-name-resolution
            cabal test --test-show-details=direct
          '';

          name-resolution-progress = mkApp "name-resolution-progress" ''
            set -euo pipefail
            test -d components/aihc-name-resolution || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/aihc-name-resolution
            ${nameResolutionProgressExe}
          '';

          name-resolution-progress-strict = mkApp "name-resolution-progress-strict" ''
            set -euo pipefail
            test -d components/aihc-name-resolution || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            cd components/aihc-name-resolution
            ${nameResolutionProgressExe} --strict
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
          nameResolutionTests =
            pkgs.haskell.lib.doCheck (pkgs.haskell.lib.dontHaddock hsPkgs.aihc-name-resolution);
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
          nameResolutionProgressStrict = pkgs.runCommand "aihc-name-resolution-progress-strict" {
            src = ./.;
            nativeBuildInputs = [ hsPkgs.aihc-name-resolution ];
          } ''
            cd "$src/components/aihc-name-resolution"
            name-resolution-progress --strict
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
        in {
          parser-tests = parserTests;
          cpp-tests = cppTests;
          name-resolution-tests = nameResolutionTests;
          parser-progress-strict = parserProgressStrict;
          lexer-progress-strict = lexerProgressStrict;
          parser-extension-progress-strict = parserExtensionProgressStrict;
          parser-quickcheck-smoke = parserQuickcheckSmoke;
          parser-quickcheck-soak-check = parserQuickcheckSoakCheck;
          cpp-progress-strict = cppProgressStrict;
          name-resolution-progress-strict = nameResolutionProgressStrict;
           nix-lint = nixLint;
           haskell-lint = haskellLint;
           haskell-format = haskellFormat;
           all-tests =
              pkgs.linkFarm "aihc-all-tests" [
                { name = "parser-tests"; path = parserTests; }
                { name = "cpp-tests"; path = cppTests; }
                { name = "name-resolution-tests"; path = nameResolutionTests; }
                { name = "parser-progress-strict"; path = parserProgressStrict; }
                { name = "lexer-progress-strict"; path = lexerProgressStrict; }
                { name = "parser-extension-progress-strict"; path = parserExtensionProgressStrict; }
                { name = "parser-quickcheck-smoke"; path = parserQuickcheckSmoke; }
                { name = "parser-quickcheck-soak-check"; path = parserQuickcheckSoakCheck; }
                { name = "cpp-progress-strict"; path = cppProgressStrict; }
                { name = "name-resolution-progress-strict"; path = nameResolutionProgressStrict; }
                 { name = "nix-lint"; path = nixLint; }
                { name = "haskell-lint"; path = haskellLint; }
                { name = "haskell-format"; path = haskellFormat; }
             ];
        });
    };
}
