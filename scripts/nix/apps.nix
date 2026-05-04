{
  projectHsPackages,
  mkHsPkgs,
  mkCoverageReport,
}: pkgs: let
  hsPkgs = mkHsPkgs pkgs;
  parserProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "parser-progress";
  lexerProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "lexer-progress";
  extensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-parser "extension-progress";
  cppProgressExe = pkgs.lib.getExe' hsPkgs.aihc-cpp "cpp-progress";
  resolveProgressExe = pkgs.lib.getExe' hsPkgs.aihc-resolve "resolve-progress";
  resolveExtensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-resolve "resolve-extension-progress";
  tcProgressExe = pkgs.lib.getExe' hsPkgs.aihc-tc "tc-progress";
  tcExtensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-tc "tc-extension-progress";
  aihcDevExe = pkgs.lib.getExe' hsPkgs.aihc-dev "aihc-dev";

  repoRootGuard = ''
    test -d components/aihc-parser || {
      echo "Run this app from the repository root." >&2
      exit 1
    }
  '';

  mkAppWithInputs = name: runtimeInputs: text: {
    type = "app";
    program = "${pkgs.writeShellApplication {
      inherit name runtimeInputs text;
    }}/bin/${name}";
    meta.description = "aihc app: ${name}";
  };

  mkApp = name: text: mkAppWithInputs name [pkgs.bash pkgs.cabal-install hsPkgs.ghc] text;

  mkFmtApp = name: text:
    mkAppWithInputs name [pkgs.bash pkgs.git pkgs.findutils pkgs.alejandra (projectHsPackages pkgs).ormolu] text;

  mkReportsApp = name: text: {
    type = "app";
    program = "${pkgs.writeShellApplication {
      inherit name text;
      runtimeInputs = [pkgs.bash pkgs.nix];
    }}/bin/${name}";
    meta.description = "aihc app: ${name}";
  };

  mkComponentApp = name: component: text:
    mkApp name ''
      set -euo pipefail
      ${repoRootGuard}
      cd ${component}
      ${text}
    '';
in {
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

  line-counts = mkAppWithInputs "line-counts" [pkgs.tokei pkgs.jq pkgs.jtbl pkgs.bash] ''
    set -euo pipefail

    total_code=0
    total_tests=0

    {
      for comp_path in components/*; do
        [ -d "$comp_path" ] || continue
        comp=$(basename "$comp_path")

        # Skip aihc-name-resolution (empty stub).
        [ "$comp" = "aihc-name-resolution" ] && continue

        comp_all_lines=$(tokei "$comp_path" --output json | jq '.Total.code // 0')
        test_lines=0
        if [ -d "$comp_path/test" ]; then
          test_lines=$(tokei "$comp_path/test" --output json | jq '.Total.code // 0')
        fi
        # Apps are testing tools (fuzz, progress reports, etc.), count as test code.
        if [ -d "$comp_path/app" ]; then
          app_lines=$(tokei "$comp_path/app" --output json | jq '.Total.code // 0')
          test_lines=$((test_lines + app_lines))
        fi
        # Common contains shared test infrastructure (golden, quickcheck, oracle, etc.).
        if [ -d "$comp_path/common" ]; then
          common_lines=$(tokei "$comp_path/common" --output json | jq '.Total.code // 0')
          test_lines=$((test_lines + common_lines))
        fi
        code_lines=$((comp_all_lines - test_lines))
        if [ "$code_lines" -lt 0 ]; then code_lines=0; fi

        comp_total=$comp_all_lines
        jq -nc \
          --arg Component "$comp" \
          --argjson Code "$code_lines" \
          --argjson Tests "$test_lines" \
          --argjson Total "$comp_total" \
          '{Component: $Component, Code: $Code, Tests: $Tests, Total: $Total}'

        total_code=$((total_code + code_lines))
        total_tests=$((total_tests + test_lines))
      done

      total_all=$((total_code + total_tests))
      jq -nc \
        --argjson Code "$total_code" \
        --argjson Tests "$total_tests" \
        --argjson Total "$total_all" \
        '{Component: "**Total**", Code: $Code, Tests: $Tests, Total: $Total}'
    } | {
      printf '%s\n' '```'
      jtbl --markdown
      printf '%s\n' '```'
    }
  '';

  parser-test = mkComponentApp "parser-test" "components/aihc-parser" ''
    cabal test --test-show-details=direct
  '';

  parser-progress = mkComponentApp "parser-progress" "components/aihc-parser" ''
    ${parserProgressExe}
  '';

  lexer-progress = mkComponentApp "lexer-progress" "components/aihc-parser" ''
    ${lexerProgressExe}
  '';

  parser-extension-progress = mkComponentApp "parser-extension-progress" "components/aihc-parser" ''
    ${extensionProgressExe} "$@"
  '';

  aihc-dev = mkAppWithInputs "aihc-dev" [pkgs.bash hsPkgs.ghc] ''
    exec ${aihcDevExe} "$@"
  '';

  parser-progress-strict = mkComponentApp "parser-progress-strict" "components/aihc-parser" ''
    ${parserProgressExe} --strict
  '';

  lexer-progress-strict = mkComponentApp "lexer-progress-strict" "components/aihc-parser" ''
    ${lexerProgressExe} --strict
  '';

  parser-extension-progress-strict = mkComponentApp "parser-extension-progress-strict" "components/aihc-parser" ''
    ${extensionProgressExe} --strict "$@"
  '';

  cpp-test = mkComponentApp "cpp-test" "components/aihc-cpp" ''
    cabal test --test-show-details=direct
  '';

  cpp-progress = mkComponentApp "cpp-progress" "components/aihc-cpp" ''
    ${cppProgressExe} "$@"
  '';

  cpp-progress-strict = mkComponentApp "cpp-progress-strict" "components/aihc-cpp" ''
    ${cppProgressExe} --strict "$@"
  '';

  resolve-progress = mkComponentApp "resolve-progress" "components/aihc-resolve" ''
    ${resolveProgressExe} "$@"
  '';

  resolve-progress-strict = mkComponentApp "resolve-progress-strict" "components/aihc-resolve" ''
    ${resolveProgressExe} --strict "$@"
  '';

  resolve-extension-progress = mkComponentApp "resolve-extension-progress" "components/aihc-resolve" ''
    ${resolveExtensionProgressExe} "$@"
  '';

  tc-progress = mkComponentApp "tc-progress" "components/aihc-tc" ''
    ${tcProgressExe} "$@"
  '';

  tc-progress-strict = mkComponentApp "tc-progress-strict" "components/aihc-tc" ''
    ${tcProgressExe} --strict "$@"
  '';

  tc-test = mkComponentApp "tc-test" "components/aihc-tc" ''
    cabal test --test-show-details=direct
  '';

  tc-extension-progress = mkComponentApp "tc-extension-progress" "components/aihc-tc" ''
    ${tcExtensionProgressExe} "$@"
  '';

  generate-reports = mkReportsApp "generate-reports" ''
    set -euo pipefail
    ${repoRootGuard}
    bash ./scripts/update-generated-content.sh --update
  '';

  check-reports = mkReportsApp "check-reports" ''
    set -euo pipefail
    ${repoRootGuard}
    bash ./scripts/update-generated-content.sh --check
  '';

  coverage = let
    coverageReport = mkCoverageReport pkgs;
  in
    mkAppWithInputs "coverage" [pkgs.bash] ''
      set -euo pipefail

      echo "=== HPC Coverage Report ==="
      echo ""
      echo "Coverage report location: ${coverageReport}"
      echo ""

      echo "Report contents:"
      ls -la "${coverageReport}/"
      echo ""

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

  default = mkComponentApp "default" "components/aihc-parser" ''
    cabal test --test-show-details=direct
  '';
}
