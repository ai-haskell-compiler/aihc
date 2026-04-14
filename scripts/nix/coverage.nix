{
  projectHsPackages,
  mkHsPkgsWithCoverage,
}: {
  mkCoverageReport = pkgs: let
    hsPkgsCoverage = mkHsPkgsWithCoverage pkgs;
    parserWithCoverage = pkgs.haskell.lib.doCheck hsPkgsCoverage.aihc-parser;
    cppWithCoverage = pkgs.haskell.lib.doCheck hsPkgsCoverage.aihc-cpp;
  in
    pkgs.runCommand "aihc-coverage" {
      nativeBuildInputs = [(projectHsPackages pkgs).ghc pkgs.gnused pkgs.gnugrep];
      inherit parserWithCoverage cppWithCoverage;
    } ''
      mkdir -p "$out"

      echo "=== Collecting HPC Coverage Data ==="

      # Function to extract expression coverage percentage from HPC HTML
      # and generate a shields.io compatible JSON badge file.
      extract_coverage_badge() {
        local html_file="$1"
        local output_json="$2"
        local package_name="$3"

        if [ -f "$html_file" ]; then
          # Extract the "Program Coverage Total" row and get expression coverage (last percentage).
          # The totals are on the line following "Program Coverage Total".
          local expr_pct
          expr_pct=$(grep -A1 'Program Coverage Total' "$html_file" | \
                     tail -1 | \
                     grep -oE '[0-9]+%</td>' | \
                     tail -1 | \
                     sed 's/%<\/td>//' || true)

          if [ -n "$expr_pct" ]; then
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

      if [ -d "$parserWithCoverage/hpc" ]; then
        echo "Found parser coverage data"
        cp -r "$parserWithCoverage/hpc" "$out/aihc-parser-hpc"
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

      echo "Coverage report generated at: $out" > "$out/README.txt"
      echo "" >> "$out/README.txt"
      echo "Contents:" >> "$out/README.txt"
      ls -la "$out" >> "$out/README.txt"
    '';
}
