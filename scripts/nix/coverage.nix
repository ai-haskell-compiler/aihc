{
  projectHsPackages,
  sources,
  mkHsPkgsWithCoverage,
}: {
  mkCoverageReport = pkgs: let
    hsPkgsCoverage = mkHsPkgsWithCoverage pkgs;
    parserWithCoverage = pkgs.haskell.lib.doCheck hsPkgsCoverage.aihc-parser;
    cppWithCoverage = pkgs.haskell.lib.doCheck hsPkgsCoverage.aihc-cpp;
    parserSrc = sources.parserSrc pkgs;
    cppSrc = sources.cppSrc pkgs;
  in
    pkgs.runCommand "aihc-coverage" {
      nativeBuildInputs = [(projectHsPackages pkgs).ghc pkgs.findutils pkgs.gnused pkgs.gnugrep];
      inherit parserWithCoverage cppWithCoverage parserSrc cppSrc;
    } ''
      mkdir -p "$out"

      echo "=== Collecting HPC Coverage Data ==="

      build_module_list() {
        local src_dir="$1"
        local output_file="$2"

        find "$src_dir" -type f -name '*.hs' ! -name '*.hs-boot' | sort | while IFS= read -r source_file; do
          local rel_path module_name
          rel_path="''${source_file#"$src_dir"/}"
          module_name="''${rel_path%.hs}"
          printf '%s\n' "''${module_name//\//.}"
        done > "$output_file"
      }

      render_hpc_html() {
        local package_name="$1"
        local hpc_root="$2"
        local html_dir="$3"
        local src_root="$4"
        local module_list="$5"

        local tix_file
        tix_file=$(find "$hpc_root" -type f -name '*.tix' | head -n1)
        if [ -z "$tix_file" ]; then
          echo "No .tix file found for $package_name under $hpc_root"
          return 1
        fi

        if [ ! -d "$hpc_root/vanilla/mix" ]; then
          echo "No mix directory found for $package_name under $hpc_root"
          return 1
        fi

        local stage_root
        stage_root=$(mktemp -d)
        cp -r "$src_root"/. "$stage_root/"
        mkdir -p "$stage_root/.hpc"
        cp -r "$hpc_root/vanilla/mix"/. "$stage_root/.hpc/"

        local -a module_args
        mapfile -t module_args < "$module_list"
        if [ "''${#module_args[@]}" -eq 0 ]; then
          echo "No modules found under $src_root/src for $package_name"
          return 1
        fi

        hpc markup "$tix_file" "--destdir=$html_dir" "--srcdir=$stage_root" "''${module_args[@]}"
      }

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
        parser_modules="$TMPDIR/aihc-parser-modules.txt"
        build_module_list "$parserSrc/src" "$parser_modules"
        if render_hpc_html "aihc-parser" "$out/aihc-parser-hpc" "$out/aihc-parser-html" "$parserSrc" "$parser_modules"; then
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
        cpp_modules="$TMPDIR/aihc-cpp-modules.txt"
        build_module_list "$cppSrc/src" "$cpp_modules"
        if render_hpc_html "aihc-cpp" "$out/aihc-cpp-hpc" "$out/aihc-cpp-html" "$cppSrc" "$cpp_modules"; then
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
