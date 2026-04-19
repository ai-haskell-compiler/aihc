{
  projectHsPackages,
  sources,
}: let
  componentSpecs = {
    aihc-parser = {
      src = sources.parserSrc;
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = true;
      supportsCoverage = true;
    };
    aihc-parser-cli = {
      src = sources.parserCliSrc;
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-cpp = {
      src = sources.cppSrc;
      disableProfiling = false;
      optimizeForChecks = false;
      supportsDocs = true;
      supportsCoverage = true;
    };
    aihc-resolve = {
      src = sources.resolveSrc;
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
    aihc-tc = {
      src = sources.tcSrc;
      disableProfiling = true;
      optimizeForChecks = true;
      supportsDocs = false;
      supportsCoverage = false;
    };
  };

  enableCoverageWithExport = hsLib: drv:
    hsLib.overrideCabal drv (old: {
      configureFlags = (old.configureFlags or []) ++ ["--enable-coverage"];
      testFlags = (old.testFlags or []) ++ ["--hide-successes"];
      preCheck = (old.preCheck or "") + ''
        # GHC 9.12: test-suite mix files land in dist/build/<comp>/<comp>-tmp/extra-compilation-artifacts/
        # but hpc markup only searches dist/build/extra-compilation-artifacts/hpc/vanilla/mix/.
        # Copy all vanilla mix files from component build dirs into the searched location.
        target_mix=dist/build/extra-compilation-artifacts/hpc/vanilla/mix
        mkdir -p "$target_mix"
        while IFS= read -r mix_file; do
          rel="''${mix_file##*/extra-compilation-artifacts/hpc/vanilla/mix/}"
          pkg_dir="$target_mix/$(dirname "$rel")"
          mkdir -p "$pkg_dir"
          cp "$mix_file" "$pkg_dir/" 2>/dev/null || true
        done < <(find dist/build -path "*/extra-compilation-artifacts/hpc/vanilla/mix/*.mix" 2>/dev/null)
      '';
      postInstall =
        (old.postInstall or "")
        + ''
          # Export HPC coverage data.
          if [ -d dist/hpc ]; then
            mkdir -p "$out/hpc"
            cp -r dist/hpc/* "$out/hpc/"
          fi
        '';
    });

  applyHaddockMode = hsLib: mode: drv:
    if mode == "do"
    then hsLib.doHaddock drv
    else if mode == "dont"
    then hsLib.dontHaddock drv
    else drv;

  isOverridableHaskellDrv = pkgs: drv:
    pkgs.lib.isDerivation drv && drv ? overrideScope;

  disableUpstreamChecks = pkgs: hsLib: localPackageNames: _final: prev:
    builtins.mapAttrs (
      name: drv:
        if builtins.elem name localPackageNames || !(isOverridableHaskellDrv pkgs drv)
        then drv
        else hsLib.dontCheck drv
    )
    prev;
in rec {
  # Hackage dependencies whose build settings need manual adjustment.
  hackageDepTestFixes = pkgs: _final: prev: {
    network = pkgs.haskell.lib.dontCheck prev.network;
  };

  mkHsPkgsVariant = pkgs: {
    disableOptimization ? false,
    enableDocs ? false,
    enableCoverage ? false,
  }: let
    hsLib = pkgs.haskell.lib;
    localPackageNames = (builtins.attrNames componentSpecs) ++ ["aihc-hackage"];

    mkComponent = final: name: spec: let
      baseDrv = final.callCabal2nix name (spec.src pkgs) {};
      profilingAdjusted =
        if spec.disableProfiling
        then hsLib.disableExecutableProfiling (hsLib.disableLibraryProfiling baseDrv)
        else baseDrv;
      optimizationAdjusted =
        if disableOptimization && spec.optimizeForChecks
        then hsLib.disableOptimization profilingAdjusted
        else profilingAdjusted;
      coverageAdjusted =
        if enableCoverage && spec.supportsCoverage
        then enableCoverageWithExport hsLib optimizationAdjusted
        else optimizationAdjusted;
      checksAdjusted = hsLib.dontCheck coverageAdjusted;
      haddockMode =
        if enableDocs
        then
          if spec.supportsDocs
          then "do"
          else "dont"
        else "leave";
    in
      applyHaddockMode hsLib haddockMode checksAdjusted;
  in
    (projectHsPackages pkgs).override {
      overrides = final: prev:
        disableUpstreamChecks pkgs hsLib localPackageNames final prev
        // hackageDepTestFixes pkgs final prev
        // {
          ghc-lib-parser = pkgs.haskell.lib.dontHaddock final.ghc-lib-parser_9_14_1_20251220;
          aihc-hackage = pkgs.haskell.lib.dontCheck (
            pkgs.haskell.lib.dontHaddock (final.callCabal2nix "aihc-hackage" (sources.hackageSrc pkgs) {})
          );
        }
        // builtins.mapAttrs (mkComponent final) componentSpecs;
    };

  mkHsPkgs = pkgs: mkHsPkgsVariant pkgs {};

  mkHsPkgsForChecks = pkgs:
    mkHsPkgsVariant pkgs {
      disableOptimization = true;
    };

  mkHsPkgsWithHaddock = pkgs:
    mkHsPkgsVariant pkgs {
      enableDocs = true;
    };

  mkHsPkgsWithCoverage = pkgs:
    mkHsPkgsVariant pkgs {
      enableCoverage = true;
    };
}
