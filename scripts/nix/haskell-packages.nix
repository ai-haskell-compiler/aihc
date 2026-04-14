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

  addHiddenSuccesses = old: {
    # Hide passing tests so failures are visible in Nix's truncated output.
    testFlags = (old.testFlags or []) ++ ["--hide-successes"];
  };

  enableCoverageWithExport = hsLib: drv:
    hsLib.overrideCabal drv (old: {
      configureFlags = (old.configureFlags or []) ++ ["--enable-coverage"];
      testFlags = (old.testFlags or []) ++ ["--hide-successes"];
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

  applyCheckMode = hsLib: mode: drv:
    if mode == "disable"
    then hsLib.dontCheck drv
    else if mode == "hide-successes"
    then hsLib.overrideCabal drv addHiddenSuccesses
    else drv;
in rec {
  # Hackage dependencies whose test suites are unsuitable for the Nix build sandbox.
  hackageDepTestFixes = pkgs: _final: prev: {
    network = pkgs.haskell.lib.dontCheck prev.network;
  };

  mkHsPkgsVariant = pkgs: {
    disableOptimization ? false,
    checkMode ? "disable",
    enableDocs ? false,
    enableCoverage ? false,
  }: let
    hsLib = pkgs.haskell.lib;

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
      haddockMode =
        if enableDocs
        then
          if spec.supportsDocs
          then "do"
          else "dont"
        else "leave";
      effectiveCheckMode =
        if enableCoverage && spec.supportsCoverage
        then "default"
        else checkMode;
    in
      applyCheckMode hsLib effectiveCheckMode (applyHaddockMode hsLib haddockMode coverageAdjusted);
  in
    (projectHsPackages pkgs).override {
      overrides = final: prev:
        hackageDepTestFixes pkgs final prev
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

  mkHsPkgsWithTests = pkgs:
    mkHsPkgsVariant pkgs {
      checkMode = "hide-successes";
    };

  mkHsPkgsWithTestsForChecks = pkgs:
    mkHsPkgsVariant pkgs {
      disableOptimization = true;
      checkMode = "hide-successes";
    };

  mkHsPkgsWithHaddock = pkgs:
    mkHsPkgsVariant pkgs {
      enableDocs = true;
    };

  mkHsPkgsWithHaddockForChecks = pkgs:
    mkHsPkgsVariant pkgs {
      disableOptimization = true;
      enableDocs = true;
    };

  mkHsPkgsWithCoverage = pkgs:
    mkHsPkgsVariant pkgs {
      enableCoverage = true;
    };
}
