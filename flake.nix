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
    root = ./.;
    core = import ./scripts/nix/core.nix {inherit nixpkgs;};
    sources = import ./scripts/nix/sources.nix {inherit root;};
    haskell = import ./scripts/nix/haskell-packages.nix {
      inherit (core) projectHsPackages;
      inherit sources;
    };
    docs = import ./scripts/nix/docs.nix {
      inherit (core) projectHsPackages;
      inherit (haskell) mkHsPkgsWithHaddock mkHsPkgsWithHaddockForChecks;
    };
    coverage = import ./scripts/nix/coverage.nix {
      inherit (core) projectHsPackages;
      inherit (haskell) mkHsPkgsWithCoverage;
    };
    wasm = import ./scripts/nix/wasm.nix {
      inherit ghc-wasm-meta;
      inherit (sources) wasmBuildSrc;
    };
    mkPackages = import ./scripts/nix/packages.nix {
      inherit (docs) mkCombinedDocs;
      inherit (coverage) mkCoverageReport;
      inherit (wasm) mkParserWasmWasi;
    };
    mkApps = import ./scripts/nix/apps.nix {
      inherit (core) projectHsPackages;
      inherit (haskell) mkHsPkgs;
      inherit (coverage) mkCoverageReport;
    };
    mkChecks = import ./scripts/nix/checks.nix {
      inherit (core) projectHsPackages;
      inherit sources;
      inherit (haskell) mkHsPkgsForChecks mkHsPkgsWithTestsForChecks;
      inherit (docs) mkCombinedDocsForChecks;
    };
    mkDevShells = import ./scripts/nix/dev-shells.nix {
      inherit (haskell) mkHsPkgs;
    };
  in {
    packages = core.forAllSystems mkPackages;

    formatter = core.forAllSystems (pkgs: pkgs.alejandra);

    apps = core.forAllSystems mkApps;

    checks = core.forAllSystems mkChecks;

    devShells = core.forAllSystems mkDevShells;
  };
}
