{nixpkgs}: let
  systems = [
    "x86_64-linux"
    "aarch64-linux"
    "x86_64-darwin"
    "aarch64-darwin"
  ];
in rec {
  inherit systems;

  forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs {inherit system;}));

  # GHC / Stackage package set for Nix-built Haskell derivations.
  # Keep build-time Haskell dependencies in the same GHC 9.12 package set
  # instead of falling back to nixpkgs' default haskellPackages alias.
  toolHsPackages = pkgs: let
    hsPkgs = pkgs.haskell.packages.ghc9124.override {
      buildHaskellPackages = hsPkgs;
      overrides = _final: prev: {
        mkDerivation = args:
          prev.mkDerivation
          (args
            // {
              enableLibraryProfiling = false;
              enableExecutableProfiling = false;
            });
        # The upstream Unix socket test can exceed Darwin's socket path limit
        # under Nix's build directory.
        network = pkgs.haskell.lib.dontCheck prev.network;
      };
    };
  in
    hsPkgs;

  projectHsPackages = toolHsPackages;
}
