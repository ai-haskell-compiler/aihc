{nixpkgs}: let
  systems = [
    "x86_64-linux"
    "aarch64-linux"
    "x86_64-darwin"
    "aarch64-darwin"
  ];
in {
  inherit systems;

  forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs {inherit system;}));

  # GHC / Stackage package set for Nix-built Haskell derivations (matches nixpkgs haskell.packages.ghc9124).
  projectHsPackages = pkgs: pkgs.haskell.packages.ghc9124;
}
