{mkHsPkgs}: pkgs: let
  hsPkgs = mkHsPkgs pkgs;
in {
  default = pkgs.mkShell {
    buildInputs = [
      hsPkgs.ghc
      pkgs.cabal-install
      pkgs.ormolu
      pkgs.haskellPackages.cabal-gild
      pkgs.alejandra
      pkgs.hlint
    ];
  };
}
