{
  toolHsPackages,
  mkHsPkgs,
}: pkgs: let
  hsPkgs = mkHsPkgs pkgs;
  toolHsPkgs = toolHsPackages pkgs;
in {
  default = pkgs.mkShell {
    buildInputs = [
      hsPkgs.ghc
      toolHsPkgs.cabal-install
      toolHsPkgs.ormolu
      toolHsPkgs.cabal-gild
      pkgs.alejandra
      toolHsPkgs.hlint
    ];
  };
}
