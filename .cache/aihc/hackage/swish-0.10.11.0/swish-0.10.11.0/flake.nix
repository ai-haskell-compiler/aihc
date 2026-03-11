{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Swish: a semantic web toolkit for Haskell (experimantal)";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      # supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        swish = final.haskellPackages.callCabal2nix "swish" ./. {};
      });
      packages = forAllSystems (system: {
         swish = nixpkgsFor.${system}.swish;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.swish);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.swish];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            hlint
            cabal-install
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = ''
  echo -e "*** \e[1;32mWelcome to swish\e[0m ***"
  export PS1='swish:\A \e[1;34m\w\e[0m '
	'';
        });
  };
}
