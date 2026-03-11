let
  pkgs = import <nixpkgs> {};
  latest-coin-utils = pkgs.callPackage ./nix/coin-utils.nix {};
  latest-osi =
    (pkgs.callPackage ./nix/osi.nix {}).override {
      coin-utils = latest-coin-utils;
    };
  latest-clp =
    (pkgs.callPackage ./nix/clp.nix {}).override {
      coin-utils = latest-coin-utils;
      osi = latest-osi;
    };
in
pkgs.mkShell {
  # "override" required dependencies
  buildInputs = (with pkgs; [
    latest-clp
    latest-coin-utils
    pkg-config
    gmp
    bzip2
    lapack
    blas
  ]);
  shellHook = ''
    # Why are these paths not set by Nix?
    export PKG_CONFIG_PATH=${latest-coin-utils}/lib/pkgconfig:$PKG_CONFIG_PATH
    export LD_LIBRARY_PATH=${latest-coin-utils}/lib:$LD_LIBRARY_PATH
    export PKG_CONFIG_PATH=${latest-osi}/lib/pkgconfig:$PKG_CONFIG_PATH
    export LD_LIBRARY_PATH=${latest-osi}/lib:$LD_LIBRARY_PATH
    export PKG_CONFIG_PATH=${latest-clp}/lib/pkgconfig:$PKG_CONFIG_PATH
    export LD_LIBRARY_PATH=${latest-clp}/lib:$LD_LIBRARY_PATH
  '';
}
