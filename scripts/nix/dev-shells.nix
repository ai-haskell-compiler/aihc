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
      pkgs.clang-tools
      (pkgs.writeShellScriptBin "wasm32-clang" ''
        exec ${pkgs.llvmPackages.clang-unwrapped}/bin/clang --target=wasm32-unknown-unknown "$@"
      '')
      (pkgs.writeShellScriptBin "wasm-ld" ''
        exec ${pkgs.lld}/bin/wasm-ld "$@"
      '')
      pkgs.wasm-tools
      pkgs.wasmtime
      pkgs.wit-bindgen
      pkgs.zlib
      pkgs.python3Packages.mkdocs-material
    ];
  };
}
