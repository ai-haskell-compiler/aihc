{ compiler ? "ghc966"
}:

(import ./release.nix { compiler = compiler; }).exe
