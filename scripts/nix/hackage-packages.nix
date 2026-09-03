# Hackage packages that `aihc install` must install in `nix flake check`.
#
# Add a package by appending an entry. The `hash` is the SRI hash of the
# unpacked tarball, as printed by
# `nix-prefetch-url --unpack https://hackage.haskell.org/package/NAME-VERSION/NAME-VERSION.tar.gz`
# followed by `nix hash convert --hash-algo sha256 --to sri HASH`.
#
# Each entry accepts:
#   name     Hackage package name.
#   version  Exact Hackage version.
#   hash     SRI hash of the unpacked tarball.
#   lint     Optional. Pass `--lint` to `aihc install`. Defaults to true.
#   targets  Optional list of targets. Defaults to the host targets that the
#            example tests use.
let
  packages = [
    {
      name = "deepseq";
      version = "1.5.2.0";
      hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
    }
    {
      name = "array";
      version = "0.5.8.0";
      hash = "sha256-YGP+ZsyP6onvdd7QbEGQJLPFH2kSubQnVfO/YgpjcwY=";
    }
  ];

  fetchPackage = pkgs: {
    name,
    version,
    hash,
    ...
  }:
    pkgs.fetchzip {
      url = "https://hackage.haskell.org/package/${name}-${version}/${name}-${version}.tar.gz";
      inherit hash;
    };
in {
  inherit packages fetchPackage;
}
