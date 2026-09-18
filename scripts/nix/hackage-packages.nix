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
#   revision Optional. The Hackage cabal file revision to build with, as
#            `{ number = N; hash = "..."; }`, where the hash is the SRI hash
#            of https://hackage.haskell.org/package/NAME-VERSION/revision/N.cabal
#            as printed by `nix hash file --sri N.cabal`. A revision relaxes
#            the version bounds of a release after the fact; the tarball
#            carries the original file, which the dependency solver would
#            reject when the bounds exclude the packages the release is
#            built against.
#   lint     Optional. Pass `--lint` to `aihc install`. Defaults to true.
#   targets  Optional list of targets. Defaults to the host targets that the
#            example tests use.
#   dependencies
#            Optional list of Hackage packages that the package depends on,
#            each with `name`, `version`, and `hash`. The test puts them next
#            to the package source, so `aihc install` finds them without
#            network access.
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
      revision = {
        number = 2;
        hash = "sha256-zLz7SYAd4SjK+EBLy62ZzVXMsbZZB7tTkQ2j1h7Aw5s=";
      };
    }
    {
      name = "split";
      version = "0.2.5.1";
      hash = "sha256-cgX0dtppA/mKggnrVXQ/gpjCgnRlBQVEWFoaEPMRwrU=";
    }
    {
      name = "bytestring";
      version = "0.12.2.0";
      hash = "sha256-bBKEw1dWp24YUf+wrQYNqQ/eDqnM3m1ZtyFy6g1ZFq0=";
      dependencies = [
        {
          name = "deepseq";
          version = "1.5.2.0";
          hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
        }
      ];
    }
    {
      name = "base64-bytestring";
      version = "1.2.1.0";
      hash = "sha256-Oe2u9XbsjSFi10dsUqlZbjoz+Bl5jn+s3xTTCono/oE=";
      revision = {
        number = 1;
        hash = "sha256-RTBcz4kUxm04W1GHIUcse4yFjxmGlFN390+FweDUmAM=";
      };
      dependencies = [
        {
          name = "bytestring";
          version = "0.12.2.0";
          hash = "sha256-bBKEw1dWp24YUf+wrQYNqQ/eDqnM3m1ZtyFy6g1ZFq0=";
        }
        {
          name = "deepseq";
          version = "1.5.2.0";
          hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
        }
      ];
    }
    {
      name = "tagged";
      version = "0.8.10";
      hash = "sha256-PqVvvs5oh9qVXzUcUUK2kX18qnRbe2yquC2zw/+GZ7k=";
      dependencies = [
        {
          name = "deepseq";
          version = "1.5.2.0";
          hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
        }
      ];
    }
    {
      name = "colour";
      version = "2.3.7";
      hash = "sha256-NvAF43qCGqg+rmrmb39sStmvdvvjSYBz6U9rzdqdMVM=";
    }
    {
      name = "dlist";
      version = "1.0";
      hash = "sha256-D2gFpv68TRj7z5xCmhi/MWUQ7uyg4zxPgHLRrFGbDNI=";
      revision = {
        number = 2;
        hash = "sha256-hUcnWUxagWqz0Q8VsbxP7a+eP30e9Reiu5AR8puiYdI=";
      };
      dependencies = [
        {
          name = "deepseq";
          version = "1.5.2.0";
          hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
        }
      ];
    }
    {
      name = "pretty";
      version = "1.1.3.6";
      hash = "sha256-5UIpOfD+0vxVCsb+3Efe8w91dIqFpLcaGsZ+RZm4Wx4=";
      dependencies = [
        {
          name = "deepseq";
          version = "1.5.2.0";
          hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
        }
      ];
    }
    {
      name = "containers";
      version = "0.8";
      hash = "sha256-zCP93Ma5w+FZO/OyqHgISrQXzHKipfxzeTAQ6ST1tRo=";
      dependencies = [
        {
          name = "deepseq";
          version = "1.5.2.0";
          hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
        }
        {
          name = "array";
          version = "0.5.8.0";
          hash = "sha256-YGP+ZsyP6onvdd7QbEGQJLPFH2kSubQnVfO/YgpjcwY=";
          revision = {
            number = 2;
            hash = "sha256-zLz7SYAd4SjK+EBLy62ZzVXMsbZZB7tTkQ2j1h7Aw5s=";
          };
        }
      ];
    }
  ];

  # The unpacked release, with its cabal file replaced by the pinned
  # revision when the entry names one, as cabal-install does when it unpacks.
  fetchPackage = pkgs: {
    name,
    version,
    hash,
    revision ? null,
    ...
  }: let
    source = pkgs.fetchzip {
      url = "https://hackage.haskell.org/package/${name}-${version}/${name}-${version}.tar.gz";
      inherit hash;
    };
  in
    if revision == null
    then source
    else
      pkgs.runCommand "${name}-${version}-r${toString revision.number}" {} ''
        cp -R --no-preserve=mode ${source} "$out"
        cp ${pkgs.fetchurl {
          url = "https://hackage.haskell.org/package/${name}-${version}/revision/${toString revision.number}.cabal";
          inherit (revision) hash;
        }} "$out/${name}.cabal"
      '';
in {
  inherit packages fetchPackage;
}
