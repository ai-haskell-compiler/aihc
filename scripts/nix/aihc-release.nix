# The precompiled aihc executable of a GitHub release.
#
# The release has executables for two systems only. For each other system,
# the value is null.
#
# To use a new release, change `version` and each `hash`. The hashes come
# from the SHA256SUMS file of the release. Convert each one with
# `nix hash convert --hash-algo sha256 --to sri HASH`.
pkgs: let
  version = "0.3.0";
  assets = {
    aarch64-darwin = {
      target = "apple-arm64";
      hash = "sha256-hTOsg4Ow8mEfI6LKlN1G+FU3Ymd13n5DlCEkh+A0b4w=";
    };
    x86_64-linux = {
      target = "linux-amd64";
      hash = "sha256-sWZJJlh6lt9npMmZUA3sq+k8SuHRStjG4cXXzswutIQ=";
    };
  };
  asset = assets.${pkgs.stdenv.hostPlatform.system} or null;
  name = "aihc-${version}-${asset.target}";
in
  if asset == null
  then null
  else
    pkgs.stdenvNoCC.mkDerivation {
      pname = "aihc-release";
      inherit version;
      src = pkgs.fetchurl {
        url = "https://github.com/ai-haskell-compiler/aihc/releases/download/v${version}/${name}.tar.gz";
        inherit (asset) hash;
      };
      # The Linux executable is static, and the macOS executable links only
      # system libraries. Thus the executable needs no change.
      dontConfigure = true;
      dontBuild = true;
      dontFixup = true;
      installPhase = ''
        runHook preInstall
        install -Dm755 aihc "$out/bin/aihc"
        install -Dm644 LICENSE "$out/share/doc/aihc/LICENSE"
        runHook postInstall
      '';
      doInstallCheck = true;
      installCheckPhase = ''
        "$out/bin/aihc" --help > /dev/null
      '';
      meta = {
        description = "Precompiled aihc ${version} from the GitHub release";
        homepage = "https://github.com/ai-haskell-compiler/aihc";
        license = pkgs.lib.licenses.unlicense;
        platforms = builtins.attrNames assets;
        mainProgram = "aihc";
      };
    }
