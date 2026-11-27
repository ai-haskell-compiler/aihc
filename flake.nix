{
  description = "aihc development flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    root = ./.;
    core = import ./scripts/nix/core.nix {inherit nixpkgs;};
    sources = import ./scripts/nix/sources.nix {inherit root;};
    haskell = import ./scripts/nix/haskell-packages.nix {
      inherit (core) projectHsPackages;
      inherit sources;
    };
    docs = import ./scripts/nix/docs.nix {};
    mkWasiSysroot = import ./scripts/nix/wasi-sysroot.nix;
    mkEditorGrammars = import ./scripts/nix/editor-grammars.nix;
    mkLirExtension = import ./scripts/nix/vscode-lir.nix;
    mkPackages = import ./scripts/nix/packages.nix {
      inherit mkEditorGrammars mkLirExtension;
    };
    mkApps = import ./scripts/nix/apps.nix {
      inherit (haskell) mkHsPkgs;
      inherit mkWasiSysroot;
    };
    mkChecks = import ./scripts/nix/checks.nix {
      inherit (core) projectHsPackages;
      inherit sources mkWasiSysroot mkEditorGrammars mkLirExtension;
      inherit (haskell) mkHsPkgsForChecks;
      inherit (docs) mkManual mkGuide;
    };
    mkAihcRelease = import ./scripts/nix/aihc-release.nix;
    # The release executable exists only for some systems.
    aihcReleaseFor = pkgs: let
      release = mkAihcRelease pkgs;
    in
      pkgs.lib.optionalAttrs (release != null) {aihc-release = release;};
    mkDevShells = import ./scripts/nix/dev-shells.nix {
      inherit mkWasiSysroot;
      inherit (haskell) mkHsPkgs;
    };
  in {
    packages = core.forAllSystems (pkgs: mkPackages pkgs // (mkChecks pkgs).packages // aihcReleaseFor pkgs);

    formatter = core.forAllSystems (pkgs: pkgs.alejandra);

    apps = core.forAllSystems mkApps;

    checks = core.forAllSystems (pkgs: (mkChecks pkgs).checks // aihcReleaseFor pkgs);

    devShells = core.forAllSystems mkDevShells;
  };
}
