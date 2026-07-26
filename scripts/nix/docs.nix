{}: let
  mkUserGuide = pkgs:
    pkgs.runCommand "aihc-user-guide" {
      nativeBuildInputs = [pkgs.python3Packages.mkdocs-material];
    } ''
      mkdocs build \
        --strict \
        --config-file ${../../docs/aihc-users-guide}/mkdocs.yml \
        --site-dir "$out"
    '';
in {
  inherit mkUserGuide;
}
