{}: let
  mkManual = pkgs:
    pkgs.runCommand "aihc-manual" {
      nativeBuildInputs = [pkgs.python3Packages.mkdocs-material];
    } ''
      mkdocs build \
        --strict \
        --config-file ${../../docs/aihc-manual}/mkdocs.yml \
        --site-dir "$out"
    '';
in {
  inherit mkManual;
}
