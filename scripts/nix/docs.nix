{}: let
  # The pipeline pages include the programs that `aihc-dev pipeline-examples`
  # makes. `generated` is the output directory of that command. mkdocs reads
  # it from the working directory.
  mkManual = pkgs: generated:
    pkgs.runCommand "aihc-manual" {
      nativeBuildInputs = [pkgs.python3Packages.mkdocs-material];
    } ''
      cp -R --no-preserve=mode ${../../docs/aihc-manual} manual
      cp -R --no-preserve=mode ${generated} manual/generated
      cd manual
      mkdocs build \
        --strict \
        --config-file mkdocs.yml \
        --site-dir "$out"
    '';
in {
  inherit mkManual;
}
