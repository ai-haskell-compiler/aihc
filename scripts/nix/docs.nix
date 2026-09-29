{}: let
  # The pipeline pages include the programs that
  # scripts/generate-pipeline-examples.sh makes. `generated` is the output
  # directory of that script. mkdocs reads it from the working directory.
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
