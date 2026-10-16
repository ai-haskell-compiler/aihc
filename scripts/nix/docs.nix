{}: let
  # The pipeline pages include the programs that `aihc-dev pipeline-examples`
  # makes. `generated` is the output directory of that command. mkdocs reads
  # it from the working directory. The hook of the manual runs
  # aihc-highlight from `highlighter` for the fences of the intermediate
  # languages.
  mkManual = pkgs: generated: highlighter:
    pkgs.runCommand "aihc-manual" {
      nativeBuildInputs = [pkgs.python3Packages.mkdocs-material highlighter];
    } ''
      cp -R --no-preserve=mode ${../../docs/aihc-manual} manual
      cp -R --no-preserve=mode ${generated} manual/generated
      cd manual
      mkdocs build \
        --strict \
        --config-file mkdocs.yml \
        --site-dir "$out"
    '';

  # The Lazy Reader. The helper of its playground and the bundle of the
  # core libraries are not part of this derivation: the deploy workflow
  # builds them with `nix run .#build-lazy-reader-helper` and adds them to
  # the site.
  mkGuide = pkgs: guideSrc:
    pkgs.runCommand "aihc-lazy-reader" {
      nativeBuildInputs = [pkgs.python3Packages.mkdocs-material];
    } ''
      cp -R --no-preserve=mode ${guideSrc} guide
      cd guide
      mkdocs build \
        --strict \
        --config-file mkdocs.yml \
        --site-dir "$out"
    '';
in {
  inherit mkManual mkGuide;
}
