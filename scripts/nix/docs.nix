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

  # The Lazy Reader. The playground of the guide compiles the primitive
  # modules of aihc and the small Prelude of aihc-play in the browser, so
  # the site carries their sources under play/. The helper itself is not
  # part of this derivation.
  mkGuide = pkgs: guideSrc: primSrc: playSrc:
    pkgs.runCommand "aihc-lazy-reader" {
      nativeBuildInputs = [pkgs.python3Packages.mkdocs-material];
    } ''
      cp -R --no-preserve=mode ${guideSrc} guide
      mkdir -p guide/src/play/prim/GHC guide/src/play/prim/Aihc/Prim
      for module in GHC/Types.hs GHC/Prim.hs Aihc/Prim/IO.hs GHC/Tuple.hs GHC/CString.hs; do
        cp ${primSrc}/src/$module guide/src/play/prim/$module
      done
      cp -R --no-preserve=mode ${playSrc}/bin/aihc-play/prelude guide/src/play/prelude
      cd guide
      mkdocs build \
        --strict \
        --config-file mkdocs.yml \
        --site-dir "$out"
    '';
in {
  inherit mkManual mkGuide;
}
