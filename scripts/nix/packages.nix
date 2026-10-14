# The manual packages (docs, manual, and default) are in checks.nix,
# because the manual build compiles the pipeline examples.
{
  mkEditorGrammars,
  mkLirExtension,
}: pkgs: {
  aihc-grammars = mkEditorGrammars pkgs;
  link-bundle-python = pkgs.python3.withPackages (ps: [ps.cbor2]);
  vscode-lir = mkLirExtension pkgs;
}
