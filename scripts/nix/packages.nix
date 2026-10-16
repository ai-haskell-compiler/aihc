# The manual packages (docs, manual, and default) are in checks.nix,
# because the manual build compiles the pipeline examples.
{
  mkEditorGrammars,
  mkLirExtension,
}: pkgs: {
  aihc-grammars = mkEditorGrammars pkgs;
  aihc-play-tools = import ./play-tools.nix {inherit pkgs;};
  vscode-lir = mkLirExtension pkgs;
}
