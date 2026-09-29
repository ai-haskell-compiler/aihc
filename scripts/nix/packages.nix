# The manual packages (docs, manual, and default) are in checks.nix,
# because the manual build compiles the pipeline examples.
{mkLirExtension}: pkgs: {
  vscode-lir = mkLirExtension pkgs;
}
