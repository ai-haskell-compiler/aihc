{
  mkUserGuide,
  mkLirExtension,
}: pkgs: {
  docs = mkUserGuide pkgs;
  user-guide = mkUserGuide pkgs;
  default = mkUserGuide pkgs;
  vscode-lir = mkLirExtension pkgs;
}
