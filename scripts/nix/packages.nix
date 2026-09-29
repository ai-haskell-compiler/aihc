{
  mkManual,
  mkLirExtension,
}: pkgs: {
  docs = mkManual pkgs;
  manual = mkManual pkgs;
  default = mkManual pkgs;
  vscode-lir = mkLirExtension pkgs;
}
