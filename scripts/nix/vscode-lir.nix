# The VS Code extension for the AIHC intermediate languages. Its grammars
# are the shared grammars in editors/grammars; the aihc-grammars package
# tests them. The syntaxes directory of the extension is a link to them, so
# this build copies them into the package.
pkgs:
pkgs.stdenvNoCC.mkDerivation {
  pname = "aihc-vscode-lir";
  version = "0.2.0";
  src = pkgs.lib.fileset.toSource {
    root = ../../editors;
    fileset = pkgs.lib.fileset.unions [
      ../../editors/grammars/syntaxes
      ../../editors/vscode-lir/package.json
      ../../editors/vscode-lir/fc-language-configuration.json
      ../../editors/vscode-lir/grin-language-configuration.json
      ../../editors/vscode-lir/lir-language-configuration.json
      ../../editors/vscode-lir/.vscodeignore
      ../../editors/vscode-lir/USAGE.md
    ];
  };
  nativeBuildInputs = [pkgs.vsce];
  buildPhase = ''
    runHook preBuild
    cd vscode-lir
    cp -R ../grammars/syntaxes syntaxes
    cp ${../../LICENSE} LICENSE
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    vsce package --no-dependencies --out "$out/aihc-lir-0.2.0.vsix"
    runHook postInstall
  '';
  meta = {
    description = "VS Code syntax highlighting for the AIHC intermediate languages";
    license = pkgs.lib.licenses.unlicense;
    platforms = pkgs.lib.platforms.all;
  };
}
