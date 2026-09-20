pkgs:
pkgs.buildNpmPackage {
  pname = "aihc-vscode-lir";
  version = "0.1.0";
  src = pkgs.lib.fileset.toSource {
    root = ../../editors/vscode-lir;
    fileset = pkgs.lib.fileset.unions [
      ../../editors/vscode-lir/package.json
      ../../editors/vscode-lir/package-lock.json
      ../../editors/vscode-lir/language-configuration.json
      ../../editors/vscode-lir/.vscodeignore
      ../../editors/vscode-lir/syntaxes
      ../../editors/vscode-lir/test
      ../../editors/vscode-lir/USAGE.md
    ];
  };
  npmDepsHash = "sha256-PTLwIybuVXdTtkf7pGZ/QsTd8+AZ2zi4fmKXkfrUDp0=";
  dontNpmBuild = true;
  npmInstallFlags = ["--ignore-scripts"];
  nativeBuildInputs = [pkgs.vsce];
  doCheck = true;
  checkPhase = ''
    runHook preCheck
    npm test
    runHook postCheck
  '';
  installPhase = ''
    runHook preInstall
    cp ${../../LICENSE} LICENSE
    mkdir -p "$out"
    vsce package --no-dependencies --out "$out/aihc-lir-0.1.0.vsix"
    runHook postInstall
  '';
  meta = {
    description = "VS Code syntax highlighting for AIHC LIR";
    license = pkgs.lib.licenses.unlicense;
    platforms = pkgs.lib.platforms.all;
  };
}
