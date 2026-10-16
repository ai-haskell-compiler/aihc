# Pin the browser tools independently of the compiler and the guide.
{pkgs}: let
  archive = name: version: hash:
    pkgs.fetchurl {
      url = "https://registry.npmjs.org/${name}/-/${builtins.baseNameOf name}-${version}.tgz";
      inherit hash;
    };
  bindgen = archive "@bytecodealliance/jco-transpile" "0.19.0" "sha256-XAKdbRzmgPJrFsJ+fXjnIe9zZcNYSntf5ZMmgUSQzDc=";
  shim = archive "@bytecodealliance/preview2-shim" "0.29.0" "sha256-7UxV9Hsaq5DdTjRfHQ9dOfa/4pyk1FJxxy3gezX2OEc=";
  clang = archive "@yowasp/clang" "22.0.0-git20542-10" "sha256-YjDqGvqWkfoGWTXPaMAWQv+bMcGD/orGTN/aAl3wYAk=";
in
  pkgs.runCommand "aihc-play-tools" {
    nativeBuildInputs = [pkgs.nodejs_24 pkgs.esbuild pkgs.makeWrapper];
  } ''
    mkdir -p node_modules/@bytecodealliance/{jco-transpile,preview2-shim} node_modules/@yowasp/clang
    tar -xf ${bindgen} --strip-components=1 -C node_modules/@bytecodealliance/jco-transpile
    tar -xf ${shim} --strip-components=1 -C node_modules/@bytecodealliance/preview2-shim
    tar -xf ${clang} --strip-components=1 -C node_modules/@yowasp/clang
    cp ${../build-play-tools.mjs} build.mjs
    cp ${../transpile-play.mjs} transpile.mjs
    node build.mjs "$out"
    mkdir -p "$out/bin"
    makeWrapper ${pkgs.nodejs_24}/bin/node "$out/bin/aihc-play-transpile" \
      --add-flags "$out/lib/transpile.mjs"
  ''
