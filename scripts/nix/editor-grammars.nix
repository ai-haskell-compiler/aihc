# The TextMate grammars of the AIHC intermediate languages, their tests, and
# the aihc-highlight command that the manual uses. The check phase also
# tokenizes the compiler output in the repository, so a printer change that
# a grammar does not follow fails this build.
pkgs: let
  root = ../..;
  corpus = pkgs.lib.fileset.toSource {
    inherit root;
    fileset = pkgs.lib.fileset.unions [
      ../../bin/aihc/compiler/fc/test/Test/Fixtures/golden
      ../../bin/aihc/compiler/grin/test/Test/Fixtures/grin
      ../../bin/aihc/compiler/lir/test/Test/Fixtures/lir/asm
      ../../bin/aihc/compiler/lir/test/Test/Fixtures/lir/eval
      ../../bin/aihc/compiler/lir/test/Test/Fixtures/lir/include
      ../../bin/aihc/compiler/native/test/Test/Fixtures/c-abi
      (pkgs.lib.fileset.fileFilter (file: file.hasExt "lir") ../../core-libs/aihc-rts/native)
    ];
  };
in
  pkgs.buildNpmPackage {
    pname = "aihc-grammars";
    version = "0.1.0";
    src = pkgs.lib.fileset.toSource {
      root = ../../editors/grammars;
      fileset = pkgs.lib.fileset.unions [
        ../../editors/grammars/package.json
        ../../editors/grammars/package-lock.json
        ../../editors/grammars/grammars.mjs
        ../../editors/grammars/highlight.mjs
        ../../editors/grammars/syntaxes
        ../../editors/grammars/test
      ];
    };
    npmDepsHash = "sha256-QadTuh3AZ+DqwiYDwE9YppB+kG7cGzyWzy9vMueZ9ZE=";
    dontNpmBuild = true;
    npmInstallFlags = ["--ignore-scripts"];
    doCheck = true;
    checkPhase = ''
      runHook preCheck
      AIHC_GRAMMAR_CORPUS_ROOT=${corpus} npm test
      runHook postCheck
    '';
    meta = {
      description = "TextMate grammars and a highlighter for the AIHC intermediate languages";
      license = pkgs.lib.licenses.unlicense;
      mainProgram = "aihc-highlight";
      platforms = pkgs.lib.platforms.all;
    };
  }
