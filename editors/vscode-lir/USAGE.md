# AIHC LIR for VS Code

This extension supplies syntax highlighting for `.lir` files.
It recognizes LIR keywords, instructions, types, labels, values, symbols, literals, and comments.
It also supplies bracket pairs, quote pairs, and a line comment command.
The active VS Code theme selects the colors.

## Install

From the repository root, build the extension:

```sh
nix build .#vscode-lir
```

This command checks the grammar fixtures and creates a VSIX package.
Install the package:

```sh
code --install-extension result/aihc-lir-0.1.0.vsix
```

If the `code` command is not available, use **Extensions: Install from VSIX...** in the VS Code command palette.
Select `result/aihc-lir-0.1.0.vsix`.
Open a `.lir` file.
The language mode must show **AIHC LIR**.
If necessary, use **Developer: Reload Window**.

The package uses `aihc.aihc-lir` as its local extension identifier.
A local installation does not require a Marketplace account.

## Change the grammar

Use `docs/lir.md` and `bin/aihc/compiler/lir/src/Aihc/Lir/Parser.hs` as the language references.
The grammar file is `syntaxes/lir.tmLanguage.json`.
The grammar uses standard TextMate scopes so existing themes can select colors.
It does not check types or report compiler errors.

Open `editors/vscode-lir` as a folder in VS Code.
Press **F5** to start an Extension Development Host.
In that window, open a LIR file.
Use **Developer: Inspect Editor Tokens and Scopes** to inspect the colors and scopes.

## Check the grammar

Each fixture has a `.lir` source file and a `.json` file with expected scopes.
Line numbers start at one.
Each assertion gives the source text and its most specific TextMate scope.
An optional `parentScope` checks the string scope of a quoted name.
This scope prevents automatic bracket pairs inside quoted names.
For repeated text, set `occurrence` to select the required occurrence.
Each file starts with a fresh tokenizer state.
The state continues across lines to check multiline strings and quoted names.
The fixtures include source fragments and incomplete source for editor use.

From the repository root, install the test dependencies:

```sh
nix shell --inputs-from . nixpkgs#nodejs --command npm --prefix editors/vscode-lir ci
```

Run the fixture checks:

```sh
nix shell --inputs-from . nixpkgs#nodejs --command npm --prefix editors/vscode-lir test
```

The tests use `vscode-textmate` and `vscode-oniguruma`, which also supply the VS Code grammar engine.
`just check` and `nix flake check` build the extension and run these checks.
The VSIX package contains no test dependencies.

References:

- [VS Code syntax highlight guide](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide)
- [VS Code language configuration guide](https://code.visualstudio.com/api/language-extensions/language-configuration-guide)
