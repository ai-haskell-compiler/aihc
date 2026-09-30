# AIHC Intermediate Languages for VS Code

This extension supplies syntax highlighting for the intermediate languages of AIHC.

| Language | Files |
| --- | --- |
| System FC | `.fc` files, and the `core` files of `--keep-core` |
| GRIN | `.grin` files, and the `grin` files of `--keep-grin` |
| LIR | `.lir` files |

It also supplies bracket pairs, quote pairs, and the comment commands of each language.
The active VS Code theme selects the colors.

## Install

From the repository root, build the extension:

```sh
nix build .#vscode-lir
```

This command creates a VSIX package.
Install the package:

```sh
code --install-extension result/aihc-lir-0.2.0.vsix
```

If the `code` command is not available, use **Extensions: Install from VSIX...** in the VS Code command palette.
Select `result/aihc-lir-0.2.0.vsix`.
Open a `.lir` file.
The language mode must show **AIHC LIR**.
If necessary, use **Developer: Reload Window**.

The package uses `aihc.aihc-lir` as its local extension identifier.
A local installation does not require a Marketplace account.

## Change a grammar

The grammars are in `editors/grammars/syntaxes`.
The AIHC Manual uses the same grammars.
The directory `syntaxes` of this extension is a link to that directory.
See `editors/grammars/README.md` for the grammar tests.

Open `editors/vscode-lir` as a folder in VS Code.
Press **F5** to start an Extension Development Host.
In that window, open a file of one of the languages.
Use **Developer: Inspect Editor Tokens and Scopes** to inspect the colors and scopes.

References:

- [VS Code syntax highlight guide](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide)
- [VS Code language configuration guide](https://code.visualstudio.com/api/language-extensions/language-configuration-guide)
