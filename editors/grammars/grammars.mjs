// Load the TextMate grammars of the AIHC intermediate languages.
//
// The VS Code extension, the grammar tests, and the manual highlighter use
// the same grammar files through this module.
import { readFile } from "node:fs/promises";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import oniguruma from "vscode-oniguruma";
import textmate from "vscode-textmate";

const require = createRequire(import.meta.url);

// Each language has one grammar. A token with a scope that starts with
// "invalid." is text that the grammar does not recognize.
export const languages = [
  { id: "fc", name: "System FC", scopeName: "source.aihc-fc", grammar: "syntaxes/fc.tmLanguage.json", extension: ".fc" },
  { id: "grin", name: "GRIN", scopeName: "source.aihc-grin", grammar: "syntaxes/grin.tmLanguage.json", extension: ".grin" },
  { id: "lir", name: "Lir", scopeName: "source.lir", grammar: "syntaxes/lir.tmLanguage.json", extension: ".lir" },
];

export function language(id) {
  const found = languages.find((candidate) => candidate.id === id);
  if (!found) throw new Error(`Unknown language ${id}.`);
  return found;
}

let registryPromise;

async function registry() {
  registryPromise ??= (async () => {
    await oniguruma.loadWASM(await readFile(require.resolve("vscode-oniguruma/release/onig.wasm")));
    return new textmate.Registry({
      onigLib: Promise.resolve({
        createOnigScanner: (patterns) => new oniguruma.OnigScanner(patterns),
        createOnigString: (text) => new oniguruma.OnigString(text),
      }),
      loadGrammar: async (scopeName) => {
        const entry = languages.find((candidate) => candidate.scopeName === scopeName);
        if (!entry) return null;
        const path = new URL(entry.grammar, import.meta.url);
        return textmate.parseRawGrammar(await readFile(path, "utf8"), fileURLToPath(path));
      },
    });
  })();
  return registryPromise;
}

export async function loadGrammar(id) {
  const grammar = await (await registry()).loadGrammar(language(id).scopeName);
  if (!grammar) throw new Error(`The language ${id} has no grammar.`);
  return grammar;
}

// Tokenize a text. The result has one array of tokens for each line. Each
// token has its text and its scopes, from the outermost to the innermost.
export function tokenize(grammar, text) {
  let state = textmate.INITIAL;
  return text.split(/\r?\n/).map((line, index) => {
    const result = grammar.tokenizeLine(line, state);
    if (result.stoppedEarly) throw new Error(`Line ${index + 1}: the tokenizer stopped early.`);
    state = result.ruleStack;
    return result.tokens.map(({ startIndex, endIndex, scopes }) => ({
      text: line.slice(startIndex, endIndex),
      startIndex,
      scopes,
    }));
  });
}

// The tokens that the grammar does not recognize, with their positions.
export function invalidTokens(lines) {
  const found = [];
  lines.forEach((tokens, index) => {
    for (const token of tokens) {
      if (token.scopes.some((scope) => scope.startsWith("invalid."))) {
        found.push({ line: index + 1, column: token.startIndex + 1, text: token.text });
      }
    }
  });
  return found;
}
