import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import oniguruma from "vscode-oniguruma";
import textmate from "vscode-textmate";

const require = createRequire(import.meta.url);
const root = new URL("../", import.meta.url);
const manifest = JSON.parse(await readFile(new URL("package.json", root), "utf8"));
const language = manifest.contributes.languages.find(({ extensions }) => extensions.includes(".lir"));
const contribution = manifest.contributes.grammars.find(({ language: id }) => id === language.id);
const grammarPath = new URL(contribution.path, root);
await oniguruma.loadWASM(await readFile(require.resolve("vscode-oniguruma/release/onig.wasm")));
const registry = new textmate.Registry({
  onigLib: Promise.resolve({
    createOnigScanner: (patterns) => new oniguruma.OnigScanner(patterns),
    createOnigString: (text) => new oniguruma.OnigString(text),
  }),
  loadGrammar: async (scope) => scope === contribution.scopeName
    ? textmate.parseRawGrammar(await readFile(grammarPath, "utf8"), fileURLToPath(grammarPath))
    : null,
});
const grammar = await registry.loadGrammar(contribution.scopeName);
assert.ok(grammar, "The language must have a grammar.");

const fixtures = new URL("fixtures/", import.meta.url);
let assertions = 0;
for (const name of (await readdir(fixtures)).filter((name) => name.endsWith(".json")).sort()) {
  const expected = JSON.parse(await readFile(new URL(name, fixtures), "utf8"));
  const lines = (await readFile(new URL(name.replace(/\.json$/, ".lir"), fixtures), "utf8")).split(/\r?\n/);
  let state = textmate.INITIAL;
  const tokens = lines.map((line) => {
    const result = grammar.tokenizeLine(line, state);
    assert.equal(result.stoppedEarly, false, `${name}: The tokenizer must complete.`);
    state = result.ruleStack;
    return result.tokens;
  });
  for (const { line, text, scope, parentScope, occurrence = 1 } of expected) {
    let start = -1;
    for (let index = 0; index < occurrence; index++) {
      start = lines[line - 1].indexOf(text, start + 1);
      assert.ok(start >= 0, `${name}:${line}: The fixture must contain ${JSON.stringify(text)}.`);
    }
    for (let offset = start; offset < start + text.length; offset++) {
      const token = tokens[line - 1].find(({ startIndex, endIndex }) => startIndex <= offset && offset < endIndex);
      assert.equal(token?.scopes.at(-1), scope,
        `${name}:${line}:${offset + 1}: ${JSON.stringify(text)} must have scope ${scope}.`);
      if (parentScope) {
        assert.ok(token.scopes.includes(parentScope),
          `${name}:${line}:${offset + 1}: ${JSON.stringify(text)} must have parent scope ${parentScope}.`);
      }
    }
    assertions++;
  }
  console.log(`PASS ${name}`);
}
assert.ok(assertions > 0, "The fixtures must have scope assertions.");
console.log(`PASS ${assertions} scope assertions`);
registry.dispose();
