// Check the scopes of selected tokens in the fixtures of each language.
//
// test/fixtures/<language>/<name>.json lists the assertions for the source
// file <name><extension> beside it.
import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import { languages, loadGrammar, tokenize } from "../grammars.mjs";

let assertions = 0;
for (const entry of languages) {
  const grammar = await loadGrammar(entry.id);
  const fixtures = new URL(`fixtures/${entry.id}/`, import.meta.url);
  const names = (await readdir(fixtures)).filter((name) => name.endsWith(".json")).sort();
  assert.ok(names.length > 0, `${entry.id}: The language must have fixtures.`);
  for (const name of names) {
    const expected = JSON.parse(await readFile(new URL(name, fixtures), "utf8"));
    const source = await readFile(new URL(name.replace(/\.json$/, entry.extension), fixtures), "utf8");
    const lines = source.split(/\r?\n/);
    const tokens = tokenize(grammar, source);
    for (const { line, text, scope, parentScope, occurrence = 1 } of expected) {
      const where = `${entry.id}/${name}:${line}`;
      let start = -1;
      for (let index = 0; index < occurrence; index++) {
        start = lines[line - 1].indexOf(text, start + 1);
        assert.ok(start >= 0, `${where}: The fixture must contain ${JSON.stringify(text)}.`);
      }
      for (let offset = start; offset < start + text.length; offset++) {
        const token = tokens[line - 1].find(({ startIndex, text: tokenText }) =>
          startIndex <= offset && offset < startIndex + tokenText.length);
        assert.equal(token?.scopes.at(-1), scope,
          `${where}:${offset + 1}: ${JSON.stringify(text)} must have scope ${scope}.`);
        if (parentScope) {
          assert.ok(token.scopes.includes(parentScope),
            `${where}:${offset + 1}: ${JSON.stringify(text)} must have parent scope ${parentScope}.`);
        }
      }
      assertions++;
    }
    console.log(`PASS ${entry.id}/${name}`);
  }
}
assert.ok(assertions > 0, "The fixtures must have scope assertions.");
console.log(`PASS ${assertions} scope assertions`);
