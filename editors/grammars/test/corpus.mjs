// Check that each grammar recognizes all of the compiler output in the
// repository: the expected programs of the System FC and GRIN golden tests,
// and the Lir test and runtime sources.
//
// A change to a printer that a grammar does not follow makes this test fail.
// AIHC_GRAMMAR_CORPUS_ROOT is the repository root. The default is the
// repository that contains this file.
import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import { join, relative } from "node:path";
import { fileURLToPath } from "node:url";
import YAML from "yaml";
import { invalidTokens, loadGrammar, tokenize } from "../grammars.mjs";

const root = process.env.AIHC_GRAMMAR_CORPUS_ROOT ?? fileURLToPath(new URL("../../../", import.meta.url));

// The Lir lint fixtures are left out: some of them are malformed on purpose.
const corpus = [
  { id: "fc", kind: "golden", directories: ["bin/aihc/compiler/fc/test/Test/Fixtures/golden"] },
  { id: "grin", kind: "golden", directories: ["bin/aihc/compiler/grin/test/Test/Fixtures/grin"] },
  {
    id: "lir",
    kind: "source",
    directories: [
      "bin/aihc/compiler/lir/test/Test/Fixtures/lir/asm",
      "bin/aihc/compiler/lir/test/Test/Fixtures/lir/eval",
      "bin/aihc/compiler/lir/test/Test/Fixtures/lir/include",
      "bin/aihc/compiler/arm64/test/Test/Fixtures/c-abi",
      "core-libs/aihc-rts/native",
    ],
  },
];

async function files(directory, extension) {
  const entries = await readdir(directory, { withFileTypes: true });
  const nested = await Promise.all(entries.map((entry) => {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) return files(path, extension);
    return entry.name.endsWith(extension) ? [path] : [];
  }));
  return nested.flat().sort();
}

// The expected program of a golden test that passes. A failing test expects
// an error message, not a program.
function goldenProgram(text) {
  const fixture = YAML.parse(text);
  if ((fixture.status ?? "pass") !== "pass") return null;
  return typeof fixture.expected === "string" && fixture.expected.trim() !== "" ? fixture.expected : null;
}

let failures = 0;
for (const { id, kind, directories } of corpus) {
  const grammar = await loadGrammar(id);
  const failuresBefore = failures;
  let checked = 0;
  for (const directory of directories) {
    const paths = await files(join(root, directory), kind === "golden" ? ".yaml" : ".lir");
    assert.ok(paths.length > 0, `${directory}: The corpus directory must have files.`);
    for (const path of paths) {
      const text = await readFile(path, "utf8");
      const program = kind === "golden" ? goldenProgram(text) : text;
      if (program === null) continue;
      checked++;
      for (const token of invalidTokens(tokenize(grammar, program))) {
        failures++;
        console.error(`${relative(root, path)}: ${id} line ${token.line}, column ${token.column}: unrecognized ${JSON.stringify(token.text)}`);
      }
    }
  }
  assert.ok(checked > 0, `${id}: The corpus must have programs.`);
  console.log(`${failures === failuresBefore ? "PASS" : "FAIL"} ${id}: ${checked} programs`);
}
assert.equal(failures, 0, `The grammars do not recognize ${failures} tokens of the corpus.`);
