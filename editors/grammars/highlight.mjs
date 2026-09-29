#!/usr/bin/env node
// Highlight one program of an AIHC intermediate language as HTML.
//
// Usage: highlight.mjs LANGUAGE < PROGRAM > HTML
//
// LANGUAGE is fc, grin, or lir. The output is a code block with the classes
// of the Pygments highlighter, so the theme of a site gives the colors. The
// command fails when the grammar does not recognize a token of the program.
import { readFileSync } from "node:fs";
import { invalidTokens, language, loadGrammar, tokenize } from "./grammars.mjs";

// The Pygments class for a TextMate scope. The first prefix that matches
// the innermost scope of a token gives the class.
const classes = [
  ["comment", "c1"],
  ["constant.character.escape", "se"],
  ["string", "s"],
  ["constant.numeric", "m"],
  ["constant.language", "kc"],
  ["keyword.operator.instruction", "k"],
  ["keyword.operator", "o"],
  ["keyword", "k"],
  ["storage.type", "no"],
  ["support.function", "nf"],
  ["entity.name.namespace", "c"],
  ["entity.name.type", "no"],
  ["entity.name.constructor", "no"],
  ["entity.name.function", "nf"],
  ["entity.name.label", "nl"],
  ["variable.other.global", "nf"],
  ["variable", "nv"],
  ["punctuation", "p"],
];

function classFor(scopes) {
  for (let index = scopes.length - 1; index >= 0; index--) {
    const match = classes.find(([prefix]) => scopes[index] === prefix || scopes[index].startsWith(`${prefix}.`));
    if (match) return match[1];
  }
  return null;
}

function escapeHtml(text) {
  return text.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
}

const [id] = process.argv.slice(2);
if (!id) {
  console.error("Usage: highlight.mjs LANGUAGE < PROGRAM > HTML");
  process.exit(2);
}
const entry = language(id);
const lines = tokenize(await loadGrammar(id), readFileSync(0, "utf8").replace(/\n$/, ""));
const invalid = invalidTokens(lines);
if (invalid.length > 0) {
  for (const token of invalid) {
    console.error(`line ${token.line}, column ${token.column}: the ${entry.name} grammar does not recognize ${JSON.stringify(token.text)}`);
  }
  process.exit(1);
}
const body = lines.map((tokens) => tokens.map(({ text, scopes }) => {
  const name = classFor(scopes);
  return name ? `<span class="${name}">${escapeHtml(text)}</span>` : escapeHtml(text);
}).join("")).join("\n");
// The markup of a Pygments code block, so the theme styles it the same way.
process.stdout.write(`<div class="highlight"><pre><span></span><code>${body}\n</code></pre></div>`);
