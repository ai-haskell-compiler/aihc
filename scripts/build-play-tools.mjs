import { readFile, writeFile, readdir, mkdir, copyFile } from "node:fs/promises";
import { execFileSync } from "node:child_process";
import { gzipSync } from "node:zlib";
import { createHash } from "node:crypto";

const output = process.argv[2];
const vendor = "node_modules/@bytecodealliance/jco-transpile/vendor";
const llvm = "node_modules/@yowasp/clang/gen";
const assets = output + "/assets";
await mkdir(assets, { recursive: true });
await mkdir(output + "/lib", { recursive: true });

// Use the browser shims for both the browser and the native transpiler.
const entry = `
import { $init as bindgenInit, generate } from './${vendor}/js-component-bindgen-component.js';
import { $init as toolsInit, tools } from './${vendor}/wasm-tools.js';
export { commands } from './${llvm}/bundle.js';
export async function transpile(...args) { await bindgenInit; return generate(...args); }
export async function componentNew(...args) { await toolsInit; return tools.componentNew(...args); }
`;
const original = await readFile(llvm + "/bundle.js", "utf8");
const needle = "await fetch3(new URL(";
if (!original.includes(needle)) throw new Error("The LLVM asset loader has changed.");
await writeFile(llvm + "/bundle.js", original.replaceAll(needle, "await fetchCompressed(new URL(") + `
async function fetchCompressed(url) {
  const response = await fetch(url + '.gz');
  if (!response.ok) throw new Error('Cannot load the assembler: ' + response.status);
  return new Response(response.body.pipeThrough(new DecompressionStream('gzip')), {
    headers: { 'content-type': url.pathname.endsWith('.wasm') ? 'application/wasm' : 'application/octet-stream' }
  });
}
`);
await writeFile("tools.mjs", entry);
const flags = ["--bundle", "--format=esm", "--platform=browser", "--external:node:*", "--legal-comments=eof"];
execFileSync("esbuild", ["tools.mjs", ...flags, "--outfile=" + assets + "/tools.js"], { stdio: "inherit" });
// The native transpiler needs only the component generator.
execFileSync("esbuild", [vendor + "/js-component-bindgen-component.js", ...flags, "--outfile=" + output + "/lib/bindgen.mjs"], { stdio: "inherit" });
await copyFile("transpile.mjs", output + "/lib/transpile.mjs");
for (const name of (await readdir(vendor)).sort()) {
  if (/^(js-component-bindgen-component|wasm-tools)\.core.*\.wasm$/.test(name)) {
    await copyFile(vendor + "/" + name, assets + "/" + name);
    if (name.startsWith("js-component-bindgen-component")) await copyFile(vendor + "/" + name, output + "/lib/" + name);
  }
}
for (const name of (await readdir(llvm)).sort()) {
  if (name.endsWith(".wasm") || name.endsWith(".tar")) {
    await writeFile(assets + "/" + name + ".gz", gzipSync(await readFile(llvm + "/" + name), { level: 9 }));
  }
}
// Keep the upstream licenses with the public assets.
for (const [directory, prefix] of [[vendor + "/..", "jco"], ["node_modules/@bytecodealliance/preview2-shim", "shim"], [llvm + "/..", "llvm"]]) {
  for (const name of (await readdir(directory)).sort()) {
    if (/^(LICENSE|COPYING|NOTICE)/i.test(name)) await copyFile(directory + "/" + name, assets + "/" + prefix + "-" + name);
  }
}
const manifest = [];
for (const name of (await readdir(assets)).sort()) {
  manifest.push([name, createHash("sha256").update(await readFile(assets + "/" + name)).digest("hex")]);
}
const version = createHash("sha256").update(JSON.stringify(manifest)).digest("hex");
await writeFile(output + "/version", version + "\n");
await writeFile(output + "/manifest.json", JSON.stringify(manifest, null, 2) + "\n");
