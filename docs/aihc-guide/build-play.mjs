import { build } from "esbuild";
import { readFile, writeFile, readdir, mkdir } from "node:fs/promises";
import { createRequire } from "node:module";
import { dirname, resolve, join } from "node:path";
import { gzipSync } from "node:zlib";

const require = createRequire(import.meta.url);
const root = dirname(new URL(import.meta.url).pathname);
const [output, runtime] = process.argv.slice(2).map((path) => resolve(path));
if (!output || !runtime) throw new Error("Give the output and runtime directories.");
await mkdir(output, { recursive: true });
const bindgen = dirname(require.resolve("@bytecodealliance/jco-transpile/component"));
const llvm = dirname(require.resolve("@yowasp/clang"));
await build({
  entryPoints: [join(root, "src/play/run.js")],
  outfile: join(output, "run.js"),
  bundle: true,
  format: "esm",
  platform: "browser",
  external: ["node:*", "../compiler.js"],
  alias: { "play-wasm-tools": join(bindgen, "wasm-tools.js") },
  plugins: [{
    name: "compressed-llvm-assets",
    setup(build) {
      build.onLoad({ filter: /@yowasp\/clang\/gen\/bundle\.js$/ }, async ({ path }) => ({
        contents: (await readFile(path, "utf8")).replaceAll("await fetch3(new URL(", "await fetchCompressed(new URL(") + `
async function fetchCompressed(url) {
  const response = await fetch(url + '.gz');
  if (!response.ok) throw new Error('Cannot load the assembler: ' + response.status);
  return new Response(response.body.pipeThrough(new DecompressionStream('gzip')), {
    headers: { 'content-type': url.pathname.endsWith('.wasm') ? 'application/wasm' : 'application/octet-stream' }
  });
}
`,
        loader: "js",
      }));
    },
  }],
});
for (const name of await readdir(bindgen)) {
  if (/^(js-component-bindgen-component|wasm-tools)\.core.*\.wasm$/.test(name)) {
    await writeFile(join(output, name), await readFile(join(bindgen, name)));
  }
}
for (const name of await readdir(llvm)) {
  if (name.endsWith(".wasm") || name.endsWith(".tar")) {
    const bytes = gzipSync(await readFile(join(llvm, name)), { level: 9 });
    if (bytes.length > 25 * 1024 * 1024) throw new Error("The assembler asset exceeds the site limit: " + name);
    await writeFile(join(output, name + ".gz"), bytes);
  }
}
const names = (await readFile(join(runtime, "files.list"), "utf8")).trim().split("\n");
const files = Object.fromEntries(await Promise.all(names.map(async (name) => [name, (await readFile(join(runtime, name))).toString("base64")])));
for (const name of ["libc.a", "libclang_rt.builtins.a"]) {
  files[name] = (await readFile(join(process.env.AIHC_WASM_SYSROOT, "lib/wasm32-wasip3", name))).toString("base64");
}
const metadata = await readFile(join(runtime, "world.wasm"));
await writeFile(join(output, "runtime.json.gz"), gzipSync(JSON.stringify({ files, metadata: metadata.subarray(8).toString("base64") }), { level: 9 }));
