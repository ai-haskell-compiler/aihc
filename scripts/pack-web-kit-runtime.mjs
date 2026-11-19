// Write runtime.pack.gz: the linker inputs of the web kit in one pack.
//
// Usage: node pack-web-kit-runtime.mjs OUTPUT RUNTIME
//
// RUNTIME is the output of `aihc-dev play-runtime`, with world.wasm. The
// pack has the runtime objects, the archives of the core libraries, the C
// library, and the component metadata of the WIT world.
//
// A pack is binary. The same files as base64 in JSON were a download that
// was 30% larger. The format is:
//   8 bytes   "AIHCPACK"
//   4 bytes   the format version, 1, little-endian
//   4 bytes   the length N of the index, little-endian
//   N bytes   the index: UTF-8 JSON {"files":[{"name","offset","bytes"}]}
//   the files, each at its offset from the end of the index
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { resolve, join } from "node:path";
import { gzipSync } from "node:zlib";

const [output, runtime] = process.argv.slice(2).map((path) => resolve(path));
await mkdir(output, { recursive: true });
const names = (await readFile(join(runtime, "files.list"), "utf8")).trim().split("\n");
const entries = await Promise.all(names.map(async (name) => [name, await readFile(join(runtime, name))]));
for (const name of ["libc.a", "libclang_rt.builtins.a"]) {
  entries.push([name, await readFile(join(process.env.AIHC_WASM_SYSROOT, "lib/wasm32-wasip3", name))]);
}
// The linker output becomes a component with this metadata. The first eight
// bytes of world.wasm are the WebAssembly header.
entries.push(["component-metadata", (await readFile(join(runtime, "world.wasm"))).subarray(8)]);

let offset = 0;
const files = entries.map(([name, bytes]) => {
  const entry = { name, offset, bytes: bytes.length };
  offset += bytes.length;
  return entry;
});
const index = Buffer.from(JSON.stringify({ files }));
const header = Buffer.alloc(16);
header.write("AIHCPACK", 0, "latin1");
header.writeUInt32LE(1, 8);
header.writeUInt32LE(index.length, 12);
const pack = Buffer.concat([header, index, ...entries.map(([, bytes]) => bytes)]);
await writeFile(join(output, "runtime.pack.gz"), gzipSync(pack, { level: 9 }));
