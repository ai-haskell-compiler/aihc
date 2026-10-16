import { readFile, writeFile, mkdir } from "node:fs/promises";
import { resolve, join } from "node:path";
import { gzipSync } from "node:zlib";
const [output, runtime] = process.argv.slice(2).map((path) => resolve(path));
await mkdir(output, { recursive: true });
const names = (await readFile(join(runtime, "files.list"), "utf8")).trim().split("\n");
const files = Object.fromEntries(await Promise.all(names.map(async (name) => [name, (await readFile(join(runtime, name))).toString("base64")])));
for (const name of ["libc.a", "libclang_rt.builtins.a"]) {
  files[name] = (await readFile(join(process.env.AIHC_WASM_SYSROOT, "lib/wasm32-wasip3", name))).toString("base64");
}
const metadata = await readFile(join(runtime, "world.wasm"));
await writeFile(join(output, "runtime.json.gz"), gzipSync(JSON.stringify({ files, metadata: metadata.subarray(8).toString("base64") }), { level: 9 }));
