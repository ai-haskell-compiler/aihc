import { readFile, writeFile, mkdir } from "node:fs/promises";
import { $init, generate } from "./bindgen.mjs";

const [input, output] = process.argv.slice(2);
await $init;
const result = generate(new Uint8Array(await readFile(input)), {
  name: "aihc-play-component",
  instantiation: { tag: "async" },
  noNodejsCompat: true,
  noTypescript: true,
  asyncMode: { tag: "jspi", val: { imports: [], exports: [] } },
});
await mkdir(output, { recursive: true });
for (const [name, bytes] of result.files) await writeFile(output + "/" + name, bytes);
