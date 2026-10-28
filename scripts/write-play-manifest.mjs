// Write tools.json for the program playground of The Lazy Reader.
//
// Usage: node write-play-manifest.mjs OUTPUT TOOLS_URL TOOLS_ASSETS
//
// The file gives the URL of the browser tools and each large file that the
// playground downloads, with its size. The page shows the total size before
// the download. The version names the browser cache of the files, so a new
// build does not use old files.
import { readFile, readdir, stat, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { createHash } from "node:crypto";

const [output, toolsUrl, toolsAssets] = process.argv.slice(2);
const toolsBase = toolsUrl.slice(0, toolsUrl.lastIndexOf("/") + 1);
const files = [];
const add = async (url, path) => {
  const bytes = await readFile(path);
  // The page imports a JavaScript module. The other files go through fetch.
  files.push({ url, bytes: bytes.length, module: url.endsWith(".js"), sha256: createHash("sha256").update(bytes).digest("hex") });
};
for (const name of (await readdir(output)).sort()) {
  if (name === "aihc-play-component.js" || name.endsWith(".gz")) await add("./" + name, join(output, name));
}
for (const name of (await readdir(toolsAssets)).sort()) {
  if (!/LICENSE|COPYING|NOTICE/i.test(name) && (await stat(join(toolsAssets, name))).isFile()) await add(toolsBase + name, join(toolsAssets, name));
}
const version = createHash("sha256").update(JSON.stringify(files)).digest("hex").slice(0, 16);
await writeFile(join(output, "tools.json"), JSON.stringify({ url: toolsUrl, version, files: files.map(({ sha256, ...file }) => file) }));
