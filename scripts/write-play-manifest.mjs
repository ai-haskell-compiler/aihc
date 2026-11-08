// Write tools.json for the program playground of The Lazy Reader.
//
// Usage: node write-play-manifest.mjs OUTPUT TOOLS_URL TOOLS_ASSETS [HELPER_URL]
//
// The file gives the URL of the browser tools and each large file that the
// playground downloads, with its size. The page shows the total size before
// the download. The version names the browser cache of the files, so a new
// build does not use old files.
//
// The compressed helper files are large, and each build makes new ones. With
// HELPER_URL, tools.json puts them at HELPER_URL/VERSION/, and the preview
// workflow publishes them there. Thus the preview branch does not keep a copy
// of each build. Without HELPER_URL, the files are beside tools.json.
import { readFile, readdir, stat, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { createHash } from "node:crypto";

const [output, toolsUrl, toolsAssets, helperUrl] = process.argv.slice(2);
const toolsBase = toolsUrl.slice(0, toolsUrl.lastIndexOf("/") + 1);
const files = [];
const add = async (name, path, url) => {
  const bytes = await readFile(path);
  // The page imports a JavaScript module. The other files go through fetch.
  files.push({ name, url, bytes: bytes.length, module: name.endsWith(".js"), sha256: createHash("sha256").update(bytes).digest("hex") });
};
for (const name of (await readdir(output)).sort()) {
  // The component module stays beside tools.json. The page imports it as a
  // module of the site. The URL of a compressed file depends on the version.
  if (name === "aihc-play-component.js") await add(name, join(output, name), "./" + name);
  else if (name.endsWith(".gz")) await add(name, join(output, name), null);
}
for (const name of (await readdir(toolsAssets)).sort()) {
  if (!/LICENSE|COPYING|NOTICE/i.test(name) && (await stat(join(toolsAssets, name))).isFile()) await add(name, join(toolsAssets, name), toolsBase + name);
}
const version = createHash("sha256").update(JSON.stringify(files.map(({ name, bytes, sha256 }) => ({ name, bytes, sha256 })))).digest("hex").slice(0, 16);
const helper = helperUrl ? helperUrl.replace(/\/*$/, "/") + version + "/" : "./";
for (const file of files) file.url ??= helper + file.name;
await writeFile(join(output, "tools.json"), JSON.stringify({ url: toolsUrl, version, helper, files: files.map(({ name, sha256, ...file }) => file) }));
