// Write manifest.json for the web kit.
//
// Usage: node write-web-kit-manifest.mjs OUTPUT VERSION TOOLS_ASSETS [--tools-url URL] [--data-url URL]
//
// The manifest lists each file of the kit with its size and SHA-256. The
// page shows the total size before the download. The build is a hash of the
// files. It names the browser cache, so a new build does not use old files.
//
// A release has all files beside manifest.json, and uses neither option.
// The options are for The Lazy Reader:
//   --tools-url URL  The browser tools are at URL/ and not in OUTPUT/tools/.
//   --data-url URL   The compressed files are at URL/BUILD/. Each build
//                    makes new ones, and the preview workflow publishes
//                    them there. Thus the preview branch does not keep a
//                    copy of each build.
import { readFile, readdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { createHash } from "node:crypto";

const [output, version, toolsAssets, ...options] = process.argv.slice(2);
const option = (name) => (options.includes(name) ? options[options.indexOf(name) + 1].replace(/\/*$/, "/") : null);
const toolsUrl = option("--tools-url");
const dataUrl = option("--data-url");

const GLUE = ["aihc.js", "worker.js", "host.js", "manifest.js"];
const COMPILER = "aihc-play-component.js";
const isDocument = (path) => /(^|\/)(README\.md|[^/]*(LICENSE|COPYING|NOTICE)[^/]*)$/i.test(path);
const group = (path) => {
  if (isDocument(path)) return "doc";
  if (GLUE.includes(path)) return "glue";
  if (path.startsWith("tools/") || path === "runtime.pack.gz") return "linker";
  return "compiler";
};

const files = [];
const add = async (path, source, url) => {
  const bytes = await readFile(source);
  const file = { path, bytes: bytes.length, sha256: createHash("sha256").update(bytes).digest("hex"), group: group(path) };
  // The browser imports a module. Each other file goes through fetch.
  if (path.endsWith(".js")) file.module = true;
  if (url) file.url = url;
  files.push(file);
};
async function walk(directory, prefix, url) {
  for (const entry of (await readdir(directory, { withFileTypes: true })).sort((a, b) => (a.name < b.name ? -1 : 1))) {
    const path = prefix + entry.name;
    if (entry.isDirectory()) await walk(join(directory, entry.name), path + "/", url && url + entry.name + "/");
    else if (path !== "manifest.json") await add(path, join(directory, entry.name), url && url + entry.name);
  }
}
await walk(output, "", null);
if (toolsUrl) await walk(toolsAssets, "tools/", toolsUrl);
files.sort((a, b) => (a.path < b.path ? -1 : 1));

for (const path of [...GLUE, COMPILER, "core.bundle.gz", "runtime.pack.gz", "tools/tools.js"]) {
  if (!files.some((file) => file.path === path)) throw new Error("The kit does not have " + path + ".");
}
const build = createHash("sha256").update(JSON.stringify(files.map(({ path, bytes, sha256 }) => ({ path, bytes, sha256 })))).digest("hex").slice(0, 16);
if (dataUrl) {
  for (const file of files) if (!file.url && file.path.endsWith(".gz")) file.url = dataUrl + build + "/" + file.path;
}
const manifest = {
  schema: 1,
  name: "aihc-web-kit",
  version,
  build,
  entry: "aihc.js",
  worker: "worker.js",
  compiler: { module: COMPILER, bundle: "core.bundle.gz" },
  linker: { tools: "tools/tools.js", runtime: "runtime.pack.gz" },
  // A later release lists the library packs here. See the README of the kit.
  packs: [],
  files,
};
await writeFile(join(output, "manifest.json"), JSON.stringify(manifest, null, 1) + "\n");
