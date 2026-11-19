// The smoke test of the web kit.
//
// Usage: node web-kit/test/smoke.mjs (--kit DIR | --archive FILE) [--timeout SECONDS]
//
// The test does these steps:
//   1. It compares each file of the kit with manifest.json.
//   2. It serves the kit on one port and a test page on a different port.
//      Thus the page loads the kit from a different origin, as a website
//      loads a release from assets.aihc.app.
//   3. It starts a headless browser. The page type-checks, compiles, and
//      runs the programs of this directory, and sends the results back.
//
// AIHC_SMOKE_BROWSER names the browser executable. Without it, the test
// finds Chromium below AIHC_SMOKE_BROWSERS, which the Nix app sets.
import { createServer } from "node:http";
import { createHash } from "node:crypto";
import { execFileSync, spawn } from "node:child_process";
import { mkdtemp, readFile, readdir, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, normalize, sep } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const option = (name) => (args.includes(name) ? args[args.indexOf(name) + 1] : null);
const timeout = Number(option("--timeout") ?? 600) * 1000;
const temporary = await mkdtemp(join(tmpdir(), "aihc-web-kit-smoke-"));

let browser;
async function cleanUp() {
  browser?.kill("SIGKILL");
  await rm(temporary, { recursive: true, force: true, maxRetries: 5, retryDelay: 200 });
}
async function fail(message) {
  console.error("FAIL: " + message);
  await cleanUp();
  process.exit(1);
}

// ---------------------------------------------------------------------------
// The kit

let kit = option("--kit");
if (option("--archive")) {
  execFileSync("tar", ["-xzf", option("--archive"), "-C", temporary]);
  const [top] = await readdir(temporary);
  kit = join(temporary, top);
}
if (!kit) {
  console.error("usage: smoke.mjs (--kit DIR | --archive FILE) [--timeout SECONDS]");
  process.exit(2);
}

async function listFiles(directory, prefix = "") {
  const names = [];
  for (const name of await readdir(directory)) {
    // The Nix store has links to directories. stat follows a link.
    const isDirectory = (await stat(join(directory, name)).catch(() => null))?.isDirectory();
    if (isDirectory) names.push(...(await listFiles(join(directory, name), prefix + name + "/")));
    else names.push(prefix + name);
  }
  return names;
}

const manifest = JSON.parse(await readFile(join(kit, "manifest.json"), "utf8"));
const listed = new Set(manifest.files.map((file) => file.path));
for (const file of manifest.files) {
  if (file.url) await fail("A release kit must have each file in the kit, but " + file.path + " is at " + file.url + ".");
  const bytes = await readFile(join(kit, file.path)).catch(() => null);
  if (!bytes) await fail("The kit does not have " + file.path + ".");
  if (bytes.length !== file.bytes) await fail(file.path + " has " + bytes.length + " bytes, and the manifest gives " + file.bytes + ".");
  if (createHash("sha256").update(bytes).digest("hex") !== file.sha256) await fail("The SHA-256 of " + file.path + " is not the one in the manifest.");
}
for (const path of await listFiles(kit)) {
  if (path !== "manifest.json" && !listed.has(path)) await fail("The manifest does not list " + path + ".");
}
console.log("The " + manifest.files.length + " files of kit " + manifest.version + " (build " + manifest.build + ") agree with the manifest.");

// ---------------------------------------------------------------------------
// The servers

const TYPES = { js: "text/javascript", json: "application/json", wasm: "application/wasm", gz: "application/gzip", md: "text/markdown", html: "text/html" };
const listen = (handler) =>
  new Promise((resolve) => {
    const server = createServer(handler);
    server.listen(0, "127.0.0.1", () => resolve(server));
  });

// The kit server gives the same headers as assets.aihc.app.
const kitServer = await listen(async (request, response) => {
  const path = normalize(join(kit, decodeURIComponent(new URL(request.url, "http://localhost").pathname)));
  try {
    if (!path.startsWith(kit + sep) || !(await stat(path)).isFile()) throw new Error("not a file");
    response.writeHead(200, {
      "content-type": TYPES[path.slice(path.lastIndexOf(".") + 1)] ?? "text/plain",
      "access-control-allow-origin": "*",
      "cache-control": "public, max-age=31536000, immutable",
    });
    response.end(await readFile(path));
  } catch {
    response.writeHead(404, { "access-control-allow-origin": "*" });
    response.end();
  }
});
const kitUrl = "http://127.0.0.1:" + kitServer.address().port + "/";

const programs = {
  hello: await readFile(join(here, "Hello.hs"), "utf8"),
  echo: await readFile(join(here, "Echo.hs"), "utf8"),
  wrong: await readFile(join(here, "Wrong.hs"), "utf8"),
};
const page = `<!doctype html>
<meta charset="utf-8">
<title>The smoke test of the AIHC web kit</title>
<script type="module">
const send = (path, value) => fetch(path, { method: "POST", body: JSON.stringify(value) });
const log = (text) => send("/log", text);
addEventListener("error", (event) => log("error: " + event.message));
addEventListener("unhandledrejection", (event) => log("rejection: " + event.reason));
const programs = ${JSON.stringify(programs)};
try {
  const { createCompiler } = await import(${JSON.stringify(kitUrl + "aihc.js")});
  const aihc = await createCompiler(${JSON.stringify(kitUrl + "manifest.json")});
  const result = { version: aihc.version, supported: aihc.supported };
  result.sizeBefore = await aihc.downloadSize();
  await log("check Wrong.hs");
  result.wrong = await aihc.check(programs.wrong);
  await log("check Hello.hs");
  result.checked = await aihc.check(programs.hello);
  const build = async (name) => {
    await log("compile " + name);
    const built = await aihc.compile(programs[name], { onStatus: log, onLog: log });
    if (!built.ok) throw new Error(name + " has problems: " + JSON.stringify(built.diagnostics));
    return built.program;
  };
  const run = async (program, stdin) => {
    let stdout = "", stderr = "";
    const { exitCode } = await aihc.run(program, { stdin, onStdout: (text) => { stdout += text; }, onStderr: (text) => { stderr += text; } });
    return { exitCode, stdout, stderr };
  };
  const hello = await build("hello");
  result.hello = await run(hello);
  // A program can run again.
  result.helloAgain = await run(hello);
  result.echo = await run(await build("echo"), "first line\\nsecond line\\n");
  result.sizeAfter = await aihc.downloadSize();
  await send("/result", { ok: true, result });
} catch (error) {
  await send("/result", { ok: false, message: String(error?.stack ?? error) });
}
</script>
`;

let finish;
const finished = new Promise((resolve) => (finish = resolve));
const pageServer = await listen(async (request, response) => {
  if (request.method === "POST") {
    let body = "";
    for await (const chunk of request) body += chunk;
    response.writeHead(204);
    response.end();
    if (request.url === "/log") console.log("  page: " + String(JSON.parse(body)).trimEnd());
    else if (request.url === "/result") finish(JSON.parse(body));
    return;
  }
  response.writeHead(request.url === "/" ? 200 : 404, { "content-type": "text/html" });
  response.end(request.url === "/" ? page : "");
});
const pageUrl = "http://127.0.0.1:" + pageServer.address().port + "/";

// ---------------------------------------------------------------------------
// The browser

async function findBrowser() {
  if (process.env.AIHC_SMOKE_BROWSER) return process.env.AIHC_SMOKE_BROWSER;
  const root = process.env.AIHC_SMOKE_BROWSERS;
  if (!root) await fail("Set AIHC_SMOKE_BROWSER to a browser, or run the test as nix run .#web-kit-smoke-test.");
  const found = (await listFiles(root)).find((path) => /(^|\/)(chrome|Google Chrome for Testing|Chromium)$/.test(path));
  if (!found) await fail("No Chromium is below " + root + ".");
  return join(root, found);
}

const executable = await findBrowser();
console.log("Browser: " + executable);
console.log("The kit is at " + kitUrl + " and the page is at " + pageUrl);
browser = spawn(
  executable,
  [
    "--headless=new",
    // The sandbox of Chromium needs user namespaces, which a CI runner does
    // not give. The browser loads only the two local servers of this test.
    "--no-sandbox",
    "--disable-gpu",
    "--disable-dev-shm-usage",
    "--no-first-run",
    "--no-default-browser-check",
    "--user-data-dir=" + join(temporary, "profile"),
    pageUrl,
  ],
  { stdio: ["ignore", "ignore", "pipe"] },
);
let browserErrors = "";
browser.stderr.on("data", (chunk) => (browserErrors += chunk));
const outcome = await Promise.race([
  finished,
  new Promise((resolve) => setTimeout(() => resolve({ ok: false, message: "No result after " + timeout / 1000 + " seconds." }), timeout)),
  new Promise((resolve) => browser.on("exit", (code) => resolve({ ok: false, message: "The browser stopped with code " + code + ".\n" + browserErrors }))),
]);
kitServer.close();
pageServer.close();
kitServer.closeAllConnections();
pageServer.closeAllConnections();

if (!outcome.ok) await fail(outcome.message);
const { result } = outcome;
const expect = async (what, actual, expected) => {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) await fail(what + ": got " + JSON.stringify(actual) + ", expected " + JSON.stringify(expected));
  console.log("ok: " + what);
};
await expect("the version is the version of the manifest", result.version, manifest.version);
await expect("the browser has JavaScript Promise Integration", result.supported, true);
await expect("Hello.hs has no problems", { ok: result.checked.ok, diagnostics: result.checked.diagnostics }, { ok: true, diagnostics: [] });
await expect("Wrong.hs has a problem", result.wrong.ok, false);
await expect("the problem of Wrong.hs is on line 4", result.wrong.diagnostics.map((problem) => problem.line), [4]);
await expect("Hello.hs prints its greeting", result.hello, { exitCode: 0, stdout: "Hello from AIHC in the browser!\n", stderr: "" });
await expect("Hello.hs gives the same output on a second run", result.helloAgain, result.hello);
await expect("Echo.hs reads standard input and sets its exit code", result.echo, { exitCode: 1, stdout: "FIRST LINE\nSECOND LINE\n", stderr: "2 lines\n" });
await expect("the browser cache has all the files after the test", result.sizeAfter.missing, 0);
await expect("the download size is the size of the glue, the compiler, and the linker", result.sizeBefore.total, manifest.files.filter((file) => file.group !== "doc").reduce((sum, file) => sum + file.bytes, 0));
await cleanUp();
console.log("The smoke test passed.");
process.exit(0);
