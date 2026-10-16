import { measure } from "./timing.js";

const encoder = new TextEncoder();
const decoder = new TextDecoder();
let requestId;
const report = (type, text) => postMessage({ id: requestId, type, text });
const time = (stage, action, reused = false) => measure(stage, action, (result) => postMessage({ id: requestId, type: "timing", ...result }), reused);
let compilePromise, toolsPromise, runtimePromise;

async function inflated(path) {
  const response = await fetch(new URL(path, import.meta.url));
  if (!response.ok) throw new Error("Cannot load " + path + ": " + response.status);
  return new Response(response.body.pipeThrough(new DecompressionStream("gzip")));
}

async function compiler() {
  const inputs = [];
  const waiters = [];
  let pending;
  let buffer = "";
  let stopped;
  const push = (bytes) => waiters.length ? waiters.shift()(bytes) : inputs.push(bytes);
  const reject = (error) => { stopped = error; pending?.reject(error); };
  globalThis.lazyReaderHost = {
    nextInput: () => inputs.length ? Promise.resolve(inputs.shift()) : new Promise((resolve) => waiters.push(resolve)),
    write: (bytes) => {
      buffer += decoder.decode(bytes, { stream: true });
      let newline;
      while ((newline = buffer.indexOf("\n")) >= 0) {
        const line = buffer.slice(0, newline);
        buffer = buffer.slice(newline + 1);
        try { pending?.resolve(JSON.parse(line)); } catch (error) { reject(error); }
        pending = null;
      }
    },
    writeErr: (bytes) => report("compiler-stderr", decoder.decode(bytes)),
  };
  const ask = (command, bytes) => new Promise((resolve, reject) => {
    if (stopped) return reject(stopped);
    pending = { resolve, reject };
    const header = encoder.encode(command + " " + bytes.length + "\n");
    if (bytes.length >= 8192) {
      push(header);
      push(bytes);
    } else {
      const frame = new Uint8Array(header.length + bytes.length);
      frame.set(header);
      frame.set(bytes, header.length);
      for (let offset = 0; offset < frame.length; offset += 4096) push(frame.subarray(offset, offset + 4096));
    }
  });
  const [helper, bundle] = await time("Compiler load and initialization", () => Promise.all([
    import("../compiler.js"),
    inflated("core.bundle.gz").then((response) => response.arrayBuffer()),
  ]));
  helper.run.run().then(() => reject(new Error("The compiler stopped.")), reject);
  const loaded = await time("Core interface decode", () => ask("load-bundle", new Uint8Array(bundle)));
  if (!loaded.ok) throw new Error(loaded.message);
  return (source) => ask("compile", encoder.encode(source));
}

const fromBase64 = (text) => Uint8Array.from(atob(text), (character) => character.charCodeAt(0));

async function loadTools() {
  const response = await fetch(new URL("tools.json", import.meta.url));
  if (!response.ok) throw new Error("Cannot load the tool configuration: " + response.status);
  const config = await response.json();
  return import(new URL(config.url, import.meta.url).href);
}

async function loadRuntime() {
  const runtime = await (await inflated("runtime.json.gz")).json();
  return {
    files: Object.fromEntries(Object.entries(runtime.files).map(([name, value]) => [name, fromBase64(value)])),
    metadata: fromBase64(runtime.metadata),
  };
}

// The page sends one build at a time. Program execution has a separate worker.
onmessage = async ({ data: { id, source } }) => {
  requestId = id;
  try {
    if (typeof WebAssembly.Suspending !== "function") throw new Error("This browser needs JavaScript Promise Integration.");
    report("status", "Load the compiler.");
    if (compilePromise) {
      await time("Compiler load and initialization", () => compilePromise, true);
      await time("Core interface decode", () => {}, true);
    } else {
      compilePromise = compiler();
    }
    const compile = await compilePromise;
    report("status", "Compile the module.");
    const answer = await time("Haskell compilation", () => compile(source));
    if (!answer.ok) throw new Error(answer.message);
    if (answer.errors?.length) {
      postMessage({ id, type: "diagnostics", errors: answer.errors });
      return;
    }
    report("status", "Assemble and link the program.");
    const toolsReused = !!toolsPromise;
    const { commands, componentNew, transpile } = await time("Tool import", () => toolsPromise ??= loadTools(), toolsReused);
    const runtimeReused = !!runtimePromise;
    const runtime = await time("Runtime load and decode", () => runtimePromise ??= loadRuntime(), runtimeReused);
    // Each build gets a new file map. Cached library bytes remain in this worker.
    let files = { ...runtime.files };
    const objects = Object.keys(files).filter((name) => name.endsWith(".o")).sort();
    const libraries = Object.keys(files).filter((name) => name.endsWith(".a") && name !== "libc.a" && name !== "libclang_rt.builtins.a").sort();
    const options = { stdout: () => {}, stderr: (bytes) => { if (bytes) report("tool-stderr", decoder.decode(bytes)); } };
    await time("Assembly", async () => {
      for (const [index, assembly] of answer.assembly.entries()) {
        const name = "module" + index;
        files[name + ".s"] = assembly;
        files = await commands.clang(["--target=wasm32-wasip3", "-mtail-call", "-mmultivalue", "-mreference-types", "-msign-ext", "-c", name + ".s", "-o", name + ".o"], files, options);
        objects.push(name + ".o");
      }
    });
    files = await time("Link", () => commands["wasm-ld"]([
      "--no-entry", "--export-memory", "--extra-features=atomics", "--wrap=chmod", "--wrap=fchmod",
      ...objects, ...libraries, "libc.a", "libclang_rt.builtins.a", "-o", "program.wasm",
    ], files, options));
    const core = files["program.wasm"];
    const module = new Uint8Array(core.length + runtime.metadata.length);
    module.set(core);
    module.set(runtime.metadata, core.length);
    const component = await time("Component creation", () => componentNew(module, []));
    report("status", "Prepare the P3 program.");
    const result = await time("jco conversion", () => transpile(component, {
      name: "program", instantiation: { tag: "async" }, noNodejsCompat: true, noTypescript: true,
      asyncMode: { tag: "jspi", val: { imports: [], exports: [] } },
    }));
    // Only the new program leaves this worker. Do not transfer cached inputs.
    const transfers = [...new Set(result.files.map(([, bytes]) => bytes.buffer))];
    postMessage({ id, type: "program", files: result.files }, transfers);
  } catch (error) {
    report("error", String(error?.message ?? error));
  }
};
