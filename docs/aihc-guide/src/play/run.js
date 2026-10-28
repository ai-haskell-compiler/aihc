import { cachedFetch, manifest } from "../cache.js";

// The browser tools fetch their files with the global fetch. With this
// fetch, the browser keeps the files for the next visit.
globalThis.fetch = cachedFetch;

const encoder = new TextEncoder();
const decoder = new TextDecoder();
let requestId;
const report = (type, text) => postMessage({ id: requestId, type, text });
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
  const [helper, bundle] = await Promise.all([
    import("../compiler.js"),
    inflated("core.bundle.gz").then((response) => response.arrayBuffer()),
  ]);
  helper.run.run().then(() => reject(new Error("The compiler stopped.")), reject);
  const loaded = await ask("load-bundle", new Uint8Array(bundle));
  if (!loaded.ok) throw new Error(loaded.message);
  return (source) => ask("compile", encoder.encode(source));
}

// Uint8Array.fromBase64 decodes the runtime in milliseconds. The fallback
// takes about one second. Chrome 137 to 139 have JSPI but no fromBase64.
const fromBase64 = (text) => Uint8Array.fromBase64
  ? Uint8Array.fromBase64(text)
  : Uint8Array.from(atob(text), (character) => character.charCodeAt(0));

async function loadTools() {
  return import((await manifest()).url);
}

async function loadRuntime() {
  const runtime = await (await inflated("runtime.json.gz")).json();
  return {
    files: Object.fromEntries(Object.entries(runtime.files).map(([name, value]) => [name, fromBase64(value)])),
    metadata: fromBase64(runtime.metadata),
  };
}

// The worker does one build at a time, in the order of the requests.
// Program execution has a separate worker.
let queue = Promise.resolve();
onmessage = ({ data }) => {
  queue = queue.then(() => build(data));
};

// A warm request builds the program and keeps only the warm state. The first
// compile in a new worker is about five times slower than the next ones,
// mostly because the WebAssembly engine optimizes the compiler while it runs. The
// page sends a warm request when the playground comes into view.
async function build({ id, source, warm }) {
  requestId = id;
  try {
    if (typeof WebAssembly.Suspending !== "function") throw new Error("This browser needs JavaScript Promise Integration.");
    // The tools and the runtime download while the compiler loads and runs.
    toolsPromise ??= loadTools();
    runtimePromise ??= loadRuntime();
    toolsPromise.catch(() => {});
    runtimePromise.catch(() => {});
    report("status", "Load the compiler.");
    const compile = await (compilePromise ??= compiler());
    report("status", "Compile the module.");
    const answer = await compile(source);
    if (!answer.ok) throw new Error(answer.message);
    if (answer.errors?.length) {
      postMessage({ id, type: "diagnostics", errors: answer.errors });
      return;
    }
    report("status", "Assemble and link the program.");
    const { commands, componentNew, transpile } = await toolsPromise;
    const runtime = await runtimePromise;
    // Each build gets a new file map. Cached library bytes remain in this worker.
    let files = { ...runtime.files };
    const objects = Object.keys(files).filter((name) => name.endsWith(".o")).sort();
    const libraries = Object.keys(files).filter((name) => name.endsWith(".a") && name !== "libc.a" && name !== "libclang_rt.builtins.a").sort();
    const options = { stdout: () => {}, stderr: (bytes) => { if (bytes) report("tool-stderr", decoder.decode(bytes)); } };
    for (const [index, assembly] of answer.assembly.entries()) {
      const name = "module" + index;
      files[name + ".s"] = assembly;
      files = await commands.clang(["--target=wasm32-wasip3", "-mtail-call", "-mmultivalue", "-mreference-types", "-msign-ext", "-c", name + ".s", "-o", name + ".o"], files, options);
      objects.push(name + ".o");
    }
    files = await commands["wasm-ld"]([
      "--no-entry", "--export-memory", "--extra-features=atomics", "--wrap=chmod", "--wrap=fchmod",
      ...objects, ...libraries, "libc.a", "libclang_rt.builtins.a", "-o", "program.wasm",
    ], files, options);
    const core = files["program.wasm"];
    const module = new Uint8Array(core.length + runtime.metadata.length);
    module.set(core);
    module.set(runtime.metadata, core.length);
    const component = await componentNew(module, []);
    report("status", "Prepare the P3 program.");
    const result = await transpile(component, {
      name: "program", instantiation: { tag: "async" }, noNodejsCompat: true, noTypescript: true,
      asyncMode: { tag: "jspi", val: { imports: [], exports: [] } },
    });
    if (warm) {
      postMessage({ id, type: "warm" });
      return;
    }
    // Only the new program leaves this worker. Do not transfer cached inputs.
    const transfers = [...new Set(result.files.map(([, bytes]) => bytes.buffer))];
    postMessage({ id, type: "program", files: result.files }, transfers);
  } catch (error) {
    report("error", String(error?.message ?? error));
  }
}
