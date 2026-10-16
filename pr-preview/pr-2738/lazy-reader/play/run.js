import { transpile } from "@bytecodealliance/jco/component";
import { $init, tools } from "play-wasm-tools";
import { commands } from "@yowasp/clang";
import * as host from "./host.js";

const encoder = new TextEncoder();
const decoder = new TextDecoder();
const report = (type, text) => postMessage({ type, text });

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

const fromBase64 = (text) => Uint8Array.from(atob(text), (character) => character.charCodeAt(0));

onmessage = async ({ data: source }) => {
  try {
    if (typeof WebAssembly.Suspending !== "function") throw new Error("This browser needs JavaScript Promise Integration.");
    report("status", "Load the compiler.");
    const compile = await compiler();
    report("status", "Compile the module.");
    const answer = await compile(source);
    if (!answer.ok) throw new Error(answer.message);
    if (answer.errors?.length) {
      postMessage({ type: "diagnostics", errors: answer.errors });
      return;
    }
    report("status", "Assemble and link the program.");
    const runtime = await (await inflated("runtime.json.gz")).json();
    let files = Object.fromEntries(Object.entries(runtime.files).map(([name, value]) => [name, fromBase64(value)]));
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
    const metadata = fromBase64(runtime.metadata);
    const module = new Uint8Array(core.length + metadata.length);
    module.set(core);
    module.set(metadata, core.length);
    await $init;
    const component = tools.componentNew(module, []);
    report("status", "Prepare the P3 program.");
    const result = await transpile(component, {
      name: "program", instantiation: { tag: "async" }, noNodejsCompat: true, noTypescript: true,
      asyncMode: { tag: "jspi", val: { imports: [], exports: [] } },
    });
    const outputs = new Map(result.files);
    const url = URL.createObjectURL(new Blob([outputs.get("program.js")], { type: "text/javascript" }));
    try {
      const { instantiate } = await import(url);
      globalThis.lazyReaderHost = {
        nextInput: async () => new Uint8Array(),
        write: (bytes) => report("stdout", decoder.decode(bytes, { stream: true })),
        writeErr: (bytes) => report("stderr", decoder.decode(bytes)),
      };
      const instance = await instantiate((name) => WebAssembly.compile(outputs.get(name)), host.imports);
      report("status", "Run the program.");
      await instance.run.run();
      report("done", "The program finished.");
    } finally { URL.revokeObjectURL(url); }
  } catch (error) {
    report("error", String(error?.message ?? error));
  }
};
