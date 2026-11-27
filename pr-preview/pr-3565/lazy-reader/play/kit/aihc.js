// The entry module of the AIHC web kit.
//
//   import { createCompiler } from "https://assets.aihc.app/play/releases/VERSION/aihc.js";
//   const aihc = await createCompiler("https://assets.aihc.app/play/releases/VERSION/manifest.json");
//   const built = await aihc.compile(source);
//   if (built.ok) await aihc.run(built.program, { onStdout: (text) => show(text) });
//
// The README of the kit gives the full interface.
import { createDownloads, loadManifest } from "./manifest.js";

const WORKER = new URL("./worker.js", import.meta.url);

// The compiler and each compiled program need JavaScript Promise Integration.
export const isSupported = () => typeof WebAssembly !== "undefined" && typeof WebAssembly.Suspending === "function";

// A browser starts a worker only from a script of the same origin as the
// page. When the kit is on a different origin, the worker starts from a blob
// that imports the worker module of the kit. A page that forbids blob
// workers gives the URL of its own copy of that one import as workerUrl.
function startWorker(workerUrl) {
  if (workerUrl) return new Worker(new URL(workerUrl, globalThis.location.href), { type: "module" });
  if (WORKER.origin === globalThis.location.origin) return new Worker(WORKER, { type: "module" });
  const shim = URL.createObjectURL(new Blob(["import " + JSON.stringify(WORKER.href) + ";\n"], { type: "text/javascript" }));
  try {
    return new Worker(shim, { type: "module" });
  } finally {
    // The worker starts its fetch of the blob before this task ends.
    setTimeout(() => URL.revokeObjectURL(shim), 60000);
  }
}

const aborted = (message) => new DOMException(message, "AbortError");

export async function createCompiler(manifestUrl, { workerUrl } = {}) {
  const manifest = await loadManifest(manifestUrl);
  const downloads = createDownloads(manifest);
  let worker = null;
  let nextId = 1;
  const pending = new Map();

  const failAll = (error) => {
    for (const { reject } of pending.values()) reject(error);
    pending.clear();
  };

  function compilerWorker() {
    if (worker) return worker;
    const started = startWorker(workerUrl);
    started.onmessage = ({ data }) => {
      if (started !== worker) return;
      if (data.type === "fatal") {
        // The compiler stopped. The next request starts a new worker.
        worker = null;
        started.terminate();
        failAll(new Error(data.text));
        return;
      }
      const request = pending.get(data.id);
      if (!request) return;
      if (data.type === "status") request.onStatus?.(data.text);
      else if (data.type === "log") request.onLog?.(data.text);
      else {
        pending.delete(data.id);
        if (data.type === "result") request.resolve(data.value);
        else request.reject(new Error(data.text));
      }
    };
    started.onerror = (event) => {
      if (started !== worker) return;
      event.preventDefault?.();
      worker = null;
      started.terminate();
      failAll(new Error(event.message || "The compiler worker failed."));
    };
    started.postMessage({ role: "compiler", manifest });
    worker = started;
    return worker;
  }

  const send = (message, { onStatus, onLog } = {}) =>
    new Promise((resolve, reject) => {
      if (!isSupported()) return reject(new Error("This browser needs JavaScript Promise Integration."));
      const id = nextId++;
      pending.set(id, { resolve, reject, onStatus, onLog });
      compilerWorker().postMessage({ id, ...message });
    });

  // Download and start the front end of the compiler.
  const load = () => send({ op: "load" });

  // Send one command of the helper protocol: parse, names, types, or
  // compile. The answer is the JSON value of the helper.
  const request = (command, source) => send({ op: "request", command, source });

  // Type-check one module.
  async function check(source) {
    const answer = await request("types", source);
    if (!answer.ok) throw new Error(answer.message);
    return { ok: answer.errors.length === 0, diagnostics: answer.errors, signatures: answer.signatures };
  }

  // Compile and link one module that has a main function.
  async function compile(source, options) {
    const result = await send({ op: "compile", source }, options);
    if (result.diagnostics) return { ok: false, diagnostics: result.diagnostics };
    return { ok: true, diagnostics: [], program: { files: result.files } };
  }

  // The first build in a new worker is slow, mostly because the WebAssembly
  // engine optimizes the compiler while it runs. This builds a module and
  // keeps only the warm worker.
  const warm = (source = "main :: IO ()\nmain = pure ()\n") => send({ op: "compile", source, warm: true }).then(() => {});

  // Run a program in a worker of its own. The promise resolves with the
  // exit code. An abort of the signal stops the program.
  function run(program, { stdin = "", onStdout, onStderr, signal } = {}) {
    return new Promise((resolve, reject) => {
      if (signal?.aborted) return reject(aborted("The program did not start."));
      const runner = startWorker(workerUrl);
      const finish = (settle, value) => {
        runner.terminate();
        signal?.removeEventListener("abort", abort);
        settle(value);
      };
      const abort = () => finish(reject, aborted("The program stopped."));
      signal?.addEventListener("abort", abort);
      runner.onmessage = ({ data }) => {
        if (data.type === "stdout") onStdout?.(data.text);
        else if (data.type === "stderr") onStderr?.(data.text);
        else if (data.type === "exit") finish(resolve, { exitCode: data.code });
        else if (data.type === "error") finish(reject, new Error(data.text));
      };
      runner.onerror = (event) => {
        event.preventDefault?.();
        finish(reject, new Error(event.message || "The program worker failed."));
      };
      // The program stays with the caller, so the caller can run it again.
      runner.postMessage({ role: "program", files: program.files, stdin: typeof stdin === "string" ? new TextEncoder().encode(stdin) : stdin });
    });
  }

  // Stop the compiler worker. A compile that runs is synchronous in the
  // worker, so this is the only method to stop it. Each request that waits
  // fails with an AbortError. The next request starts a new worker.
  function terminate() {
    worker?.terminate();
    worker = null;
    failAll(aborted("The compiler stopped."));
  }

  return {
    version: manifest.version,
    build: manifest.build,
    manifest,
    supported: isSupported(),
    downloadSize: downloads.downloadSize,
    load,
    check,
    compile,
    warm,
    run,
    request,
    terminate,
  };
}
