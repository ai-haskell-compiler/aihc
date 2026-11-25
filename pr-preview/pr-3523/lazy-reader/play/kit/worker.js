// The worker of the web kit. aihc.js starts it and sends it messages.
//
// The first message gives the worker one of two roles:
//   { role: "compiler", manifest }       The compiler, the assembler, the
//                                        linker, and the engine that runs
//                                        Template Haskell splices.
//   { role: "program", files, stdin }    One compiled program.
// A compiler worker does one request at a time, in the order of the
// requests. Each program has its own worker, memory, and lifetime.
import { createImports, quickTimers } from "./host.js";
import { createDownloads, decodePack } from "./manifest.js";

const encoder = new TextEncoder();

// A component reads its input through a buffer of 8192 bytes. A piece that
// is larger than the read of the component loses its tail in the stream of
// the host. Thus the host gives the input in pieces of 4096 bytes.
const INPUT_CHUNK = 4096;
// The helper reads a request body of at least the buffer size in one read
// of the whole body. Thus the host gives such a body in one piece.
const DIRECT_READ = 8192;

onmessage = ({ data }) => {
  if (data.role === "compiler") onmessage = compilerRole(data.manifest);
  else if (data.role === "program") runProgram(data);
};

// ---------------------------------------------------------------------------
// The compiler

function compilerRole(manifest) {
  const downloads = createDownloads(manifest);
  // The browser tools fetch their files with the global fetch. With this
  // fetch, the browser keeps the files for the next visit.
  globalThis.fetch = downloads.fetch;
  let helperPromise, toolsPromise, runtimePromise, imagePromise;
  let current;
  const report = (type, text) => postMessage({ id: current, type, text });

  async function startHelper() {
    const inputs = [];
    const waiters = [];
    const decoder = new TextDecoder();
    let pending, stopped;
    let buffer = "";
    const push = (bytes) => (waiters.length ? waiters.shift()(bytes) : inputs.push(bytes));
    // After a stop, the helper cannot answer. The page starts a new worker.
    const stop = (error) => {
      stopped = error;
      pending?.reject(error);
      postMessage({ type: "fatal", text: String(error?.message ?? error) });
    };
    const host = {
      arguments: ["aihc-play"],
      nextInput: () => (inputs.length ? Promise.resolve(inputs.shift()) : new Promise((resolve) => waiters.push(resolve))),
      write: (bytes) => {
        buffer += decoder.decode(bytes, { stream: true });
        let newline;
        while ((newline = buffer.indexOf("\n")) >= 0) {
          const line = buffer.slice(0, newline);
          buffer = buffer.slice(newline + 1);
          let answer;
          try {
            answer = JSON.parse(line);
          } catch {
            const waiter = pending;
            pending = null;
            waiter?.reject(new Error("The compiler gave an answer that is not JSON: " + line.slice(0, 200)));
            continue;
          }
          // A line with the field engine is for the engine of the splices.
          if (typeof answer?.engine === "string") {
            splices(answer);
            continue;
          }
          const waiter = pending;
          pending = null;
          waiter?.resolve(answer);
        }
      },
      writeErr: (bytes) => report("log", new TextDecoder().decode(bytes)),
    };
    // Give one line to the helper, for example a reply of the engine.
    const toHelper = (text) => {
      const bytes = encoder.encode(text.replace(/\n/g, " ") + "\n");
      for (let offset = 0; offset < bytes.length; offset += INPUT_CHUNK) push(bytes.subarray(offset, offset + INPUT_CHUNK));
    };
    const splices = spliceRelay(toHelper);
    // Give a request to the helper: the header line, then the body.
    const ask = (command, body) =>
      new Promise((resolve, reject) => {
        if (stopped) return reject(stopped);
        pending = { resolve, reject };
        const header = encoder.encode(command + " " + body.length + "\n");
        if (body.length >= DIRECT_READ) {
          push(header);
          push(body);
        } else {
          const frame = new Uint8Array(header.length + body.length);
          frame.set(header);
          frame.set(body, header.length);
          for (let offset = 0; offset < frame.length; offset += INPUT_CHUNK) push(frame.subarray(offset, offset + INPUT_CHUNK));
        }
      });
    const [{ instantiate }, bundle] = await Promise.all([
      import(downloads.file(manifest.compiler.module).href),
      downloads.inflated(manifest.compiler.bundle),
    ]);
    const prefix = manifest.compiler.module.slice(0, manifest.compiler.module.lastIndexOf("/") + 1);
    const instance = await instantiate(async (name) => WebAssembly.compile(await downloads.inflated(prefix + name + ".gz")), createImports(host));
    // The helper runs until the worker stops. Its promise settles only on a
    // failure.
    instance.run.run().then(() => stop(new Error("The compiler stopped.")), stop);
    const loaded = await ask("load-bundle", new Uint8Array(bundle));
    if (!loaded.ok) throw new Error(loaded.message);
    return (command, source) => ask(command, encoder.encode(source));
  }

  const helper = () => {
    helperPromise ??= startHelper();
    return helperPromise;
  };

  // The assembler and the linker download while the compiler loads and runs.
  function startLinker() {
    toolsPromise ??= import(downloads.file(manifest.linker.tools).href);
    runtimePromise ??= downloads.inflated(manifest.linker.runtime).then(decodePack);
    toolsPromise.catch(() => {});
    runtimePromise.catch(() => {});
  }

  // The image of the engine, compiled one time.
  const spliceImage = () => {
    imagePromise ??= downloads.inflated(manifest.splices.image).then((bytes) => WebAssembly.compile(bytes));
    return imagePromise;
  };

  // The relay between the helper and the engine of the splices. The helper
  // writes the messages of Main.hs of bin/aihc-play. The engine runs in
  // this worker: while the engine runs a splice, the helper waits for its
  // next input, and JavaScript Promise Integration suspends the two.
  function spliceRelay(toHelper) {
    let engine = null;
    let queue = Promise.resolve();
    const oneLine = (error) => String(error?.message ?? error).replace(/\s+/g, " ");
    async function open() {
      const [{ createEngine }, image] = await Promise.all([import("./engine-core.js"), spliceImage()]);
      const lines = [];
      const waiters = [];
      const modules = new Map();
      const current = {
        modules,
        next: 0,
        push: (line) => (waiters.length ? waiters.shift()(line) : lines.push(line)),
      };
      const core = createEngine({
        imageBytes: image,
        async: true,
        transport: {
          readLine: () => (lines.length ? Promise.resolve(lines.shift()) : new Promise((resolve) => waiters.push(resolve))),
          write: toHelper,
        },
        readModule: (id) => {
          const bytes = modules.get(id);
          modules.delete(id);
          if (!bytes) throw new Error("no shared module " + id);
          return bytes;
        },
      });
      engine = current;
      // serve writes "ok" first, and then the reply of each command.
      core.serve().catch((error) => toHelper("error " + oneLine(error))).finally(() => {
        if (engine === current) engine = null;
      });
    }
    // Assemble and link a splice module as a shared module, and load it.
    async function load(assembly) {
      if (!engine) throw new Error("the engine is not open");
      startLinker();
      const { commands } = await toolsPromise;
      const decoder = new TextDecoder();
      const options = { stdout: () => {}, stderr: (bytes) => { if (bytes) report("log", decoder.decode(bytes)); } };
      let files = { "splices.s": assembly };
      files = await commands.clang(["--target=wasm32-wasip3", "-mtail-call", "-mmultivalue", "-mreference-types", "-msign-ext", "-c", "splices.s", "-o", "splices.o"], files, options);
      files = await commands["wasm-ld"](["-shared", "--experimental-pic", "--unresolved-symbols=import-dynamic", "--extra-features=atomics", "--no-entry", "splices.o", "-o", "splices.wasm"], files, options);
      if (!files["splices.wasm"]) throw new Error("the linker made no shared module");
      const id = String(engine.next++);
      engine.modules.set(id, files["splices.wasm"]);
      engine.push("load " + id);
    }
    function handle(message) {
      if (message.engine === "open") return open();
      if (message.engine === "load") return load(message.assembly);
      if (message.engine === "line") {
        if (!engine) throw new Error("the engine is not open");
        engine.push(message.line);
      } else if (message.engine === "close") {
        engine?.push(null);
        engine = null;
      } else {
        throw new Error("unknown engine message " + message.engine);
      }
    }
    // The messages are in order, also when one of them waits.
    return (message) => {
      queue = queue.then(() => handle(message)).catch((error) => toHelper("error " + oneLine(error)));
    };
  }

  // Compile, assemble, and link one module. The result is the files of a
  // JavaScript program, or the problems of the module.
  async function compile(source, warm) {
    startLinker();
    report("status", "Load the compiler.");
    const ask = await helper();
    report("status", "Compile the module.");
    const answer = await ask("compile", source);
    if (!answer.ok) throw new Error(answer.message);
    if (answer.errors?.length) return { diagnostics: answer.errors };
    report("status", "Assemble and link the program.");
    const { commands, componentNew, transpile } = await toolsPromise;
    const { "component-metadata": metadata, ...runtime } = await runtimePromise;
    // Each build gets a new file map. The library bytes stay in this worker.
    let files = { ...runtime };
    const objects = Object.keys(files).filter((name) => name.endsWith(".o")).sort();
    const libraries = Object.keys(files).filter((name) => name.endsWith(".a") && name !== "libc.a" && name !== "libclang_rt.builtins.a").sort();
    const decoder = new TextDecoder();
    const options = { stdout: () => {}, stderr: (bytes) => { if (bytes) report("log", decoder.decode(bytes)); } };
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
    const module = new Uint8Array(core.length + metadata.length);
    module.set(core);
    module.set(metadata, core.length);
    const component = await componentNew(module, []);
    report("status", "Prepare the program.");
    const result = await transpile(component, {
      name: "program", instantiation: { tag: "async" }, noNodejsCompat: true, noTypescript: true,
      asyncMode: { tag: "jspi", val: { imports: [], exports: [] } },
    });
    // A warm request keeps only the warm state of the worker.
    return warm ? { warm: true } : { files: result.files };
  }

  async function handle({ id, op, command, source, warm }) {
    current = id;
    try {
      if (typeof WebAssembly.Suspending !== "function") throw new Error("This browser needs JavaScript Promise Integration.");
      let value;
      if (op === "load") await helper();
      else if (op === "request") value = await (await helper())(command, source);
      else if (op === "compile") value = await compile(source, warm);
      else throw new Error("Unknown operation: " + op);
      // Only a new program leaves this worker. Do not transfer cached inputs.
      const transfers = value?.files ? [...new Set(value.files.map(([, bytes]) => bytes.buffer))] : [];
      postMessage({ id, type: "result", value }, transfers);
    } catch (error) {
      postMessage({ id, type: "error", text: String(error?.message ?? error) });
    }
  }

  let queue = Promise.resolve();
  return ({ data }) => {
    queue = queue.then(() => handle(data));
  };
}

// ---------------------------------------------------------------------------
// One program

async function runProgram({ files, stdin }) {
  const outputs = new Map(files);
  const input = [];
  for (let offset = 0; offset < stdin.length; offset += INPUT_CHUNK) input.push(stdin.subarray(offset, offset + INPUT_CHUNK));
  const out = new TextDecoder();
  const err = new TextDecoder();
  const host = {
    // This worker runs only the program, so its short timers can run at once.
    wake: quickTimers(),
    nextInput: async () => input.shift() ?? new Uint8Array(),
    write: (bytes) => postMessage({ type: "stdout", text: out.decode(bytes, { stream: true }) }),
    writeErr: (bytes) => postMessage({ type: "stderr", text: err.decode(bytes, { stream: true }) }),
  };
  const url = URL.createObjectURL(new Blob([outputs.get("program.js")], { type: "text/javascript" }));
  try {
    const { instantiate } = await import(url);
    const instance = await instantiate((name) => WebAssembly.compile(outputs.get(name)), createImports(host));
    postMessage({ type: "started" });
    await instance.run.run();
    postMessage({ type: "exit", code: 0 });
  } catch (error) {
    if (error?.name === "ProgramExit") postMessage({ type: "exit", code: error.code });
    else postMessage({ type: "error", text: String(error?.message ?? error) });
  } finally {
    URL.revokeObjectURL(url);
  }
}
