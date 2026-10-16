import { imports } from "./host.js";
import { measure } from "./timing.js";

// Each program has its own host, memory, and lifetime.
onmessage = async ({ data: { id, files } }) => {
  const report = (type, text) => postMessage({ id, type, text });
  const time = (stage, action) => measure(stage, action, (result) => postMessage({ id, type: "timing", ...result }));
  const decoder = new TextDecoder();
  const outputs = new Map(files);
  const url = URL.createObjectURL(new Blob([outputs.get("program.js")], { type: "text/javascript" }));
  try {
    const instance = await time("Program initialization", async () => {
      const { instantiate } = await import(url);
      globalThis.lazyReaderHost = {
        nextInput: async () => new Uint8Array(),
        write: (bytes) => report("stdout", decoder.decode(bytes, { stream: true })),
        writeErr: (bytes) => report("stderr", decoder.decode(bytes)),
      };
      return instantiate((name) => WebAssembly.compile(outputs.get(name)), imports);
    });
    report("status", "Run the program.");
    await time("Program execution", () => instance.run.run());
    report("done", "The program finished.");
  } catch (error) {
    if (error?.name === "ProgramExit") report(error.code === 0 ? "done" : "error", error.message);
    else report("error", String(error?.message ?? error));
  } finally {
    URL.revokeObjectURL(url);
  }
};
