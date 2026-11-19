// The browser host of a WASI 0.3 component.
//
// jco turns a component into a JavaScript module that imports the WASI
// interfaces. createImports gives these interfaces. The component gets one
// input stream and two output streams and nothing else: no files, no clock
// of its own, no network.
//
// The caller supplies the streams:
//   { nextInput(): Promise<Uint8Array>, write(bytes), writeErr(bytes) }
// nextInput resolves with the next input bytes. An empty array is the end
// of the input. The component reads one chunk per read request.

export class ProgramExit extends Error {
  constructor(code) {
    super("The program exited with code " + code + ".");
    this.name = "ProgramExit";
    this.code = code;
  }
}

function toBytes(value) {
  if (value instanceof Uint8Array) return value;
  if (Array.isArray(value)) return Uint8Array.from(value);
  if (typeof value === "number") return Uint8Array.of(value);
  return new TextEncoder().encode(String(value));
}

async function drain(data, sink) {
  if (typeof data.read === "function") {
    for (;;) {
      const value = await data.read({ count: 65536 });
      if (value === null || value === undefined) break;
      if (value && typeof value === "object" && "done" in value) {
        if (value.done) break;
        sink(value.value);
      } else {
        sink(value);
      }
    }
  } else {
    for await (const chunk of data) sink(chunk);
  }
}

class TerminalInput {}
class TerminalOutput {}
class Descriptor {}
class Fields {}
class Request {}
class RequestOptions {}
class Response {}

export function createImports(host) {
  const start = performance.now();
  const nowNs = () => BigInt(Math.round((performance.now() - start) * 1e6));
  const output = (sink) => ({
    writeViaStream: async (data) => {
      await drain(data, (value) => sink(toBytes(value)));
      return { tag: "ok", val: undefined };
    },
  });
  return {
    "wasi:cli/environment": { getArguments: () => host.arguments ?? ["program"], getInitialCwd: () => undefined },
    "wasi:cli/exit": {
      exit: (status) => { throw new ProgramExit(status.tag === "ok" ? 0 : 1); },
      exitWithCode: (code) => { throw new ProgramExit(code); },
    },
    "wasi:cli/types": {},
    "wasi:cli/stdin": {
      // One stream per read. The stream gives one chunk and then ends. The
      // component reads the chunk and drops the stream itself. A stream that
      // ends with no chunk is the end of the input.
      readViaStream: () => {
        async function* source() {
          const chunk = await host.nextInput();
          if (chunk.length > 0) yield chunk;
        }
        return [source(), Promise.resolve({ tag: "ok", val: undefined })];
      },
    },
    "wasi:cli/stdout": output((bytes) => host.write(bytes)),
    "wasi:cli/stderr": output((bytes) => host.writeErr(bytes)),
    "wasi:cli/terminal-input": { TerminalInput },
    "wasi:cli/terminal-output": { TerminalOutput },
    "wasi:cli/terminal-stdin": { getTerminalStdin: () => undefined },
    "wasi:cli/terminal-stdout": { getTerminalStdout: () => undefined },
    "wasi:cli/terminal-stderr": { getTerminalStderr: () => undefined },
    "wasi:clocks/types": {},
    "wasi:clocks/system-clock": {},
    "wasi:clocks/monotonic-clock": {
      now: nowNs,
      waitFor: (ns) => new Promise((resolve) => setTimeout(resolve, Number(ns) / 1e6)),
      waitUntil: (ns) => new Promise((resolve) => setTimeout(resolve, Math.max(0, Number(ns - nowNs()) / 1e6))),
    },
    "wasi:filesystem/types": { Descriptor },
    "wasi:filesystem/preopens": { getDirectories: () => [] },
    "wasi:http/types": { Fields, Request, RequestOptions, Response },
    "wasi:http/client": {
      send: () => {
        throw new Error("wasi:http is not available in the browser host");
      },
    },
  };
}
