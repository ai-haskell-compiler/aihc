// The browser host of the playground helper.
//
// The helper is a WASI 0.3 component. jco turns it into a JavaScript module
// that imports the WASI interfaces from this file. The host gives the
// helper one input stream and two output streams and nothing else: no
// files, no clock of its own, no network.
//
// The page sets globalThis.lazyReaderHost before it starts the helper:
//   { nextInput(): Promise<Uint8Array>, write(bytes), writeErr(bytes) }
// nextInput resolves with the next request bytes. The helper reads one
// chunk per read request, so the host keeps the queue.

const host = () => globalThis.lazyReaderHost;

export const environment = {
  getArguments: () => ["aihc-play"],
  getInitialCwd: () => undefined,
};

export const types = {};

export class ProgramExit extends Error {
  constructor(code) {
    super("The program exited with code " + code + ".");
    this.name = "ProgramExit";
    this.code = code;
  }
}

export const exit = {
  exit: (status) => { throw new ProgramExit(status.tag === "ok" ? 0 : 1); },
  exitWithCode: (code) => { throw new ProgramExit(code); },
};

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

function toBytes(value) {
  if (value instanceof Uint8Array) return value;
  if (Array.isArray(value)) return Uint8Array.from(value);
  if (typeof value === "number") return Uint8Array.of(value);
  return new TextEncoder().encode(String(value));
}

export const stdin = {
  // One stream per read. The stream gives one chunk and then ends; the
  // helper reads the chunk and drops the stream itself. A stream that ends
  // with no chunk is the end of the input, so the source waits for a
  // request instead of ending early.
  readViaStream: () => {
    async function* source() {
      const chunk = await host().nextInput();
      if (chunk.length > 0) yield chunk;
    }
    return [source(), Promise.resolve({ tag: "ok", val: undefined })];
  },
};

export const stdout = {
  writeViaStream: async (data) => {
    await drain(data, (value) => host().write(toBytes(value)));
    return { tag: "ok", val: undefined };
  },
};

export const stderr = {
  writeViaStream: async (data) => {
    await drain(data, (value) => host().writeErr(toBytes(value)));
    return { tag: "ok", val: undefined };
  },
};

export class TerminalInput {}
export class TerminalOutput {}
export const terminalInput = { TerminalInput };
export const terminalOutput = { TerminalOutput };
export const terminalStdin = { getTerminalStdin: () => undefined };
export const terminalStdout = { getTerminalStdout: () => undefined };
export const terminalStderr = { getTerminalStderr: () => undefined };

const start = typeof performance !== "undefined" ? performance.now() : Date.now();
const nowNs = () =>
  BigInt(Math.round(((typeof performance !== "undefined" ? performance.now() : Date.now()) - start) * 1e6));
export const clocksTypes = {};
export const systemClock = {};
export const monotonicClock = {
  now: nowNs,
  waitFor: (ns) => new Promise((resolve) => setTimeout(resolve, Number(ns) / 1e6)),
  waitUntil: (ns) => new Promise((resolve) => setTimeout(resolve, Math.max(0, Number(ns - nowNs()) / 1e6))),
};

export class Descriptor {}
export const fsTypes = { Descriptor };
export const preopens = { getDirectories: () => [] };

export class Fields {}
export class Request {}
export class RequestOptions {}
export class Response {}
export const httpTypes = { Fields, Request, RequestOptions, Response };
export const client = {
  send: () => {
    throw new Error("wasi:http is not available in the browser host");
  },
};

export const imports = {
  "wasi:cli/environment": environment,
  "wasi:cli/exit": exit,
  "wasi:cli/types": types,
  "wasi:cli/stdin": stdin,
  "wasi:cli/stdout": stdout,
  "wasi:cli/stderr": stderr,
  "wasi:cli/terminal-input": terminalInput,
  "wasi:cli/terminal-output": terminalOutput,
  "wasi:cli/terminal-stdin": terminalStdin,
  "wasi:cli/terminal-stdout": terminalStdout,
  "wasi:cli/terminal-stderr": terminalStderr,
  "wasi:clocks/types": clocksTypes,
  "wasi:clocks/system-clock": systemClock,
  "wasi:clocks/monotonic-clock": monotonicClock,
  "wasi:filesystem/types": fsTypes,
  "wasi:filesystem/preopens": preopens,
  "wasi:http/types": httpTypes,
  "wasi:http/client": client,
};
