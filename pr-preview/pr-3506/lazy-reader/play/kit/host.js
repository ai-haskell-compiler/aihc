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
// The caller can also supply wake(), the result of quickTimers.

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

// The timers of the page. quickTimers replaces the global ones, and the clock
// of the host must wait for the real time.
const realSetTimeout = setTimeout;
const realSetInterval = setInterval;
const realClearTimeout = clearTimeout;

// The number of short timers that run at once after each call of wake.
const QUICK_TURNS = 256;

// Make the short timers of this worker run at once.
//
// The module that jco makes polls with timers. A program that waits for the
// host continues only when a timer of 10 ms sees the answer, and a browser
// delays a nested timer of 0 ms by 4 ms. Thus each write of a program took
// 16 ms, and a program printed 60 lines in one second.
//
// After quickTimers, a timer of at most 10 ms runs as the next task of the
// worker. The result is the function wake. Each call of wake permits
// QUICK_TURNS such timers. When they are spent, a timer waits for its real
// time again. Thus a program that sleeps or waits for input polls at the
// slow rate and does not keep a processor busy. createImports calls wake
// when the program calls the host and when the host has an answer.
//
// Call quickTimers only in a worker that runs one program and nothing else.
export function quickTimers() {
  const channel = new MessageChannel();
  const tasks = new Map();
  let turns = 0;
  let nextId = -1;
  const quick = (callback, delay) => typeof callback === "function" && !(delay > 10) && turns > 0;
  const post = (id) => {
    turns -= 1;
    channel.port2.postMessage(id);
  };
  channel.port1.onmessage = ({ data: id }) => {
    const task = tasks.get(id);
    if (!task) return;
    if (!task.repeat) tasks.delete(id);
    else if (turns > 0) post(id);
    else task.real = realSetInterval(task.callback, task.delay, ...task.args);
    task.callback(...task.args);
  };
  const add = (callback, delay, args, repeat) => {
    const id = nextId--;
    tasks.set(id, { callback, delay, args, repeat, real: null });
    post(id);
    return id;
  };
  // A quick timer has a negative identifier. A browser gives positive ones.
  const clear = (id) => {
    const task = tasks.get(id);
    if (!task) return realClearTimeout(id);
    tasks.delete(id);
    if (task.real !== null) realClearTimeout(task.real);
  };
  globalThis.setTimeout = (callback, delay, ...args) => (quick(callback, delay) ? add(callback, delay, args, false) : realSetTimeout(callback, delay, ...args));
  globalThis.setInterval = (callback, delay, ...args) => (quick(callback, delay) ? add(callback, delay, args, true) : realSetInterval(callback, delay, ...args));
  globalThis.clearTimeout = clear;
  globalThis.clearInterval = clear;
  return () => {
    turns = QUICK_TURNS;
  };
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
  const wake = () => host.wake?.();
  const sleep = (ms) => {
    wake();
    return new Promise((resolve) => realSetTimeout(resolve, ms)).then(wake);
  };
  const output = (sink) => ({
    writeViaStream: async (data) => {
      wake();
      await drain(data, (value) => sink(toBytes(value)));
      wake();
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
        wake();
        async function* source() {
          const chunk = await host.nextInput();
          wake();
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
      waitFor: (ns) => sleep(Number(ns) / 1e6),
      waitUntil: (ns) => sleep(Math.max(0, Number(ns - nowNs()) / 1e6)),
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
