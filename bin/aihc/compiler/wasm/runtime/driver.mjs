// The engine driver of Aihc.Wasm.Engine.
//
// Usage: node driver.mjs IMAGE
//
// The driver makes one instance of the image, which is a core module that
// exports its memory, its function table, and its symbols. Then it reads
// one command from each line of the standard input and writes one reply
// line to the standard output:
//
//   load PATH              Load a shared module into the instance.
//   call NAME ARGUMENT...  Call an exported function. An argument is
//                          i32:VALUE or i64:VALUE.
//
// A reply is "ok", "ok VALUE", or "error MESSAGE".
//
// The driver reads the standard input synchronously. Thus, an import of the
// instance can also read the next command while the instance runs.
//
// The instance gets no access to the host. Each import of the image is
// denied, apart from the functions in hostFunctions.

import { readFileSync, readSync, writeSync } from "node:fs";

const pageBytes = 65536;
let pendingInput = Buffer.alloc(0);

// The next line of the standard input, or null at the end of the input.
function readLine() {
  for (;;) {
    const newline = pendingInput.indexOf(10);
    if (newline >= 0) {
      const line = pendingInput.subarray(0, newline).toString("utf8");
      pendingInput = pendingInput.subarray(newline + 1);
      return line;
    }
    const chunk = Buffer.alloc(pageBytes);
    let count;
    try {
      count = readSync(0, chunk, 0, chunk.length, null);
    } catch (error) {
      if (error.code !== "EAGAIN") throw error;
      // The input has no data at this time. Wait for one millisecond.
      Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 1);
      continue;
    }
    if (count === 0) return null;
    pendingInput = Buffer.concat([pendingInput, chunk.subarray(0, count)]);
  }
}

function reply(text) {
  writeSync(1, text.replace(/\s+/g, " ") + "\n");
}

function unsignedLeb(bytes, position) {
  let value = 0;
  let shift = 0;
  let byte;
  do {
    byte = bytes[position.at++];
    value |= (byte & 0x7f) << shift;
    shift += 7;
  } while (byte & 0x80);
  return value >>> 0;
}

// The memory and the table that a shared module needs. The linker records
// them in the memory information of the dylink.0 section.
function dynamicLinkInformation(module) {
  const [section] = WebAssembly.Module.customSections(module, "dylink.0");
  if (!section) throw new Error("the module is not a shared module");
  const bytes = new Uint8Array(section);
  const position = { at: 0 };
  while (position.at < bytes.length) {
    const kind = bytes[position.at++];
    const end = unsignedLeb(bytes, position) + position.at;
    if (kind === 1) {
      return {
        memorySize: unsignedLeb(bytes, position),
        memoryAlignment: 1 << unsignedLeb(bytes, position),
        tableSize: unsignedLeb(bytes, position),
      };
    }
    position.at = end;
  }
  throw new Error("the shared module has no memory information");
}

const imageModule = new WebAssembly.Module(readFileSync(process.argv[2]));
let image;

// The functions that the host supplies to the image and to shared modules.
const hostFunctions = {
  // The clock gives a fixed time, so that a run is deterministic.
  "wasi:clocks/monotonic-clock@0.3.0 now": () => 0n,
  // An image without the run-time system takes its trap function from here.
  "env aihc_lir_trap": (message, length) => {
    const bytes = new Uint8Array(image.memory.buffer, message, Number(length));
    throw new Error("trap: " + new TextDecoder().decode(bytes));
  },
};

function denied(name) {
  return () => {
    throw new Error("the instance called the denied import " + name);
  };
}

const imageImports = {};
for (const { module, name, kind } of WebAssembly.Module.imports(imageModule)) {
  if (kind !== "function") throw new Error(`the image imports the ${kind} ${module} ${name}`);
  imageImports[module] ??= {};
  imageImports[module][name] = hostFunctions[`${module} ${name}`] ?? denied(`${module} ${name}`);
}
image = new WebAssembly.Instance(imageModule, imageImports).exports;
const table = image.__indirect_function_table;

// The symbols that a shared module can import: the exports of the image
// and of each shared module that is already loaded. A data symbol maps to
// its address, and a function symbol maps to the function.
const dataSymbols = new Map();
const functionSymbols = new Map();
const globalSymbols = new Map();
for (const [name, value] of Object.entries(image)) {
  if (typeof value === "function") functionSymbols.set(name, value);
  else if (name === "__stack_pointer") globalSymbols.set(name, value);
  else if (value instanceof WebAssembly.Global) dataSymbols.set(name, value.value);
}

// The index of each function that is in the table. A function keeps one
// index, because code can compare function addresses.
const tableIndexes = new Map();
let scannedTableLength = 0;

function tableIndex(fn) {
  for (; scannedTableLength < table.length; scannedTableLength++) {
    const entry = table.get(scannedTableLength);
    if (entry !== null && !tableIndexes.has(entry)) tableIndexes.set(entry, scannedTableLength);
  }
  let index = tableIndexes.get(fn);
  if (index === undefined) {
    index = table.grow(1);
    table.set(index, fn);
    tableIndexes.set(fn, index);
    scannedTableLength = table.length;
  }
  return index;
}

// The data of the shared modules is in pages that the driver adds to the
// memory. New pages contain zeros. The run-time system and the C library
// also add pages, and they do not use the pages of the driver.
const minimumGrowthPages = 16;
let freeMemory = 0;
let freeMemoryEnd = 0;

function allocate(size, alignment) {
  let address = (freeMemory + alignment - 1) & ~(alignment - 1);
  if (address + size > freeMemoryEnd) {
    const pages = Math.max(minimumGrowthPages, Math.ceil(size / pageBytes));
    address = image.memory.grow(pages) * pageBytes;
    freeMemoryEnd = address + pages * pageBytes;
  }
  freeMemory = address + size;
  return address;
}

function unresolved(name) {
  throw new Error("the shared module refers to the unknown symbol " + name);
}

function load(path) {
  const module = new WebAssembly.Module(readFileSync(path));
  const information = dynamicLinkInformation(module);
  const memoryBase = allocate(information.memorySize, information.memoryAlignment);
  const tableBase = table.grow(information.tableSize);
  const constant = (value) => new WebAssembly.Global({ value: "i32" }, value);
  // An entry of the Global Offset Table. The module itself defines the
  // symbols that stay in ownEntries, and their addresses are only known
  // after the instantiation.
  const ownEntries = [];
  const tableEntry = (kind, name, value) => {
    const entry = new WebAssembly.Global({ value: "i32", mutable: true }, value ?? 0);
    if (value === undefined) ownEntries.push({ kind, name, entry });
    return entry;
  };
  const imports = { env: {}, "GOT.mem": {}, "GOT.func": {} };
  for (const { module: from, name, kind } of WebAssembly.Module.imports(module)) {
    if (from === "GOT.mem") {
      imports[from][name] = tableEntry("data", name, dataSymbols.get(name));
    } else if (from === "GOT.func") {
      const fn = functionSymbols.get(name);
      imports[from][name] = tableEntry("function", name, fn && tableIndex(fn));
    } else if (from !== "env") {
      unresolved(`${from} ${name}`);
    } else if (kind === "memory") {
      imports.env[name] = image.memory;
    } else if (kind === "table") {
      imports.env[name] = table;
    } else if (name === "__memory_base") {
      imports.env[name] = constant(memoryBase);
    } else if (name === "__table_base") {
      imports.env[name] = constant(tableBase);
    } else if (kind === "global") {
      imports.env[name] = globalSymbols.get(name) ?? unresolved(name);
    } else {
      imports.env[name] = functionSymbols.get(name) ?? hostFunctions[`env ${name}`] ?? unresolved(name);
    }
  }
  const exports = new WebAssembly.Instance(module, imports).exports;
  // The instantiation filled the table entries of the module. Examine them
  // again, because they were empty before.
  scannedTableLength = Math.min(scannedTableLength, tableBase);
  for (const [name, value] of Object.entries(exports)) {
    if (name.startsWith("__wasm_")) continue;
    if (typeof value === "function" && !functionSymbols.has(name)) functionSymbols.set(name, value);
    if (value instanceof WebAssembly.Global && !dataSymbols.has(name)) dataSymbols.set(name, memoryBase + value.value);
  }
  for (const { kind, name, entry } of ownEntries) {
    if (kind === "data") entry.value = dataSymbols.get(name) ?? unresolved(name);
    else entry.value = tableIndex(functionSymbols.get(name) ?? unresolved(name));
  }
  exports.__wasm_apply_data_relocs?.();
  exports.__wasm_call_ctors?.();
}

function argument(text) {
  const [type, value] = text.split(":");
  if (type === "i32") return Number(value) | 0;
  if (type === "i64") return BigInt.asIntN(64, BigInt(value));
  throw new Error("unknown argument type " + type);
}

function call(name, parameters) {
  const fn = functionSymbols.get(name);
  if (!fn) throw new Error("unknown function " + name);
  const result = fn(...parameters.map(argument));
  return result === undefined ? "ok" : "ok " + result;
}

reply("ok");
for (let line = readLine(); line !== null; line = readLine()) {
  const [command, ...parameters] = line.split(" ");
  try {
    if (command === "load") {
      load(line.slice(5));
      reply("ok");
    } else if (command === "call") {
      reply(call(parameters[0], parameters.slice(1)));
    } else {
      throw new Error("unknown command " + command);
    }
  } catch (error) {
    reply("error " + (error?.message ?? error));
  }
}
