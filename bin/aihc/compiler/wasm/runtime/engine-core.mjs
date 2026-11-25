// The engine core of Aihc.Wasm.Engine.
//
// This module does not use Node.js. A shell, such as driver.mjs, gives it
// the bytes of the image, a transport for lines of text, and, as an option,
// a read-only file system. The core makes one instance of the image, which
// is a core module that exports its memory, its function table, and its
// symbols. Then it reads one command from each line of the transport and
// writes one reply line:
//
//   load ARGUMENT          Load a shared module into the instance. The
//                          shell changes ARGUMENT into the bytes of the
//                          module.
//   call NAME ARGUMENT...  Call an exported function. An argument is
//                          i32:VALUE or i64:VALUE.
//   start                  Start the program of the image. The program is
//                          the guest, and it asks for closures.
//   run SYMBOL             Give the guest the closure at a data symbol.
//
// A reply is "ok", "ok VALUE...", or "error MESSAGE". The reply to "run"
// has each message that the closure sent, as hexadecimal text.
//
// Before the reply to "run", the core can write "request BYTES" lines.
// A closure sends a request with aihc_engine_request and waits. The next
// line of the input must then be "reply BYTES", with the bytes of the
// reply as hexadecimal text.
//
// After "start", the guest has control. It calls aihc_engine_next for its
// next closure, and that import reads the commands "load" and "run". The
// core does not call into the guest while the guest runs.
//
// The transport reads a line synchronously. Thus, an import of the
// instance can also read the next command while the instance runs.
//
// The instance gets no access to the host. Each import of the image is
// denied, apart from the functions in hostFunctions. These functions give
// fixed answers, such as an environment with only an empty PATH.
//
// With a file system, the instance can read its files. The root of the
// file system is the only preopened directory, with the name "/". The C
// library and the run-time system start a relative path at "/". The
// instance cannot write.
//
// With the output option, the instance can write to its standard output
// and to its standard error. Before the reply to a command, the core then
// writes "stdout BYTES" and "stderr BYTES" lines, with the bytes as
// hexadecimal text. The standard input of the instance is then at its end.
// Without the option, each use of these streams is a denied import.

const pageBytes = 65536;

// The canonical ABI values of wasi:filesystem/types@0.3.0.
export const typeCodes = { blockDevice: 0, characterDevice: 1, directory: 2, fifo: 3, symbolicLink: 4, regularFile: 5, socket: 6, other: 7 };
export const errorCodes = { access: 0, badDescriptor: 2, invalid: 11, io: 12, isDirectory: 13, loop: 14, nameTooLong: 17, noEntry: 19, notDirectory: 23, notPermitted: 30, readOnly: 32 };
const openCreate = 1;
const openDirectory = 2;
const openExclusive = 4;
const openTruncate = 8;
const descriptorRead = 1;
const descriptorWrite = 2;
const descriptorMutateDirectory = 32;
// The states of a waitable and of a subtask.
const waitableCompleted = 0;
const waitableDropped = 1;
const waitableCancelled = 2;
const subtaskReturned = 2;

// A filesystem error that goes to the instance as an error code.
export class FilesystemError extends Error {
  constructor(code) {
    super("filesystem error " + code);
    this.code = code;
  }
}

const hexDigits = Array.from({ length: 256 }, (_, byte) => byte.toString(16).padStart(2, "0"));

function toHex(bytes) {
  let text = "";
  for (const byte of bytes) text += hexDigits[byte];
  return text;
}

function hexValue(code) {
  if (code >= 48 && code <= 57) return code - 48;
  if (code >= 97 && code <= 102) return code - 87;
  if (code >= 65 && code <= 70) return code - 55;
  throw new Error("the text is not hexadecimal");
}

function fromHex(text) {
  if (text.length % 2 !== 0) throw new Error("the hexadecimal text has an odd length");
  const bytes = new Uint8Array(text.length / 2);
  for (let index = 0; index < bytes.length; index++) {
    bytes[index] = (hexValue(text.charCodeAt(2 * index)) << 4) | hexValue(text.charCodeAt(2 * index + 1));
  }
  return bytes;
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

// Make an engine. The options are:
//
//   imageBytes   The bytes of the image.
//   transport    { readLine, write }. readLine gives the next line without
//                its end, or null at the end of the input. write sends one
//                line without its end.
//   readModule   A function that gives the bytes of a shared module from
//                the argument of the command "load".
//   files        null, or a read-only file system with these functions:
//                  root                   The path of the root directory.
//                  resolve(base, path)    The real path of a path that
//                                         starts at the directory base. The
//                                         result must be in the root. Else
//                                         the function throws a
//                                         FilesystemError.
//                  stat(path)             { type, nlink, size, ino, dev }.
//                                         type is a value of typeCodes.
//                  list(path)             The names in a directory.
//                  open(path)             A file value for read.
//                  read(file, length, position)
//                                         At most length bytes, as a
//                                         Uint8Array. No bytes at the end.
//                  close(file)            Release a file value.
//                  errorCode(error)       The error code of an error that
//                                         is not a FilesystemError.
//   output       true when the instance can write to its standard output
//                and to its standard error.
//
// The result has the function serve, which reads the commands until the
// end of the input.
export function createEngine({ imageBytes, transport, readModule, files = null, output = false }) {
  function reply(text) {
    transport.write(text.replace(/\s+/g, " "));
  }

  const imageModule = new WebAssembly.Module(imageBytes);
  let image;

  // The functions that the host supplies to the image and to shared modules.
  const hostFunctions = {
    // The clock gives a fixed time, so that a run is deterministic.
    "wasi:clocks/monotonic-clock@0.3.0 now": () => 0n,
    // The environment is not the environment of the host. It has only the
    // variable PATH, with an empty value. A process on the build machine
    // always has PATH, and a library can look for a program there. An
    // example is findExecutable of the package directory, which raises an
    // exception when PATH is missing. The instance cannot start a program,
    // thus PATH is empty. An empty PATH names only the current directory,
    // which is the readable directory or no directory. The C library asks for the environment
    // in its constructors when the program can read the environment. The
    // import writes the list at the address of its argument, as the address
    // of the elements and their number. Each element is a name and a value.
    "wasi:cli/environment@0.3.0 get-environment": (result) => {
      const list = guestAllocate(new Uint8Array(16), 4);
      storeString(list, "PATH");
      storeString(list + 8, "");
      const view = memoryView();
      view.setUint32(result, list, true);
      view.setUint32(result + 4, 1, true);
    },
    "env aihc_engine_next": nextClosure,
    // A closure sends a message to the host.
    "env aihc_engine_send": (message, length) => {
      closureRun.messages.push(toHex(memoryBytes(message, length)));
    },
    // A closure sends a request to the host and waits for the reply. The
    // result is the number of bytes of the reply.
    "env aihc_engine_request": (message, length) => {
      reply("request " + toHex(memoryBytes(message, length)));
      const line = transport.readLine();
      if (line === null) throw new Error("the input ended at a request");
      const [command, bytes] = line.split(" ");
      if (command !== "reply") throw new Error("the host sent the command " + command + " at a request");
      requestReply = fromHex(bytes ?? "");
      return BigInt(requestReply.length);
    },
    // Copy the reply of the last request to memory of the guest.
    "env aihc_engine_reply": (target) => {
      new Uint8Array(image.memory.buffer, target, requestReply.length).set(requestReply);
    },
    // The guest reports that the closure raised an exception.
    "env aihc_engine_fail": (message, length) => {
      closureRun.failure = new TextDecoder().decode(memoryBytes(message, length));
    },
    // An image without the run-time system takes its trap function from here.
    "env aihc_lir_trap": (message, length) => {
      throw new Error("trap: " + new TextDecoder().decode(memoryBytes(message, length)));
    },
  };

  function hostErrorCode(error) {
    if (error instanceof FilesystemError) return error.code;
    return files?.errorCode?.(error) ?? errorCodes.io;
  }

  // The resources, streams, futures, and waitable sets of the instance. They
  // use one table of handles. A handle is not zero.
  const handles = new Map();
  let nextHandle = 1;

  function addHandle(value) {
    const handle = nextHandle++;
    handles.set(handle, value);
    return handle;
  }

  function handleOf(handle, kind) {
    const value = handles.get(handle);
    if (value?.kind !== kind) throw new FilesystemError(errorCodes.badDescriptor);
    return value;
  }

  function memoryView() {
    return new DataView(image.memory.buffer);
  }

  function memoryBytes(address, length) {
    return new Uint8Array(image.memory.buffer, address, Number(length));
  }

  function guestString(address, length) {
    return new TextDecoder().decode(memoryBytes(address, length));
  }

  // Copy bytes to new memory of the instance. The instance owns the memory.
  function guestAllocate(bytes, alignment) {
    const address = image.cabi_realloc(0, 0, alignment, bytes.length);
    new Uint8Array(image.memory.buffer, address, bytes.length).set(bytes);
    return address;
  }

  // Write a string at an address, as its address and its number of bytes.
  function storeString(address, text) {
    const bytes = new TextEncoder().encode(text);
    const pointer = bytes.length === 0 ? 1 : guestAllocate(bytes, 1);
    const view = memoryView();
    view.setUint32(address, pointer, true);
    view.setUint32(address + 4, bytes.length, true);
  }

  // The path of a path that starts at a directory descriptor. The path
  // must stay in the root, also after symbolic links.
  function hostPath(directory, path) {
    const descriptor = handleOf(directory, "descriptor");
    if (!descriptor.directory) throw new FilesystemError(errorCodes.notDirectory);
    return files.resolve(descriptor.path, path);
  }

  function openDescriptor(path) {
    const stats = files.stat(path);
    return addHandle({ kind: "descriptor", path, directory: stats.type === typeCodes.directory, file: null });
  }

  // Write a result with the canonical ABI. The ok value comes from the
  // function, which gets the address of the payload. An error code goes into
  // the payload. A function that fails gives an error.
  function storeResult(address, payloadOffset, store) {
    try {
      const write = store();
      const view = memoryView();
      view.setUint8(address, 0);
      write?.(address + payloadOffset);
    } catch (error) {
      const view = memoryView();
      view.setUint8(address, 1);
      view.setUint8(address + payloadOffset, hostErrorCode(error));
    }
  }

  // A descriptor-stat at an address. The timestamps are absent, so that a
  // splice gives the same result on each run.
  function storeStat(stats) {
    return (address) => {
      const view = memoryView();
      view.setUint8(address, stats.type);
      view.setBigUint64(address + 16, BigInt(stats.nlink), true);
      view.setBigUint64(address + 24, BigInt(stats.size), true);
      for (const offset of [32, 56, 80]) view.setUint8(address + offset, 0);
    };
  }

  function storeHash(stats) {
    return (address) => {
      const view = memoryView();
      view.setBigUint64(address, BigInt.asUintN(64, BigInt(stats.ino)), true);
      view.setBigUint64(address + 8, BigInt.asUintN(64, BigInt(stats.dev)), true);
    };
  }

  function openAt(self, pathAddress, pathLength, openFlags, flags, result) {
    storeResult(result, 4, () => {
      if (openFlags & (openCreate | openExclusive | openTruncate) || flags & (descriptorWrite | descriptorMutateDirectory)) {
        throw new FilesystemError(errorCodes.readOnly);
      }
      const handle = openDescriptor(hostPath(self, guestString(pathAddress, pathLength)));
      if (openFlags & openDirectory && !handles.get(handle).directory) {
        handles.delete(handle);
        throw new FilesystemError(errorCodes.notDirectory);
      }
      return (address) => memoryView().setUint32(address, handle, true);
    });
  }

  function descriptorStats(self) {
    const descriptor = handleOf(self, "descriptor");
    return files.stat(descriptor.path);
  }

  // A function of a write operation. Its result is the error read-only. The
  // last argument is the address of the result.
  function readOnly(...parameters) {
    const result = parameters[parameters.length - 1];
    const view = memoryView();
    view.setUint8(result, 1);
    view.setUint8(result + 4, errorCodes.readOnly);
  }

  // A stream of the bytes of a file, and a future of its result.
  function readViaStream(self, offset, result) {
    let future;
    let stream;
    try {
      const descriptor = handleOf(self, "descriptor");
      if (descriptor.directory) throw new FilesystemError(errorCodes.isDirectory);
      descriptor.file ??= files.open(descriptor.path);
      stream = addHandle({ kind: "stream", descriptor, position: Number(offset), ended: false });
      future = addHandle({ kind: "future", error: null });
    } catch (error) {
      stream = addHandle({ kind: "stream", descriptor: null, position: 0, ended: true });
      future = addHandle({ kind: "future", error: hostErrorCode(error) });
    }
    const view = memoryView();
    view.setUint32(result, stream, true);
    view.setUint32(result + 4, future, true);
  }

  function readStream(handle, address, length) {
    const stream = handleOf(handle, "stream");
    if (stream.ended) return waitableDropped;
    if (length === 0) return waitableCompleted;
    const bytes = files.read(stream.descriptor.file, length, stream.position);
    const count = bytes.length;
    if (count === 0) {
      stream.ended = true;
      return waitableDropped;
    }
    new Uint8Array(image.memory.buffer, address, count).set(bytes);
    stream.position += count;
    return (count << 4) | waitableCompleted;
  }

  // A stream of the entries of a directory. The entries are in the order of
  // their names, so that a splice gives the same result on each run.
  function readDirectory(self, result) {
    let entries = [];
    let error = null;
    try {
      const descriptor = handleOf(self, "descriptor");
      if (!descriptor.directory) throw new FilesystemError(errorCodes.notDirectory);
      entries = files
        .list(descriptor.path)
        .sort()
        .map((name) => {
          let type = typeCodes.other;
          try {
            type = files.stat(hostPath(self, name)).type;
          } catch {
            // An entry outside the root, or a broken link, has the type other.
          }
          return { name, type };
        });
    } catch (failure) {
      error = hostErrorCode(failure);
    }
    const view = memoryView();
    view.setUint32(result, addHandle({ kind: "entries", entries, position: 0 }), true);
    view.setUint32(result + 4, addHandle({ kind: "future", error }), true);
  }

  // Each directory-entry has 24 bytes: the type at 0, with no payload, and
  // the name at 16.
  function readEntries(handle, address, length) {
    const stream = handleOf(handle, "entries");
    if (stream.position === stream.entries.length) return waitableDropped;
    const count = Math.min(length, stream.entries.length - stream.position);
    for (let index = 0; index < count; index++) {
      const entry = stream.entries[stream.position + index];
      const at = address + 24 * index;
      const view = memoryView();
      view.setUint8(at, entry.type);
      view.setUint8(at + 4, 0);
      storeString(at + 16, entry.name);
    }
    stream.position += count;
    return (count << 4) | waitableCompleted;
  }

  // The future of a stream has its result at once: the host reads files
  // synchronously.
  function readFuture(handle, address) {
    const future = handleOf(handle, "future");
    const view = memoryView();
    if (future.error === null) {
      view.setUint8(address, 0);
    } else {
      view.setUint8(address, 1);
      view.setUint8(address + 4, future.error);
    }
    return waitableCompleted;
  }

  function dropHandle(handle) {
    const value = handles.get(handle);
    if (value?.kind === "descriptor" && value.file !== null) files.close(value.file);
    handles.delete(handle);
  }

  const filesystem = "wasi:filesystem/types@0.3.0 ";
  const byteStream = "[method]descriptor.read-via-stream";
  const entryStream = "[method]descriptor.read-directory";

  // The functions that give read access to the root. Each operation
  // completes before it returns, thus a subtask is returned at once and a
  // stream or a future is never blocked.
  const filesystemFunctions = {
    // The root is the only preopened directory, with the name "/". Each
    // call gives a new descriptor, which the instance owns.
    "wasi:filesystem/preopens@0.3.0 get-directories": (result) => {
      const list = guestAllocate(new Uint8Array(12), 4);
      memoryView().setUint32(list, openDescriptor(files.root), true);
      storeString(list + 4, "/");
      memoryView().setUint32(result, list, true);
      memoryView().setUint32(result + 4, 1, true);
    },
    // There is no initial directory. A relative path starts at "/".
    "wasi:cli/environment@0.3.0 get-initial-cwd": (result) => {
      memoryView().setUint8(result, 0);
    },
    [filesystem + "[resource-drop]descriptor"]: dropHandle,
    [filesystem + "[method]descriptor.open-at"]: (self, _pathFlags, path, length, openFlags, flags, result) =>
      openAt(self, path, length, openFlags, flags, result),
    // The arguments are at an address: the descriptor at 0, the path flags at
    // 4, the path at 8, the open flags at 16, and the descriptor flags at 17.
    [filesystem + "[async-lower][method]descriptor.open-at"]: (arguments_, result) => {
      const view = memoryView();
      openAt(view.getUint32(arguments_, true), view.getUint32(arguments_ + 8, true), view.getUint32(arguments_ + 12, true), view.getUint8(arguments_ + 16), view.getUint8(arguments_ + 17), result);
      return subtaskReturned;
    },
    [filesystem + "[method]descriptor.stat"]: (self, result) => storeResult(result, 8, () => storeStat(descriptorStats(self))),
    [filesystem + "[async-lower][method]descriptor.stat"]: (self, result) => {
      storeResult(result, 8, () => storeStat(descriptorStats(self)));
      return subtaskReturned;
    },
    [filesystem + "[method]descriptor.stat-at"]: (self, _pathFlags, path, length, result) =>
      storeResult(result, 8, () => storeStat(files.stat(hostPath(self, guestString(path, length))))),
    [filesystem + "[async-lower][method]descriptor.get-type"]: (self, result) => {
      storeResult(result, 4, () => {
        const type = descriptorStats(self).type;
        return (address) => {
          memoryView().setUint8(address, type);
        };
      });
      return subtaskReturned;
    },
    [filesystem + "[method]descriptor.get-flags"]: (self, result) =>
      storeResult(result, 4, () => {
        handleOf(self, "descriptor");
        return (address) => memoryView().setUint8(address, descriptorRead);
      }),
    [filesystem + "[method]descriptor.metadata-hash"]: (self, result) => storeResult(result, 8, () => storeHash(descriptorStats(self))),
    [filesystem + "[method]descriptor.metadata-hash-at"]: (self, _pathFlags, path, length, result) =>
      storeResult(result, 8, () => storeHash(files.stat(hostPath(self, guestString(path, length))))),
    [filesystem + "[async-lower][method]descriptor.is-same-object"]: (self, other, result) => {
      let same = false;
      try {
        same = handleOf(self, "descriptor").path === handleOf(other, "descriptor").path;
      } catch {
        same = false;
      }
      memoryView().setUint8(result, same ? 1 : 0);
      return subtaskReturned;
    },
    // The host gives the target of each symbolic link, thus no path names a
    // symbolic link.
    [filesystem + "[method]descriptor.readlink-at"]: (_self, _path, _length, result) => {
      const view = memoryView();
      view.setUint8(result, 1);
      view.setUint8(result + 4, errorCodes.invalid);
    },
    [filesystem + "[method]descriptor.advise"]: (_self, _offset, _length, _advice, result) => memoryView().setUint8(result, 0),
    [filesystem + "[method]descriptor.sync"]: (_self, result) => memoryView().setUint8(result, 0),
    [filesystem + "[method]descriptor.sync-data"]: (_self, result) => memoryView().setUint8(result, 0),
    [filesystem + "[method]descriptor.create-directory-at"]: readOnly,
    [filesystem + "[method]descriptor.remove-directory-at"]: readOnly,
    [filesystem + "[method]descriptor.unlink-file-at"]: readOnly,
    [filesystem + "[method]descriptor.rename-at"]: readOnly,
    [filesystem + "[method]descriptor.symlink-at"]: readOnly,
    [filesystem + "[method]descriptor.link-at"]: readOnly,
    [filesystem + "[method]descriptor.set-size"]: readOnly,
    [filesystem + "[method]descriptor.set-times"]: readOnly,
    [filesystem + "[method]descriptor.set-times-at"]: readOnly,
    [filesystem + byteStream]: readViaStream,
    [filesystem + "[async-lower][stream-read-0]" + byteStream]: readStream,
    [filesystem + "[stream-drop-readable-0]" + byteStream]: dropHandle,
    [filesystem + "[stream-cancel-read-0]" + byteStream]: () => waitableCancelled,
    [filesystem + "[async-lower][future-read-1]" + byteStream]: readFuture,
    [filesystem + "[future-drop-readable-1]" + byteStream]: dropHandle,
    [filesystem + "[future-cancel-read-1]" + byteStream]: () => waitableCancelled,
    [filesystem + entryStream]: readDirectory,
    [filesystem + "[async-lower][stream-read-0]" + entryStream]: readEntries,
    [filesystem + "[stream-drop-readable-0]" + entryStream]: dropHandle,
    [filesystem + "[stream-cancel-read-0]" + entryStream]: () => waitableCancelled,
    [filesystem + "[async-lower][future-read-1]" + entryStream]: readFuture,
    [filesystem + "[future-drop-readable-1]" + entryStream]: dropHandle,
    [filesystem + "[future-cancel-read-1]" + entryStream]: () => waitableCancelled,
    // The run-time system makes a waitable set for each operation. No
    // operation blocks, thus the set never gets an event.
    "$root [waitable-set-new]": () => addHandle({ kind: "waitable-set" }),
    "$root [waitable-set-drop]": dropHandle,
    "$root [waitable-join]": () => {},
  };

  // The filesystem functions are only available with a file system.
  if (files !== null) Object.assign(hostFunctions, filesystemFunctions);

  // The run-time system makes a stream, gives the readable end to the
  // standard output or to the standard error, and writes the bytes to the
  // writable end. The two ends are handles of one stream value.
  function newOutputStream() {
    const stream = { kind: "output", target: null };
    return (BigInt(addHandle(stream)) << 32n) | BigInt(addHandle(stream));
  }

  // Give the readable end of a stream to a target. The result is a future of
  // the result of the writes, which has no error.
  function writeViaStream(target) {
    return (reader) => {
      handleOf(reader, "output").target = target;
      handles.delete(reader);
      return addHandle({ kind: "future", error: null });
    };
  }

  // Send the bytes to the host as one line. The host takes all the bytes at
  // once, thus the write is complete when the function returns.
  function writeOutput(writer, address, length) {
    const stream = handleOf(writer, "output");
    if (stream.target === null) throw new Error("a write to a stream that has no target");
    if (length > 0) reply(stream.target + " " + toHex(memoryBytes(address, length)));
    return ((length << 4) | waitableCompleted) >>> 0;
  }

  // The standard input has no bytes: its stream is at its end.
  function readStandardInput(result) {
    const view = memoryView();
    view.setUint32(result, addHandle({ kind: "stream", descriptor: null, position: 0, ended: true }), true);
    view.setUint32(result + 4, addHandle({ kind: "future", error: null }), true);
  }

  const streamEnds = "wasi:cli/stdin@0.3.0 ";
  const outputFunctions = {
    [streamEnds + "read-via-stream"]: readStandardInput,
    [streamEnds + "[async-lower][stream-read-0]read-via-stream"]: readStream,
    [streamEnds + "[stream-drop-readable-0]read-via-stream"]: dropHandle,
    [streamEnds + "[async-lower][future-read-1]read-via-stream"]: readFuture,
    [streamEnds + "[future-drop-readable-1]read-via-stream"]: dropHandle,
    [streamEnds + "[stream-new-0]read-via-stream"]: newOutputStream,
    [streamEnds + "[async-lower][stream-write-0]read-via-stream"]: writeOutput,
    [streamEnds + "[stream-drop-writable-0]read-via-stream"]: dropHandle,
    "$root [waitable-set-new]": filesystemFunctions["$root [waitable-set-new]"],
    "$root [waitable-set-drop]": dropHandle,
    "$root [waitable-join]": () => {},
  };
  for (const target of ["stdout", "stderr"]) {
    const name = `wasi:cli/${target}@0.3.0 `;
    outputFunctions[name + "write-via-stream"] = writeViaStream(target);
    outputFunctions[name + "[async-lower][future-read-1]write-via-stream"] = readFuture;
    outputFunctions[name + "[future-drop-readable-1]write-via-stream"] = dropHandle;
  }

  // The output functions are only available with the output option.
  if (output) Object.assign(hostFunctions, outputFunctions);

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

  // The data of the shared modules is in pages that the core adds to the
  // memory. New pages contain zeros. The run-time system and the C library
  // also add pages, and they do not use the pages of the core.
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

  function load(bytes) {
    const module = new WebAssembly.Module(bytes);
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

  // The guest is "idle" before the command "start", "running" while its
  // program runs, and "stopped" after its program returns or traps.
  let guest = "idle";
  // The reply of the last request.
  let requestReply = new Uint8Array(0);
  // The closure that the guest runs, or null. The command "run" has no reply
  // until the guest asks for the next closure.
  let closureRun = null;

  function failure(error) {
    return "error " + (error?.message ?? error);
  }

  // The import aihc_engine_next of the guest. It replies to the command that
  // gave control to the guest. Then it reads commands until one gives a
  // closure. The result is the address of the closure, or zero at the end of
  // the input.
  function nextClosure() {
    if (closureRun === null) reply("ok");
    else if (closureRun.failure !== undefined) reply("error " + closureRun.failure);
    else reply(["ok", ...closureRun.messages].join(" "));
    closureRun = null;
    for (let line = transport.readLine(); line !== null; line = transport.readLine()) {
      const [command, name] = line.split(" ");
      try {
        if (command === "load") {
          load(readModule(line.slice(5)));
          reply("ok");
        } else if (command === "run") {
          const address = dataSymbols.get(name);
          if (address === undefined) throw new Error("unknown closure " + name);
          closureRun = { messages: [] };
          return address;
        } else {
          throw new Error("the guest cannot do the command " + command);
        }
      } catch (error) {
        reply(failure(error));
      }
    }
    return 0;
  }

  // Run the program of the image. The call returns at the end of the input.
  // A trap in the guest is the reply to the command that the guest has.
  function start() {
    if (guest !== "idle") throw new Error("the guest was started before");
    guest = "running";
    try {
      image.__wasm_call_ctors?.();
      image.aihc_lir_program_start();
    } finally {
      guest = "stopped";
      closureRun = null;
    }
  }

  // Reply "ok" to show that the instance is ready. Then do each command
  // until the end of the input.
  function serve() {
    reply("ok");
    for (let line = transport.readLine(); line !== null; line = transport.readLine()) {
      const [command, ...parameters] = line.split(" ");
      try {
        if (command === "load") {
          load(readModule(line.slice(5)));
          reply("ok");
        } else if (command === "call") {
          reply(call(parameters[0], parameters.slice(1)));
        } else if (command === "start") {
          start();
        } else if (command === "run") {
          throw new Error("the guest is " + guest);
        } else {
          throw new Error("unknown command " + command);
        }
      } catch (error) {
        reply(failure(error));
      }
    }
  }

  return { serve };
}
