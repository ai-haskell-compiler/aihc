// The Node.js shell of the engine of Aihc.Wasm.Engine.
//
// Usage: node driver.mjs IMAGE
//
// The shell gives the image to the engine core in engine-core.mjs. The
// core reads one command from each line of the standard input and writes
// one reply line to the standard output. engine-core.mjs gives the
// commands and the replies. The argument of the command "load" is the
// path of a shared module.
//
// The shell reads the standard input synchronously. Thus, an import of the
// instance can also read the next command while the instance runs.
//
// Usage with a directory: node driver.mjs IMAGE DIRECTORY
//
// Then the instance can read the files in DIRECTORY. The directory is the
// only preopened directory, with the name "/". Thus a relative path starts
// at DIRECTORY. The instance cannot write, and it cannot read a file
// outside DIRECTORY, also through a symbolic link.
//
// With the option --output after the other arguments, the instance can
// write to its standard output and to its standard error. The core then
// writes these bytes as lines of the standard output of the shell.

import { closeSync, openSync, readFileSync, readSync, readdirSync, realpathSync, statSync, writeSync } from "node:fs";
import { resolve as resolvePath, sep as pathSeparator } from "node:path";
import { FilesystemError, createEngine, errorCodes, typeCodes } from "./engine-core.mjs";

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

function write(line) {
  writeSync(1, line + "\n");
}

function typeCode(stats) {
  if (stats.isFile()) return typeCodes.regularFile;
  if (stats.isDirectory()) return typeCodes.directory;
  if (stats.isSymbolicLink()) return typeCodes.symbolicLink;
  if (stats.isFIFO()) return typeCodes.fifo;
  if (stats.isSocket()) return typeCodes.socket;
  if (stats.isCharacterDevice()) return typeCodes.characterDevice;
  if (stats.isBlockDevice()) return typeCodes.blockDevice;
  return typeCodes.other;
}

function errorCode(error) {
  switch (error?.code) {
    case "ENOENT":
      return errorCodes.noEntry;
    case "ENOTDIR":
      return errorCodes.notDirectory;
    case "EISDIR":
      return errorCodes.isDirectory;
    case "EACCES":
    case "EPERM":
      return errorCodes.access;
    case "ELOOP":
      return errorCodes.loop;
    case "ENAMETOOLONG":
      return errorCodes.nameTooLong;
    default:
      return errorCodes.io;
  }
}

// The read-only file system of a host directory.
function directoryFiles(directory) {
  // The directory, as a path without symbolic links.
  const root = realpathSync(directory);
  const inRoot = (candidate) => candidate === root || candidate.startsWith(root + pathSeparator);
  return {
    root,
    resolve(base, path) {
      if (path.startsWith("/") || path.includes("\0")) throw new FilesystemError(errorCodes.notPermitted);
      if (!inRoot(resolvePath(base, path))) throw new FilesystemError(errorCodes.notPermitted);
      const real = realpathSync(resolvePath(base, path));
      if (!inRoot(real)) throw new FilesystemError(errorCodes.notPermitted);
      return real;
    },
    stat(path) {
      const stats = statSync(path);
      return { type: typeCode(stats), nlink: stats.nlink, size: stats.size, ino: stats.ino, dev: stats.dev };
    },
    list: (path) => readdirSync(path),
    open: (path) => openSync(path, "r"),
    read(file, length, position) {
      const buffer = Buffer.alloc(length);
      const count = readSync(file, buffer, 0, length, position);
      return buffer.subarray(0, count);
    },
    close: closeSync,
    errorCode,
  };
}

const options = process.argv.slice(2).filter((argument) => argument.startsWith("--"));
const [imagePath, readablePath] = process.argv.slice(2).filter((argument) => !argument.startsWith("--"));

createEngine({
  imageBytes: readFileSync(imagePath),
  transport: { readLine, write },
  readModule: (path) => readFileSync(path),
  files: readablePath === undefined ? null : directoryFiles(readablePath),
  output: options.includes("--output"),
}).serve();
