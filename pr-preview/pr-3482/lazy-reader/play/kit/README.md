# The AIHC web kit

The web kit is aihc for the browser.
A web page uses it to type-check, compile, and run Haskell programs without a server.

Each release of aihc ships the kit as `aihc-<version>-web.tar.gz`.
The release workflow also publishes the unpacked kit at
`https://assets.aihc.app/play/releases/<version>/`.
A published version never changes.

The kit needs JavaScript Promise Integration.
Chrome 137, Firefox 139, and Safari 27 have it.

## Use the kit on a website

Pin one version string.
Import the entry module and give it the manifest of the same version.

```js
const KIT = "https://assets.aihc.app/play/releases/0.3.0/";
const { createCompiler } = await import(KIT + "aihc.js");
const aihc = await createCompiler(KIT + "manifest.json");

const built = await aihc.compile('main :: IO ()\nmain = putStrLn "Hello"\n');
if (built.ok) {
  const { exitCode } = await aihc.run(built.program, {
    onStdout: (text) => console.log(text),
  });
}
```

To move to a new release, change the version in `KIT`.
The host sends `access-control-allow-origin: *` and an immutable cache header.
Thus a page on a different origin can load each file, and a browser downloads a file only one time.

A site can also unpack the archive and serve the files itself.
Each file is smaller than 25 MiB, which is the limit for a static asset of a Cloudflare Worker.

### Workers and a different origin

The kit runs the compiler and each program in a worker.
A browser starts a worker only from a script of the same origin as the page.
When the kit is on a different origin, `aihc.js` starts the worker from a blob that imports `worker.js` of the kit.
You do not have to do anything for this.

A content security policy can forbid blob workers.
The policy must permit `worker-src blob:` and scripts from the origin of the kit.
If you cannot permit blob workers, add this one-line file to your own site:

```js
// aihc-worker.js
import "https://assets.aihc.app/play/releases/0.3.0/worker.js";
```

Then give its URL to the kit:

```js
const aihc = await createCompiler(KIT + "manifest.json", { workerUrl: "/aihc-worker.js" });
```

## The JavaScript interface

`aihc.js` exports `createCompiler` and `isSupported`.
Do not import the other modules of the kit.
Their interfaces and the request protocol of the helper can change in each release.

`isSupported()` returns `true` when the browser has JavaScript Promise Integration.

`createCompiler(manifestUrl, { workerUrl })` fetches the manifest and resolves with a compiler.
It does not download the compiler.
The first request does.
Each compiler has one worker, and the worker does one request at a time.

| Member | Result |
| --- | --- |
| `version` | The version of aihc in the kit. |
| `build` | A hash of the files of the kit. It names the browser cache. |
| `supported` | The result of `isSupported()`. |
| `downloadSize(groups)` | `{ total, missing }` in bytes. `missing` is the part that the browser cache does not have. |
| `load()` | Downloads and starts the front end. Resolves when it is ready. |
| `check(source)` | `{ ok, diagnostics, signatures }` from the type checker. |
| `compile(source, { onStatus, onLog })` | `{ ok, diagnostics, program }`. |
| `run(program, { stdin, onStdout, onStderr, signal })` | `{ exitCode }` when the program stops. |
| `warm(source)` | Compiles a module and discards the result. |
| `request(command, source)` | The JSON answer of one helper command. |
| `terminate()` | Stops the compiler worker. |

A diagnostic is `{ stage, message, line, col, endLine, endCol }`.
The positions start at 1, and `endCol` is exclusive.
A diagnostic without a source position has no position fields.
A signature is `{ line, name, type }` for a binding that has no signature in the source.

`check` and `compile` reject only when the kit itself fails.
A module with problems gives `ok: false` and a list of diagnostics.

`compile` needs a module with a `main` function.
It compiles the module, assembles it with LLVM, and links it with the core libraries.
`onStatus` gets a short sentence for each step.
`onLog` gets the messages of the assembler and the linker.

`run` starts the program in a worker of its own.
You can run one program many times.
`stdin` is a string or a `Uint8Array`.
`onStdout` and `onStderr` get decoded text.
When `signal` aborts, the program stops and the promise rejects with an `AbortError`.
A program has standard input, standard output, standard error, and a clock.
It has no files and no network.
`exitCode` is `0` or `1`.
The compiled program reports only success or failure to its host, so each failure code becomes `1`.

The first compile in a new worker is slow, because the WebAssembly engine optimizes the compiler while it runs.
Call `warm()` before the user asks for a build.

A compile is synchronous in the worker.
Thus `terminate()` is the only method to stop it.
Each request that waits rejects with an `AbortError`, and the next request starts a new worker.

`downloadSize` takes a list of groups.
The default is all groups that a compile needs: `["glue", "compiler", "linker"]`.
For `check` only, use `["glue", "compiler"]`.

`request` sends `parse`, `names`, `types`, or `compile` to the helper.
The Lazy Reader uses it to show the parse tree and the name resolution.
The answers are in `bin/aihc-play/src/Aihc/Play.hs`, and they can change in each release.

## Layout

| File | Group | Contents |
| --- | --- | --- |
| `manifest.json` | | The list of all files. |
| `aihc.js` | glue | The entry module. |
| `worker.js`, `host.js`, `manifest.js` | glue | The worker, its WASI host, and the downloads. |
| `aihc-play-component.js` | compiler | The helper, `bin/aihc-play`, as a JavaScript module from jco. |
| `aihc-play-component.core*.wasm.gz` | compiler | The WebAssembly modules of the helper. |
| `core.bundle.gz` | compiler | The naming facts and the type facts of the core libraries. |
| `runtime.pack.gz` | linker | The runtime objects, the archives of the core libraries, and the C library. |
| `tools/` | linker | The LLVM assembler and linker, and the component tools of jco. |
| `README.md`, `LICENSE`, `tools/*LICENSE*` | doc | This file and the licenses. |

The kit serves each `.gz` file without `Content-Encoding`.
The glue decompresses it.
Thus the size on the wire does not depend on the server.

### The manifest

```json
{
  "schema": 1,
  "name": "aihc-web-kit",
  "version": "0.3.0",
  "build": "0123456789abcdef",
  "entry": "aihc.js",
  "worker": "worker.js",
  "compiler": { "module": "aihc-play-component.js", "bundle": "core.bundle.gz" },
  "linker": { "tools": "tools/tools.js", "runtime": "runtime.pack.gz" },
  "packs": [],
  "files": [
    { "path": "core.bundle.gz", "bytes": 1234, "sha256": "…", "group": "compiler" }
  ]
}
```

`files` lists each file of the kit, but not `manifest.json` itself.
A file with `"module": true` is a JavaScript module that the browser imports.
The glue fetches each other file, compares its size and SHA-256 with the manifest, and keeps it in Cache Storage.
A file can have a `url`.
Then the file is at that URL and not beside the manifest.
A release does not use `url`.
The Lazy Reader uses it to keep large files on R2.

The glue rejects a manifest with a different `schema`.
A change that an old `aihc.js` cannot read gets a new schema number.

### The pack format

A pack is one file that holds many named files.
`runtime.pack.gz` is a pack in a gzip stream.

| Bytes | Contents |
| --- | --- |
| 8 | The ASCII text `AIHCPACK`. |
| 4 | The format version, `1`, little-endian. |
| 4 | The length `N` of the index, little-endian. |
| `N` | The index: UTF-8 JSON `{"files":[{"name","offset","bytes"}]}`. |
| | The files. Each `offset` counts from the end of the index. |

In `runtime.pack.gz`, the file `component-metadata` is the metadata of the WIT world.
Each other file is an input of the linker.

## Library packs (planned)

The kit has only the core libraries.
A later release will add library packs, for example `text` and `aeson`.
This section gives the planned layout.
No release has a pack, and `packs` is always empty.

A pack holds one compiled package:

- The naming facts and the type facts of its modules, in the format of `core.bundle`.
- The compiled WebAssembly archives of the package, as a pack file for the linker.

The manifest lists each pack, and the glue loads a pack only when a module imports it.

```json
"packs": [
  {
    "name": "text",
    "version": "2.1.2",
    "depends": [],
    "modules": ["Data.Text", "Data.Text.IO"],
    "facts": "packs/text-2.1.2/facts.bundle.gz",
    "archives": "packs/text-2.1.2/archives.pack.gz"
  },
  {
    "name": "aeson",
    "version": "2.2.3.0",
    "depends": ["text", "scientific"],
    "modules": ["Data.Aeson"],
    "facts": "packs/aeson-2.2.3.0/facts.bundle.gz",
    "archives": "packs/aeson-2.2.3.0/archives.pack.gz"
  }
]
```

The two files of a pack are also in `files`, with the group `pack`.
Thus the size and the SHA-256 of a pack come from the same list as each other file.
`depends` names the packs that the glue must load first.
`modules` lets the glue find the pack of an import without a download.

The glue will load a pack in these steps:

1. Find the imports of the module with the `parse` command.
2. Find the pack of each import, and then the packs in `depends`.
3. Send each `facts` file to the helper with a new `load-pack` request, in dependency order.
4. For a compile, add the files of each `archives` file to the inputs of the linker.

A pack is compiled with the same compiler as the kit.
Thus a pack is correct only for the kit of its own release, and it lives in the directory of that release.

## Build and test

```sh
nix run .#build-web-kit -- --output /tmp/kit --archive /tmp/aihc-web.tar.gz
nix run .#web-kit-smoke-test -- --archive /tmp/aihc-web.tar.gz
```

`scripts/build-web-kit.sh` builds the kit.
The first build compiles the dependencies of `aihc-play` for WebAssembly and takes some minutes.
Give `--store DIR` to keep the compiled packages for the next build.

`web-kit/test/smoke.mjs` is the smoke test.
It compares the files with the manifest and serves the kit on a local port.
Then it starts a headless Chromium, which loads the kit from a different origin.
The page type-checks, compiles, and runs the programs in `web-kit/test/`.
The Chromium comes from the pinned nixpkgs.

The release workflow runs both commands with the stage 2 release compiler.
A release does not continue when the smoke test fails.
`scripts/publish-web-kit.sh` publishes the kit of a tagged release.
It does not replace or delete a published file.
