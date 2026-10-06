# WASI P3 backend

The `wasm32-wasip3` target lowers GC-GRIN to Lir and emits LLVM MC's
WebAssembly assembly syntax with `Aihc.Wasm.Lir` (see `docs/lir.md`). This is
WebAssembly machine code: the backend selects Wasm instructions, locals,
structured control flow, data objects, and the runtime ABI directly. Clang's
integrated Wasm assembler only serializes those instructions and records
relocations; generated Haskell code does not pass through C or LLVM IR.

The build pipeline uses temporary linker inputs:

```text
dependency GC-GRIN -> Lir -> WebAssembly assembly -> cached dependency objects
main GC-GRIN       -> Lir -> WebAssembly assembly -> program.o
Lir entry unit     -> WebAssembly assembly -> entry object
C runtime + P3 IO backend       -> runtime objects
WIT C bindings                  -> binding object
all objects + libc -> wasm-component-ld -> component
```

The resulting output is one WebAssembly component. The object files are
removed after linking.

The driver invokes the standard LLVM tools directly: `clang
--target=wasm32-wasip3`, `wasm-component-ld`, `wasm-ld`, `wasm-tools`, and
`wit-bindgen`. They may come from any LLVM/WASI installation on `PATH`; no
`wasm32-clang` wrapper is required. `wasm-component-ld` links the core module
with `wasm-ld` and encodes it as a component. `AIHC_WASM_CLANG` can select
another Clang executable when a host toolchain wrapper is not cross-target
safe. The Nix development environment uses that override to select its
unwrapped LLVM Clang.

## The WASI sysroot

The target links the libc that wasi-sdk 34 or later builds for
`wasm32-wasip3`. That libc calls the host through the WASI 0.3 interfaces of
the component model, so `opendir`, `readdir`, `stat`, `getcwd`, `getenv`, and
the rest of the file and environment functions work in a program that uses
the `unix` or `directory` package. The libc of the other wasi-sdk targets, and
the wasi-libc of other distributions, call the host through WASI preview 1
imports, which the component of a program cannot have.

Install the [wasi-sdk release](https://github.com/WebAssembly/wasi-sdk/releases)
sysroot, and set `AIHC_WASM_SYSROOT` to the directory that holds
`include/wasm32-wasip3` and `lib/wasm32-wasip3/libc.a` when it sits outside a
standard prefix. The link also needs the compiler runtime archive
`libclang_rt.builtins.a` of the target, which wasi-sdk ships as its own
asset. The compiler looks for it in `lib/wasm32-wasip3` of the sysroot, then in
the compiler library of a wasi-sdk installation beside it, and
`AIHC_WASM_BUILTINS` names it directly. The Nix development environment
builds a sysroot that holds both.

The C sources of the runtime compile against the headers of that sysroot, and
`wasm-component-ld` takes the `libc.a` after every other input, so the linker
draws from it only what a symbol asks for. The build stays `-nostdlib`: the
archive is an explicit input and the driver never adds a startup object of its
own. The runtime calls the constructors of the libc, `__wasm_call_ctors`,
before the program starts.

The libc finds its stack pointer and its thread-local storage through
functions, so that a component with several tasks can keep one of each per
task. The runtime runs the whole program in one task, and
`core-libs/aihc-rts/wasm/aihc_wasip3_libc.c` defines the functions over the
global stack pointer and the one static thread-local segment of the linked
module. That segment needs the `atomics` feature of the linker, which the
link adds.

A file that the runtime opens has no descriptor in the libc. The handle keeps the
position that the next read or write starts from, and `hSeek` and `hTell` change
and read it. The runtime does not know the size of the file, so a seek from the
end and `hFileSize` are unsupported.

WASI has no file modes, and the libc fails `chmod` with `ENOSYS`. The same file
defines `__wrap_chmod` and `__wrap_fchmod`. The link passes `--wrap` for both
names, so that a call succeeds and leaves the file as it is. A program that sets a mode for each file it makes, as the `tar` package does,
then runs to its end.

The host allocator of the component model is the one `cabi_realloc` of the
module. A request that the runtime makes inside an explicit host scope gets a
byte array that the collector owns, and a request that comes from a libc call
gets the memory of the libc allocator, which the libc frees.

## Runtime ABI

Generated functions have the Lir signatures of the lowering: the context,
the GRIN parameters, and no results. Every CPS transfer is a `return_call`, so the
whole program runs inside the call that started it. Each Lir value is a
WebAssembly local; values reach linear memory only at the boundaries that
need an address, such as the live-root vector of a collection safepoint on the
shadow stack.

The P3 driver owns the IO loop. The Lir entry unit exports
`aihc_lir_program_start`, which creates the machine and evaluates the entry,
and `aihc_lir_program_resume`, which continues a scheduler resumption after an
IO request completes. Both return when the machine halts or when every green
thread waits for IO, and both report which of the two happened. The driver
waits for the event of the pending request in the second case, and resumes the
program when it arrives.

The `wasi:cli/run@0.3.0` export is lifted synchronously, although the WIT
function is `async`. The whole program runs inside that one task. A task of
this kind may block, which a callback task may not, and a libc function that
waits for the host, such as one of the WASI 0.3 libc, is only allowed to in a
task that may block. The bindings are generated with
`--async=-export:wasi:cli/run@0.3.0#run` for this reason.

Runtime info tables are ordinary relocatable data objects with 4-byte words.
Function addresses in those tables become Wasm table indices when `wasm-ld`
links the program and runtime. Heap pointers remain 32-bit Wasm addresses
stored in the shared 8-byte slot type used by the other backends.

## Asynchronous stdout

The initial P3 IO backend implements stdout writes with
`wasi:cli/stdout@0.3.0`. It creates a `stream<u8>`, supplies its readable end to
`write-via-stream`, and incrementally writes the AIHC IO buffer through the
writable end. When the stream or result future blocks, the `run` export waits
on the waitable set of the request. The event finishes the request, makes its
green thread runnable, and resumes the program through
`aihc_lir_program_resume`.

The `System.IO` `stdout` handle uses this path, including its `MVar`-serialized
handle state and native-width `Int` FFI results. The current WIT world does not
import stdin, stderr, or filesystem interfaces. Those fixed handles still
exist, but unsupported operations and `openBinaryFile` report an IO error; an
uncaught `IOException` traps because the component has no synchronous error
stream.

## HTTP

The world imports `wasi:http/client@0.3.0`, so the host must provide
wasi:http. Under wasmtime, pass `-S http`.

The driver opens a path that starts with `http://` or `https://` as a
read-only stream of a response body. The open sends one GET request and
completes when the response head arrives. A status outside 200 to 299 fails
the open with the error number 10000 plus the status. A transport error
fails the open with the nearest errno, such as `ETIMEDOUT` or
`ECONNREFUSED`. Each read continues the body stream, and the last read also
resolves the trailers future of the response. At most 16 responses can be
open at one time. The `Aihc.Http` module of the `aihc-http` package uses this
path on the `wasi` operating system.

## Incremental compilation

Incremental compilation is the default. Each dependency SCC is compiled with
the complete linked program set and produces a relocatable Wasm object.
Objects are cached in target-specific library archives. Static objects and
info tables are data objects with link-time addresses, so no module needs an
initializer.

`--whole-program` remains available. It merges reachable dependency Core before
GRIN lowering and emits one generated-code object. Both modes compile the C
runtime and WIT bindings only at the final link and produce one component.
