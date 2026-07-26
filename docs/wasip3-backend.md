# WASI P3 backend

The `wasm32-wasip3` target consumes GC-GRIN and emits LLVM MC's WebAssembly
assembly syntax. This is WebAssembly machine code: the backend selects Wasm
instructions, locals, structured control flow, data objects, and the runtime
ABI directly. Clang's integrated Wasm assembler only serializes those
instructions and records relocations; generated Haskell code does not pass
through C or LLVM IR.

The build pipeline uses temporary linker inputs:

```text
dependency GC-GRIN -> WebAssembly assembly -> cached dependency objects
main GC-GRIN       -> WebAssembly assembly -> program.o
C runtime + P3 IO backend       -> runtime objects
WIT C bindings                  -> binding object
all objects -> wasm-ld -> core module -> wasm-tools -> component
```

The resulting output is one WebAssembly component. The object files and the
intermediate core module are removed after linking.

The driver invokes the standard LLVM tools directly: `clang
--target=wasm32-unknown-unknown`, `wasm-ld`, `wasm-tools`, and `wit-bindgen`.
They may come from any LLVM/WASI installation on `PATH`; no `wasm32-clang`
wrapper is required. `AIHC_WASM_CLANG` can select another Clang executable
when a host toolchain wrapper is not cross-target safe. The Nix development
environment uses that override to select its unwrapped LLVM Clang.

## Runtime ABI

Generated functions have the uniform type `(i32) -> ()`. The argument is a
pointer to an array of 64-bit runtime slots. Generated functions select their
next operation by calling one of the `aihc_wasm_transfer_*` runtime helpers;
the P3 driver then runs the resulting trampoline until the program finishes or
all green threads are waiting for IO.

Each GRIN variable is assigned an `i64` WebAssembly local. Values are copied to
linear memory only at ABI boundaries that require an address: outgoing runtime
argument vectors and the live-root vector of a `GrinEnsureHeap` safepoint. Each
generated object owns a private scratch buffer sized for its largest such
vector. After a moving collection, generated code reloads the relocated roots
into their result locals. Ordinary variable access therefore compiles to
`local.get` and `local.set`, without C accessor calls or runtime-allocated local
arrays.

Runtime info tables are ordinary relocatable data objects. Function addresses
in those tables become Wasm table indices when `wasm-ld` links the program and
runtime. Heap pointers remain 32-bit Wasm addresses represented in the shared
64-bit slot type used by the other backends.

## Asynchronous stdout

The initial P3 IO backend implements stdout writes with
`wasi:cli/stdout@0.3.0`. It creates a `stream<u8>`, supplies its readable end to
`write-via-stream`, and incrementally writes the AIHC IO buffer through the
writable end. When the stream or result future blocks, the exported async
`wasi:cli/run@0.3.0` callback returns `WAIT(waitable-set)`. A later callback
finishes the request, makes its green thread runnable, and resumes the same
generated-code trampoline.

The `System.IO` `stdout` handle uses this path, including its `MVar`-serialized
handle state and native-width `Int` FFI results. The current WIT world does not
import stdin, stderr, or filesystem interfaces. Those fixed handles still
exist, but unsupported operations and `openBinaryFile` report an IO error; an
uncaught `IOException` traps because the component has no synchronous error
stream.

## Incremental compilation

Incremental compilation is the default. Each dependency SCC is compiled with
the shared `LinkLayout` into a relocatable Wasm object and a uniquely named
initializer. Objects are cached in target-specific library archives. The main
object allocates the shared global table, installs nullary constructors, calls
the dependency initializers, and then initializes its own globals before
starting the program.

`--whole-program` remains available. It merges reachable dependency Core before
GRIN lowering and emits one generated-code object. Both modes compile the C
runtime and WIT bindings only at the final link and produce one component.
