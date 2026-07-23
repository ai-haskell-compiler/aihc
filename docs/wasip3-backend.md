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

## Runtime ABI

Generated functions have the uniform type `(i32) -> ()`. The argument is a
pointer to an array of 64-bit runtime slots. Generated functions select their
next operation by calling one of the `aihc_wasm_transfer_*` runtime helpers;
the P3 driver then runs the resulting trampoline until the program finishes or
all green threads are waiting for IO.

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
