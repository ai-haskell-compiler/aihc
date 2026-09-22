# Foreign callbacks

AIHC supports `foreign import ccall "wrapper"` and `foreign import ccall "dynamic"` on its compiled backends.
These backends include `wasm32-wasip3`, Apple ARM64, Linux AMD64, and LLVM.
The GRIN interpreter does not support these imports.

A wrapper converts a Haskell function or IO action to a C function pointer.
A dynamic import calls a C function pointer.
The callback type and the function pointer type must agree.

```haskell
import Foreign.C.Types (CInt)
import Foreign.Ptr (FunPtr, freeHaskellFunPtr)

type Callback = CInt -> IO CInt

foreign import ccall "wrapper"
  makeCallback :: Callback -> IO (FunPtr Callback)

foreign import ccall safe "dynamic"
  callCallback :: FunPtr Callback -> Callback
```

Callbacks can capture Haskell values.
The runtime protects those values from garbage collection until `freeHaskellFunPtr` releases the callback.
The runtime also protects the suspended Haskell caller during a callback.
Callbacks can allocate, use mutable references, and call other callbacks.

Each compiled module has 64 callback slots for each wrapper import that it uses.
Each call to a wrapper takes one slot.
A full pool causes a runtime error.
`freeHaskellFunPtr` releases the slot for reuse.

Release each callback after C no longer needs it.
Do not call a released function pointer.

## Types

Callbacks support the scalar C ABI types that ordinary foreign imports support.
These types include integers, floating point values, and pointers.
The compiler converts supported boxed types and newtypes to their C representations.
A callback can return a unit value through a C `void` result.
An IO callback can have no C arguments.
Callbacks cannot use byte arrays as argument or result values.

## Runtime limits

A C call that enters Haskell must use `safe` or `interruptible`.
A foreign import without a safety mark uses `safe`.
An `unsafe` C call that enters Haskell causes a runtime error.

A callback must complete synchronously on the runtime thread.
A callback that requires a scheduler switch causes a runtime error.
This limit includes an empty `MVar`, `yield`, and host I/O that must wait.
An uncaught callback exception causes a runtime error.
External C threads cannot call these callbacks.

For a safe C call, use pinned byte arrays for any byte array arguments.
Keep all other C memory valid for the complete call.

## WASM

The compiler generates fixed callback functions and stores their function-table indices in `FunPtr` values.
Each callback function selects its own protected Haskell closure.
This method needs no JavaScript and no runtime code generation.

The C library must be part of the same linked WASM module.
A function-table index is not a host function pointer or a component interface.
Host callbacks need a separate host interface.

Callback support alone does not make `terminfo` portable to WASI.
That package also needs its C library and terminal database.
Its output callback can require I/O suspension, which this implementation does not support.
