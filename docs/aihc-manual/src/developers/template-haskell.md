# Template Haskell design

This page gives the design of Template Haskell in AIHC.
The compiler runs a splice in an expression and a splice in the position of a declaration, as this page describes.
The other parts of the design are a plan.

The first implementation has these limits:

- A splice cannot use a module of its own package. Thus, a quote in the body of a splice cannot use a name of its own package.
- The compiler changes an expression quote into an expression that builds its syntax tree, and a name quote into a Template Haskell name. A quote with a construct outside the supported subset, and each other form of quote, stays a run-time error.
- A variable that a quote binds has a name with no module, so a splice result can capture it.
- The result of a splice can only have the expressions, declarations, types, and patterns that `Aihc.Th.Convert` handles.
- A declaration splice runs before name resolution of its module, as an expression splice does. Thus, it cannot use a declaration of its own module.
- A splice can report a warning or an error, make a unique name, and use `recover`, `getQ`, and `putQ`. It cannot ask the compiler for information, for example with `reify`.
- The compiler builds one image for each module that has splices.

## Goals

GHC lets splice code read and change the build machine.
AIHC must not permit this.
AIHC must also run splices when the compiler runs in a browser.

Thus, AIHC runs each splice in a WebAssembly sandbox.
The sandbox protects the build machine only.
A splice can still generate hostile code for the final program.

A package that uses Template Haskell needs two builds of its dependencies.
One build is for the target.
One build is for the sandbox.
A slow build is acceptable.

## Execution model

### Base image

The base image contains the run-time system, `template-haskell`, and the dependency closure.
`wasm-ld` links it as a core module, not as a component.
The image imports only the functions of the splice protocol.
It has no WASI imports.
The run-time system gets a host layer for this image, as it has for POSIX and for WASI.

The base image uses the store entries of the `wasm32-wasip3` target.
The compiler builds these entries at `-O0`.
The link keeps each exported symbol, because a splice can refer to each of them.

### Splice modules

The compiler compiles the splices of one declaration group into one small module.
The WebAssembly backend has a position-independent mode for this module.
`wasm-ld -shared` links the module as a shared module.
The module imports the memory and the function table of the instance.
The engine driver supplies the other imports from the exports of the base image.

Earlier modules of the same package load in the same way.
Thus, a new module does not cause a new link of the base image.

### Instance life

Each module that has splices gets one instance.
The instance lives until the compiler completes the module.
The state of the `Q` monad stays in the heap of the instance.
Modules stay isolated from each other, and parallel builds stay deterministic.

## Splice protocol

The program of the base image is the guest.
The guest asks the driver for a closure, runs the closure, and asks again.
A closure is a static value of the type `IO ()` in a shared module.
The compiler makes one closure for each splice.
The closure applies `runSplice` to the body of the splice.

The guest calls the driver through imported functions.
A call returns when the driver has the answer.
The driver does not call into the guest, and the guest does not call into the compiler.
Thus, the guest needs no coroutine monad: its `Quasi` monad is above `IO`.

On `node`, the driver reads its commands synchronously.
A browser worker cannot do that.
There, the driver must suspend the guest during a call, for example with JavaScript Promise Integration.

A request of a splice is one such call.
The guest sends the request as a tree and waits for the reply.
The driver gives the request to the compiler before the reply of the closure, and the compiler answers it.
The first request is a report of a warning or of an error.
Later requests are `Reify` and `ReadFile`.

The guest does some operations without a request.
A new unique name gets its number from a counter in the instance.
The state of `getQ` and `putQ` is in the instance, which runs the splices of one module.
The reports of the code in a `recover` stay in the instance until that code completes with no error.

The wire format is specific to AIHC.
The guest encodes a value as a tree of constructor names, with one child for each field.
Thus, the guest encoder has no knowledge of the Template Haskell AST.
The host decodes the tree directly into the surface AST.
The host encodes the result of `reify` directly from the data of the type checker.
Thus, the compiler does not depend on the types of `template-haskell`.

## Engine

The engine has a small interface: make an instance, load a module, start the guest, and run a closure.

A JavaScript driver implements this interface.
On a native host, the driver runs on `node` as a child process of the compiler.
In a browser, the same driver runs in a worker.
The Nix environment pins the version of `node`.

## Security rules

- Each splice has a fuel limit and a memory limit.
- Each effect of `runIO` is a request, and the host can refuse it.
- The host permits a splice to read files in the source tree of its package.
- The host records each file that a splice reads as a dependency of the module.
- The host does not permit processes, network access, or environment variables.
- The clock gives a fixed time.

A package that starts a process in a splice does not compile.
An example is a package that reads the Git revision.

Execution is deterministic.
Thus, the compiler can keep the result of a splice in a cache.
The key is the base image, the code of the splice, and the replies of the host.

## Two platforms

Splice code runs on `wasm32`.
The result of a splice describes code for the target.

- Platform values in a splice are the values of `wasm32`.
  Examples are `sizeOf` and `System.Info.os`.
- CPP and Cabal conditions can give a package a different API on `wasm32`.
  The first implementation assumes that the two APIs are the same.
- A package has a different identity on each platform.
  The host maps a global name in a result to the target package with the same name and version.
- The first implementation builds all dependencies of a component for the sandbox.
  The component must enable `TemplateHaskell` or `QuasiQuotes`.

## Front-end changes

A top-level splice divides a module into declaration groups.
A driver processes the groups in sequence.
For each group, it runs name resolution and the type checker.
Then, it runs the splices and continues with the next group.

A splice in an expression runs before name resolution of its group.
This is possible because the body of a splice can only use imported names.

An expression quote changes after name resolution, because the form of each variable depends on its binder.
A top-level name becomes a Template Haskell name that is bound to its package and its module.
A variable that the quote binds becomes a name with no module.
A variable that the code outside the quote binds becomes a call of `lift`.
The new expression has names that are not resolved, so name resolution runs again on a unit that has a quote.

A name quote, `'f` or `''T`, changes in the same step.
It becomes the Template Haskell name of the term or of the type, bound to its package and its module.

The result of a splice can have a name that is bound to its package and its module.
The conversion gives such a name a mark, and name resolution gives the name that term or type in each scope.

| Component | Change |
| --- | --- |
| `aihc-parser` | No change. The parser accepts all Template Haskell forms. |
| `aihc-resolve` | Accept one declaration group at a time. Keep the resolution of a name that a splice gives as a global name. |
| `aihc-tc` | Accept one declaration group at a time. Supply a query interface for `reify`. |
| `aihc-th` (new) | Change a quote into an expression that builds the Template Haskell AST. Convert the result of a splice into the surface AST. Own the wire format. |

The first implementation gives a local variable in a quote a dynamic name.
A splice can thus capture such a variable.
A later change will correct this.

## Sequence of work

1. Link a base image as a core module with the new host layer.
2. Add the position-independent mode to the WebAssembly backend.
3. Load a shared module into a live instance, call a function that allocates, and force a collection.
4. Add the coroutine `Quasi` instance and the wire format.
5. Run the splice `$(litE (integerL 42))` from the source text to the final program.
6. Add declaration splices, quotes, `reify`, and the file rules.

Steps 1 to 3 have the highest risk.
If step 3 fails, the alternative is an interpreter for splice code in the base image.
