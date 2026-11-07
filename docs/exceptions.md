# Exception handling

## Goals

AIHC exceptions must use the same continuation chain as every other control
transfer. The frames of this chain are on the thread stacks. They must not
introduce a native stack, a second exception stack, sentinel return values, or
backend-specific Haskell semantics.

The intended source interface is the `base` interface built around `Exception`,
`SomeException`, `throw`, `throwIO`, `catch`, `try`, `mask`, and `throwTo`.
Synchronous exceptions must work in pure evaluation and `IO`. Asynchronous
exceptions must be delivered only where the compiler and runtime have made the
current computation interruptible.

## Current state

`raise#` and `catch#` first lower to direct `GrinThrow` and `GrinCatch` nodes.
The FC and direct-GRIN interpreters retain their out-of-band reference
behavior. CPS-GRIN now eliminates both direct nodes: catches allocate an
explicit catch frame and raises become `GrinCpsRaise` transfers. The shared
runtime walks the continuation chain and every backend tail-resumes the
selected handler through its existing transfer convention.

Structural `Typeable` evidence and checked casts are implemented. The typed
library layer now provides `Exception`, `SomeException`, `throw`, `throwIO`,
selective `catch`, `handle`, `try`, their `Just`-selecting variants, and
`evaluate`. Nonmatching typed catches rethrow the original `SomeException`.
Class dictionaries retain superclass fields across separately compiled modules,
so the existential `Exception` dictionary supplies the `Typeable` evidence used
by `fromException`. Class default methods provide the standard empty user
instance declaration. AIHC does not have `Show` yet, so its initial `Exception`
class has the `Typeable` superclass and defaults `displayException` to the type
constructor name; adding `Show` can align that remaining superclass and default
with GHC without changing the exception representation.

The CPS pass now reifies the rest of a computation as an ordinary closure whose field zero
is always its parent continuation. CPS records a frame kind for every generated
continuation, GC-GRIN preserves it, and every backend emits it in `AihcInfo`.

Primitive and typed synchronous catch, nested rethrow, and update-frame
unwinding work. Masking, asynchronous delivery, and the cleanup combinators
work too. See "Asynchronous exceptions". `System.Timeout.timeout` forks a
watchdog thread as in GHC.

## Continuation-chain invariant

Every unapplied continuation closure must have its parent continuation in field
zero. The CPS pass must capture the parent even when a later optimization could
otherwise prove that field dead. The generated continuation function receives
the same field first. Update continuations change from `[blackhole, parent]` to
`[parent, blackhole]`.

Continuation info tables carry a small, backend-independent frame kind:

```text
normal          [parent, captures...]
catch           [parent, handler]
update          [parent, blackhole]
restore-mask    [parent, previous-mask-state]
stop            [no parent]
prompt          [parent, tag]
```

Only functions identified by `CpsGrinProgram` as continuations receive this
metadata. An ordinary closure is never treated as a frame merely because its
first field happens to be a pointer. The frame metadata must be preserved by
GC-GRIN and emitted into the shared `AihcInfo` ABI by every backend.

Appending a frame-kind field to `AihcInfo` is preferable to backend callbacks.
The unwinder then has a fixed set of portable operations, including on Wasm,
and arbitrary Haskell code is never run while the runtime is inspecting the
chain.

The invariant applies to unapplied frames. Applying a continuation may advance
its closure info table and append result fields, but no safe point is allowed in
that administrative transition before its generated entry tail-transfers.

## Synchronous exceptions

The CPS pass eliminates `GrinThrow` and `GrinCatch` rather than rejecting them.

For `catch# action handler`, with outer continuation `k`, it allocates a catch
frame containing `k` and `handler`, then applies `action` with that frame as its
continuation. Normal application of the frame forwards the action's result to
`k`.

For `raise# exception`, with current continuation `k`, it emits a CPS raise
transfer. The shared runtime walks `k`:

1. A normal frame is skipped by following field zero.
2. An update frame performs blackhole cleanup, then follows its parent.
3. A restore-mask frame restores the saved masking state, then follows its
   parent.
4. A catch frame is popped. The current mask state is saved in a restore-mask
   frame above its parent, the handler state becomes at least
   `MaskedInterruptible`, and the handler is applied to the exception with that
   restore frame as its continuation.
5. A stop frame reports an uncaught exception or terminates an unhandled child
   thread, depending on the stop kind.

Popping the catch frame before entering the handler is essential: an exception
raised by that handler must continue to the next outer handler. The restore
frame makes the implied mask around every exception handler hold on both normal
return and exceptional exit; an already `MaskedUninterruptible` handler remains
uninterruptible.

The runtime returns an ordinary `AihcResume`/portable transfer describing the
handler application. Native code tail-enters that transfer. It must not call a
handler and return through C or assembly frames.

The walk itself does not allocate or invoke GC. The machine roots the exception
and current frame for its duration. A caught frame can be retagged in place as
the restore-mask frame after its handler field has been loaded, and the current
blackhole object already reserves field zero for an indirection. Avoiding an
emergency allocator makes the unwinder valid even when the raise originated at
an allocation failure boundary.

### Update frames and blackholes

Unwinding an update frame cannot leave a blackhole owned by a computation that
no longer exists.

- A synchronous failure should replace the thunk with a small runtime raise
  thunk holding the exception, remove the blackhole record, and wake waiters.
  Re-entering the thunk then raises the same exception through the new current
  continuation.
- An asynchronously interrupted evaluation should restore the original thunk
  tag and remove the blackhole record. Waiters retry evaluation instead of
  permanently memoizing an interruption that was directed at another thread.

Waking a waiter therefore has two explicit resume forms. A memoized synchronous
failure resumes it with `Raise exception waiterContinuation`; an asynchronous
rollback resumes it with `Eval originalThunk waiterContinuation`. Neither path
pretends that the exception is the thunk's returned value.

The original thunk info pointer and environment survive the current blackhole
representation, so restoring the thunk requires no copy. The distinction is a
property of how the raise was initiated, not of the exception's Haskell type:
explicitly calling `throwIO UserInterrupt` is still a synchronous raise.

The current first implementation restores the original thunk and resumes
existing blackhole waiters by raising through each waiter's continuation. This
prevents a stranded blackhole and preserves retryability, but does not yet
memoize the synchronous exception for future entries. Installing the portable
raise thunk described above remains part of completing synchronous exception
semantics.

## `Typeable`, `Exception`, and `SomeException`

`SomeException` remains an ordinary Haskell existential node:

```haskell
data SomeException = forall e. Exception e => SomeException e
```

It requires no runtime tag. The contained `Exception e` dictionary retains the
operations and `Typeable e` evidence needed for display and typed projection.
The primitive catch frame catches every `SomeException`; the source-level typed
`catch` uses `fromException` and rethrows a nonmatching value.

`Typeable` must not base a sound cast on a user-assigned integer or an unchecked
hash. The initial implementation uses a structural representation:

```text
TypeRep(TyConIdentity, [argument TypeRep...])
```

The type checker synthesizes evidence from `TcType`. Evidence for `T a` is
composed from evidence for `a`, so polymorphic code can build representations
from given dictionaries. `cast` compares complete trees and invokes the one
trusted lifted-value `unsafeCoerce#` only after equality succeeds.

A `TyCon` carries the package, the module and the name of the constructor,
laid out as GHC's `GHC.Types.TyCon`, and two of them are equal when all three
agree. GHC compares fingerprints instead; aihc builds none, and the qualified
name identifies a constructor just as well. The kind arity and the kind
representation of a `TyCon` are placeholders: `Typeable` evidence does not
carry the kind of the constructor, and nothing in aihc reads either field
back.

The compatible library layer provides:

- automatic `Typeable` evidence, including functions, lists, tuples, and
  parameterized user types;
- superclass dictionary projection and class default methods where required by
  the standard `Exception` definition;
- `Exception`, `SomeException`, `toException`, `fromException`, and
  `displayException`;
- `throw`, `throwIO`, typed `catch`, `handle`, `try`, and `evaluate`.

`finally`, `bracket`, and the masking API remain coupled to the masking-state
step: exposing cleanup combinators before masking would give them the wrong
asynchronous-exception semantics.

The initial AIHC `Typeable` dictionary contains compiler-supplied `typeRep` and
`typeOf` fields. Once imported class-selector metadata is retained reliably,
`typeOf` should become the ordinary library wrapper used by `base`, leaving a
single representation field in the dictionary.

Default `toException` constructs `SomeException`; default `fromException`
structurally casts the contained value. User exception values and
`SomeException` are always ordinary GC-traced heap objects.

## Asynchronous exceptions

The runtime implements the GHC model: masking states, a synchronous
`throwTo`, delivery to a running thread through its heap limit, and frozen
evaluations in AP_STACK thunks.

### Per-thread state

Each `AihcThread` has these fields:

- `mask_state`, with the values of `getMaskingState#`: 0 unmasked, 1 masked
  uninterruptibly, and 2 masked interruptibly. A forked thread starts with
  the state of its parent, as in GHC.
- `blocked_kind`: the kind of the wait of the thread. For an IO wait,
  `blocked_on` names the IO request.
- `wait_epoch`: the number of cancelled waits. An MVar waiter, a blackhole
  waiter, and a throwTo message keep the epoch of their wait. The thread
  names none of these records. A thread record is in gen2, so a pointer
  from it would keep a dead waiter, its frames, and its MVar alive until a
  gen2 cycle.
- `pending_head` and `pending_tail`: the queue of throwTo messages. A
  message is an MVar waiter record: the sender, its continuation, and the
  exception.
- `async_exception`: the exception that the capability raises when the
  scheduler selects the thread next.

The object lock guards the wait fields and the queue. These fields are
fields of the thread record, which the collector traces, and not roots. A
store into them goes through the write barrier.

### Masking

`maskAsyncExceptions#`, `maskUninterruptible#`, and
`unmaskAsyncExceptions#` are scheduler primitives. The runtime pushes a
restore-mask frame `[parent, previous state]` above the continuation, sets
the new state, and applies the action. A wrapper closure of the CPS pass
evaluates the action first, as for `fork#`. The restore-mask frame sets the
previous state again when the action returns. An exception that unwinds
through it does the same. An unmask, or a return to the unmasked state,
raises a queued exception at once.

A catch handler runs masked interruptibly, or masked uninterruptibly when
the thread was. `aihc_raise` pushes a restore-mask frame above the parent of
the catch frame when the state changes.

### throwTo

`killThread#` is synchronous, as in GHC. A throwTo to the running thread
raises at once, also when the thread is masked. Otherwise the sender puts a
message on the queue of the target and waits until the target takes it. A
throwTo to a thread that ended returns at once.

Only the capability of a thread takes its messages, because the frames of
its stack name objects of that local heap. The capability of the sender
delivers a message itself when the target is on the same capability. For a
target on another capability, the sender puts the target on the
`async_targets` list of that capability. Then it sets the `interrupt` flag
of the capability and stores zero to its `heap_limit`. The next heap check
of the running thread there fails, and the runtime takes the interrupt. The
flag comes before the store, and each writer of the heap limit stores zero
again when the flag is set. Thus a refill cannot hide the interrupt.

A thread takes a message when it is unmasked, or when it is masked
interruptibly and waits. Every wait of this runtime is interruptible. A
waiting thread stops its wait when its epoch changes. The code that wakes
waiters skips a record whose epoch is not the epoch of its thread. The top
frame of the stack, which the wait noted, is the continuation. An IO
request loses its thread. A cancelled timer leaves the requests of the
capability at once. The thread then goes on the run queue,
and the scheduler raises the exception when it selects the thread. A
masked thread keeps its messages until it unmasks or starts a wait. The
scheduler checks a thread that starts a wait.

### Delivery and freezing

The delivery starts at the top of the stack of the thread. It walks the
frames to the first catch frame or stop frame. Each update frame ends one
segment. When the computation at the top of the segment can continue later,
the runtime copies the frames of the segment into an AP_STACK thunk, as
`control0#` copies frames. The thunk of the update frame then becomes an
indirection to the AP_STACK thunk. Its waiters evaluate it again. The top
of the next segment is the evaluation of that thunk. After the walk, the
exception unwinds the frames that are left, as a synchronous exception
does.

With several capabilities, the runtime freezes only a thunk of the local
heap. A global thunk would make the AP_STACK thunk and everything that its
frames name global. The work that continues later would then update global
thunks with local values, and each update would be an escape. That costs
more than the work again, so a global thunk goes back to its code.

An AP_STACK thunk holds the copies, the resumption at the top of the
segment, and the masking state there. Its entry claims the thunk and clears
the copies, as GHC blackholes an AP_STACK under evaluation. Otherwise the
copies would keep alive what the work consumes. The entry pushes the
copies above the continuation of the evaluation and continues the
resumption. When the masking state differs, it pushes a restore-mask frame
first.

The resumption at the top can continue later in these places:

- the entry check of the evaluation helpers, where the state is the value
  and its continuation;
- a scheduler primitive and a suspended thread, where the state is the
  resumption record. A cancelled wait has only its continuation, so its
  thunk goes back to its code;
- a replay reservation, where the state is the code and the arguments of
  the function. The replay helper puts them into a replay closure. Field
  zero of the closure is the index of the continuation argument. Field one
  is the code, field two is the static reference table of the function, and
  the arguments follow. One replay info table serves all functions of an
  argument shape in a unit. Its `srt` field is the runtime marker
  `aihc_replay_srt`, so the collector reads the table from field two (see
  `aihc_object_srt`). The closure entry gives the argument that was the
  continuation a new continuation, because the copies of the frames are at
  new addresses.

Another heap check cannot continue its computation later. At such a check,
an interrupt waits for up to 64 heap checks for one of the places above.
After that, the delivery starts at the highest live frame among the roots,
and the thunk of the first update frame goes back to its code.

### Context switches

The first `fork#` starts a ticker thread on a POSIX host. Every 20 ms it
sets the `context_switch` flag of each capability that runs, and it stores
zero to its heap limit. The inline thunk entry of compiled code also checks
the heap limit, so the next evaluation of a thunk reaches the evaluation
helper. The thread then goes to the end of the run queue after a poll of
its IO, and the next thread runs. A replay reservation also switches. A
WASI P3 host has no threads, so it has no ticker, and a long computation
does not stop there.

### Ctrl-C and host events

A POSIX signal handler must not allocate, traverse the heap, take a lock, or
enter generated code. It only records a `sig_atomic_t` event and wakes a blocked
poller using an async-signal-safe mechanism. The scheduler or the next
allocation poll converts that event into a pending `UserInterrupt` for the main
green thread. A preallocated exception value may be rooted by the machine so
this conversion does not depend on allocation at signal time.

The same runtime entry point should accept a user-interrupt event from non-POSIX
hosts. Wasm exception semantics remain identical even when a particular WASI
runner cannot translate terminal Ctrl-C into a component event. Deterministic
cross-backend tests use `throwTo`; signal-adapter tests are separate host
integration tests.

If a thread is blocked in the IO manager, a signal wakes the poll loop, removes
or cancels its request, and enqueues a raise resume. Request ownership and
buffer lifetime must still reach a terminal state; abandoning a continuation
must not leak a submitted request indefinitely.

## Delimited continuations

GHC's `newPromptTag#`, `prompt#`, and `control0#` reuse the same chain. Their
semantics are GHC's: `control0# tag f` captures the frames between the call
and the nearest prompt frame carrying `tag`, pops them together with the
prompt frame, and runs `f` with the captured part under the prompt's parent.
The captured part excludes the prompt, applying it installs none, and it may
be applied any number of times:

```text
E1[prompt# tag E2[control0# tag f]]  -->  E1[f (\m -> E2[m])]
```

`prompt#` is lowered by the CPS pass exactly like `catch#`: a prompt frame of
kind `prompt` holds the parent and the tag, and its normal application
forwards the action's result. Exception unwinding passes through a prompt
frame the way it passes through a normal frame. A prompt tag is a heap node
without fields, compared by address.

`control0#` is a runtime call. `GHC.Prim` defines it on top of two private
primitives so that the runtime never has to evaluate or apply a function in
more than one stage:

- `aihcControl0# tag g` walks the chain from the current continuation to the
  prompt frame. The frames between them are on the thread stack, and the
  stack reuses their bytes when the prompt pops. Thus the capture copies each
  frame to the managed heap. Each copy keeps its info table and captures, and
  links to the copy below it. The lowest copy has a null parent. A heap node
  records the top copy. The primitive then applies `g` to that node with the
  prompt's parent as continuation. `g`
  closes over the state token, so it has one application stage. The walk
  fails at an update frame, because the thunk it belongs to is not reentrant
  and marks the start of another state thread, and at a stop frame, because
  no prompt with the tag exists. GHC raises an exception in both cases; AIHC
  stops the program, since GHC documents that no program may rely on catching
  that exception.
- `aihcResume# captured m` pushes new copies of the recorded frames on the
  thread stack, from the bottom up. Each pushed copy links to the frame below
  it, and the lowest copy links where the prompt frame used to be. It then
  applies `m`, which the wrapper has already evaluated, under the pushed top.
  The heap copies are never written, which is what makes the capture
  multi-shot. Capture and resume each cost the length of the segment, as in
  GHC.

Masking state is not tracked yet, so nothing corresponds to GHC's handling of
mask frames inside a captured continuation. When restore-mask frames land,
capture must restore the prompt's mask state and a resume must install the
resumer's state at the bottom of the copied segment, as GHC does.

The GRIN interpreter keeps its continuations as host closures, so a catch
handler or a prompt held on the host stack would be lost when a capture
unwound past it. It therefore keeps an explicit control stack per thread,
whose entries mirror the native frame kinds: catch, prompt, update, and the
resume entry a re-pushed capture returns through. Returns pop, raises pop to
the nearest catch, and a capture pops to the prompt and keeps the entries
above it with the captured host continuation. See `ControlEntry` in
`Aihc.Grin.Interpret`.

## Uncaught exceptions

The main thread's stop frame routes an uncaught `SomeException` to a Haskell top
handler so `displayException` and exit status are portable. A child thread's
stop frame terminates that child and releases its runtime records; later work
may add the same reporting policy as GHC. Runtime corruption and impossible
frame layouts remain hard runtime failures, not catchable Haskell exceptions.

## Validation

The feature needs proof at several layers:

- CPS unit tests that every generated continuation has one parent link and that
  catch, update, mask, and stop frames have the declared layouts;
- CPS boundary tests showing that source throw/catch nodes are eliminated;
- runtime tests for nested handlers, handler rethrow, update-frame cleanup,
  masking restoration, blocked-thread removal, FIFO delivery, and GC while
  exceptions are pending;
- shared FC/GRIN fixtures for `Typeable`, `SomeException`, selective typed
  catches, pure exceptions forced by `evaluate`, and lazy handlers;
- backend code-generation tests for frame metadata and raise/poll transfers;
- portable examples compiled by every backend.

The `exceptions-sync` portable example uses two user exception types and nested
typed catches. The inner handler does not match, so `catch` rethrows the
original `SomeException` for the outer handler.

The remaining asynchronous example should:

- fork a worker, demonstrate masked cleanup and restore, send an exception with
  `throwTo`, and deterministically join through an `MVar`.

A separate POSIX test launches a looping executable, sends it `SIGINT`, and
checks `UserInterrupt` handling. It is not the semantic cross-backend test.

## Implementation sequence

1. **Complete:** land structural `Typeable`, exact comparison, and the trusted
   `unsafeCoerce#` primitive as an independent change.
2. **Complete:** make the continuation parent link and frame metadata explicit
   without changing behavior.
3. **In progress:** primitive and typed synchronous raise/catch lowering,
   `Exception`, `SomeException`, shared unwinding, and rollback-style update
   cleanup are implemented. Portable exception memoization remains.
4. **Complete:** masking state and restore frames, and the library masking
   and cleanup combinators.
5. **Complete:** per-thread message queues, interrupts through the heap
   limit, cancellation of waits, AP_STACK freezing, the ticker, and the
   `exceptions-async` examples.
6. Add POSIX Ctrl-C and other host event adapters.

Each step preserves the single-control-stack invariant and can be reviewed
without requiring every async source to land at once.
