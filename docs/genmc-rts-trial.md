# GenMC runtime trial

GenMC can execute selected functions from the actual C runtime.
It detects an injected race inside `aihc_mvar_try_put`, without manual yield points.
It cannot yet execute full runtime startup with this trial.
The current limits are Lir linkage and unsupported host and allocation calls.

This trial supports the test-system discussion in
[multi-threaded-rts-design.md](multi-threaded-rts-design.md).
It changes no production runtime code.

## Environment

The trial ran on ARM64 macOS on 13 October 2026.
The runtime source revision was `1ef9d84b9`.
GenMC reported version 0.19.0, with LLVM 19.1.7.
The GenMC source revision was
`9f6c4c0772d0c42b325681581acc6a0f3eb9b5f7`.
The trial uses the repository Nix pin and a separate GenMC source pin.
It does not change the development shell or the normal test suite.

## Results

Every successful control case used RC11.
The trial disabled automatic selection of a stronger memory model.

| Probe | Result | Evidence |
| --- | --- | --- |
| Stable-name counter with a test mutex | Pass, two complete executions | Two calls produce different identifiers. |
| Stable-name counter without that mutex | Non-atomic race, exit 42 | The trace identifies reads and writes in `aihc_stable_name_take_hash`. |
| MVar with a test mutex | Pass, two complete executions | Exactly one put succeeds. One take succeeds, and the next take fails. |
| MVar without that mutex | Non-atomic race, exit 42 | The trace identifies the `full` read and write inside `aihc_mvar_try_put`. |
| MVar race with seed 17, twice | Same diagnostic trace | The traces match after removal of elapsed time. |
| MVar control after the fault runs | Pass | The mutex restores the control result. |
| Full machine initialization | Unsupported call, exit 17 | `aihc_rts_nursery_bytes` has no definition in the linked C units. |
| POSIX region allocation | Unsupported call, exit 17 | The interpreter rejects the Darwin `mmap` symbol. |
| POSIX monotonic clock | Unsupported call, exit 17 | The interpreter rejects `clock_gettime`. |
| Write barrier outside the nursery | Unsupported call, exit 17 | Remembered-set growth reaches `realloc`. |
| Small pinned allocation | Unsupported call, exit 17 | The allocator reaches `calloc`. |

The initial MVar control and fault runs each took about 0.01 to 0.02 seconds inside GenMC.
These numbers exclude source compilation and the GenMC build.
They do not predict the cost of larger workloads.

## What the MVar probe executes

The driver supplies a 256-byte nursery and an empty machine record.
It calls the actual `aihc_mvar_new`, which uses the actual `aihc_gc_allocate`.
Two POSIX test threads then call the actual `aihc_mvar_try_put` on that MVar.
Each call executes the actual write barrier.
The object is in the nursery, so the barrier needs no remembered-set allocation.
After both threads finish, the driver calls `aihc_mvar_try_take` twice.

The test-side mutex serializes each complete runtime call in the control case.
The `INJECT_RACE` macro removes that mutex from both workers in the fault case.
GenMC reports the conflict between `if (mvar->full)` and `mvar->full = 1` in the runtime source.
The driver contains no yield points.

This is deliberate concurrent use of a sequential runtime.
It is not evidence of a defect in the current supported execution mode.
The mutex belongs to the experiment and is not a proposed RTS implementation.

The probe does not execute a collection, waiter wake-up, or Haskell continuation.
It does not verify the future threaded RTS.
The stable-name probe provides a smaller independent diagnostic check.

## Required preparation

The first direct compilation failed because GenMC supplies reduced C library headers.
Those headers omit declarations for `calloc`, `realloc`, and `qsort`.
The trial adds declarations only.
It provides no replacement implementation for these functions.

GenMC's `errno.h` defines `errno` in each translation unit.
The trial compiles the units with `-fcommon` so that LLVM can link them.
This is a tool-header accommodation, not a change to the RTS memory model.

An unpruned module failed on the external global `aihc_indirection_info`.
The trial makes definitions internal, except `main`, and removes unreachable code with LLVM `globaldce`.
This retains code reachable from the selected probe.
It does not supply definitions for missing runtime functions.
Full startup still fails when it calls a required Lir function.

The default GenMC function inliner consumed substantial time on the linked runtime.
The startup process used approximately 3.9 GB after about 25 seconds.
The trial stopped those runs and disabled the function inliner.
The selected probes then completed promptly.
This observation is not a timeout-based test result.

The first Nix build failed in CMake's C++ module dependency scan.
The scan could not find standard C++ headers through the compiler wrapper.
`CMAKE_CXX_SCAN_FOR_MODULES=OFF` allowed the build to complete.

## Reproduce the trial

Run this command from the repository root.
Use an output directory that does not exist.

```sh
nix develop --impure --file scripts/experiments/genmc/shell.nix \
  --command python3 scripts/experiments/genmc/run.py \
  --output /tmp/aihc-genmc-results
```

The output contains LLVM modules, compiler logs, GenMC logs, error graphs, and a JSON manifest.
The manifest records commands, revisions, and exit codes.
The runner requires both a matching exit code and a diagnostic for each expected failure.
It also checks the runtime source location in each race report.
An unsupported call does not count as successful verification.
Each subprocess has a 60-second resource limit.
The runner reports an error if that limit expires.

The fault variants differ only in the test driver's mutex calls.
The runner compiles the same production runtime sources for all variants.
It requires no runtime patch and leaves no injected fault in the source tree.

## Limits and next steps

The result establishes feasibility for small C runtime operations with controlled initial state.
It does not establish support for arbitrary compiled Haskell programs or native machine code.
Replay applies to the same LLVM module, GenMC version, configuration, and seed.
The trial has not established replay across hosts or tool versions.

The missing Lir function is a linkage gap in this trial.
A further trial can link actual LLVM output from the Lir runtime.
The POSIX and allocation failures are separate interpreter limits.
They remain even when a probe bypasses machine initialization.
Support for those interfaces needs implementation or an explicit host contract before full-runtime tests become useful.

The trial does not replace unsupported functions with mocks.
It has not tested condition variables, IO completion, or foreign callbacks.
It has not tested weak-memory publication failures.
The injected defects are ordinary data races.

The next useful step is a small test of an actual concurrent RTS protocol under GenMC.
It should retain real protocol code and use supported memory and thread operations.
Full deterministic simulation through WASM remains a separate option for broader program execution.

## Sources

- [GenMC source and build instructions](https://github.com/MPI-SWS/genmc)
- [GenMC usage and diagnostics](https://github.com/MPI-SWS/genmc/blob/master/doc/manual/usage.md)
- [GenMC supported APIs](https://github.com/MPI-SWS/genmc/blob/master/doc/manual/api.md)
