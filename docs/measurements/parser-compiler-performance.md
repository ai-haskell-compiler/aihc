# Parser compiler performance

Date: 2026-10-25.

These measurements concern compiler execution, not benchmark execution.
The input is `aihc-parser-stackage` from benchmarks commit `0f7db16a443af44db67c2e5d0c75b546e5a343f5`.
The target is `apple-arm64` with `-O2` and super-specialization.
The host is an Apple M4 Pro with 12 CPU cores and 48 GiB of memory.
GHC 9.12.4 built both compiler binaries with the same Cabal configuration.

The measurement base is `0d9b42672`.
Both measured binaries used `-O2 --super-specialise`.
The branch also includes `2ad0a795e`, which makes the same pass sequence the default at `-O2`.
That commit removes the separate option.

## Method

The first build created the dependency artifacts.
The measured builds reused these artifacts and repeated whole-program compilation.
The benchmark sources did not change.

Both binaries used the same store, build directory, output path, and fixed `AIHC_COMPILER_IDENTITY` value.
This identity permitted both binaries to read the same input artifacts.
Before each run, the measurement script deleted `lto/program.hash`.
Thus, every run repeated optimization, GRIN analysis, native code generation, and the link step.
No measured run reused the program object.

Each compiler ran twice with four runtime capabilities.
The runs alternated between the baseline and the changed compiler.
No other build or test ran during these measurements.
The table gives the mean of the two runs.
`/usr/bin/time -l` supplied elapsed time, CPU time, instruction counts, and peak resident memory.
GHC RTS statistics supplied allocation counts.

| Metric | Baseline | Changed compiler | Change |
| --- | ---: | ---: | ---: |
| Elapsed time | 55.44 s | 47.60 s | -14.1% |
| CPU time | 60.06 s | 52.97 s | -11.8% |
| Instructions | 762.60 billion | 656.09 billion | -14.0% |
| Heap allocation | 120.48 GB | 107.91 GB | -10.4% |
| Peak resident memory | 1.321 GB | 1.350 GB | +2.2% |

GB denotes decimal gigabytes.
Individual elapsed times were 55.78 and 55.09 seconds before the change, and 47.97 and 47.23 seconds after it.

One additional pair used the default runtime setting, `-N`, with all 12 cores.
Elapsed time decreased from 57.83 to 51.30 seconds, or 11.3%.
The program objects were identical.

Verbose logs also show these mean stage times:

| Stage | Baseline | Changed compiler |
| --- | ---: | ---: |
| FC optimization and final prune | 34.68 s | 30.74 s |
| GRIN points-to analysis | 10.49 s | 6.74 s |
| Backend after points-to rewrites | 6.94 s | 6.45 s |

The final row includes CPS conversion, GC conversion, LIR conversion, and native emission.

## Changes

The fresh-name scan previously constructed sets of all reference names and binder names before it computed their maximum unique number.
Many optimization passes repeated this work.
The scan now computes the maximum directly and preserves the previous selection of names.
A comparison on all 874 saved FC files gave identical maximum values.

The points-to solver previously traversed each complete location set to check its size after an update.
This benchmark requires 6,112,168 solver iterations.
The solver now stores each set size beside its set.
An update counts only new locations.
A node with one input uses the size of its predecessor.
The analysis retains the same iteration order, widening decisions, and rewrites.

LIR conversion and native emission previously ran in sequence for each item.
They now run in parallel when the runtime has more than one capability.
A bounded queue holds at most one item between the producer and consumer.
The consumer preserves source order and controls the object writer.
An exception stops both tasks before the object writer publishes the object.
The single-capability path remains sequential.

## Output and further parallel work

Every measured run produced a program object identical to the baseline object.
The isolated points-to measurement also produced identical GRIN text and identical analysis and rewrite counts.
These changes do not reduce optimization quality.
No tests or fixture expectations changed.
Progress counts do not change.

The module task graph and FC file reads already use parallel tasks.
The whole-program passes require the output of the previous pass.
The inliner also updates use counts and candidate bodies after each function.
Parallel function processing there would require a separate design to preserve those decisions.

## Repeat the measurement

Build and save each compiler binary before the measurement.
Use separate compiler binaries with one copied benchmark package and one artifact directory.
For the old baseline, add `--super-specialise` to the command below.
For the current compiler, `-O2` includes that pass sequence.

```sh
rm -f "$work/build/arm64-macos-apple/exe/aihc-parser-stackage/lto/program.hash"
AIHC_COMPILER_IDENTITY=parser-compiler-perf \
  /usr/bin/time -l "$compiler" build "$work/package" \
  --target apple-arm64 -O2 \
  --store "$work/store" --build-root "$work/build" \
  -v -o "$work/out" +RTS -N4 -s -RTS
```

Keep the source tree, GHC build configuration, environment, and compiler identity fixed between runs.
Exclude the first build from the warm measurements.
Use `cmp` to compare `lto/program/program.o` after each run.
