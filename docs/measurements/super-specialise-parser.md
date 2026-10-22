# Parser super-specialization measurements

Date: 2026-10-22.

These measurements compare `-O2` with `-O2 --super-specialise`.
The compiler source includes origin/main through `498fdec1d`.
Both executables use the default runtime and target `apple-arm64`.
Both builds passed System FC and backend lint.

The benchmark is `aihc-parser-stackage` from benchmarks commit `815c970`.
The input is the full LTS 24.58 parser corpus, with all 34,811 lines of `modules.tsv`.
The corpus directory is `/nix/store/v5gx1nb2jgrg1mjwp2z0az66sdz09wvr-aihc-parser-corpus-lts-24.58`.
The host was an Apple M-series computer with macOS 27.

Each executable ran three times with the `--report` argument.
The runs alternated between the two executables.
`/usr/bin/time -l` gave the time, instruction, cycle, and memory counts.
All six runs produced the same output.
The output ends with this line:

```text
modules=34811 ok=34809 errored=2 imports=239569 decls=822877
```

The table gives the mean of the three runs.
Negative changes indicate a decrease.
Binary sizes are file sizes after the link step, without a separate strip step.

| Metric | `-O2` | With super-specialization | Change |
| --- | ---: | ---: | ---: |
| Elapsed time | 55.09 s | 55.12 s | +0.05% |
| CPU time | 51.50 s | 52.20 s | +1.35% |
| Instructions | 784,920,564,321 | 782,242,971,331 | -0.34% |
| Cycles | 209,646,054,315 | 212,002,489,970 | +1.12% |
| Peak memory bytes | 276,966,707 | 245,383,840 | -11.40% |
| Binary bytes | 10,707,768 | 11,040,088 | +3.10% |

The option has no clear effect on speed for this benchmark.
It decreases peak memory.

## Defects found by the measurement

The first measurement of this branch, after the merge of main, gave these changes:

| Metric | Change |
| --- | ---: |
| Instructions | +11.6% |
| Cycles | +15% |
| Peak memory | -15% |
| Binary bytes | -19% |

The time profile showed that the inliner no longer copied some small functions.
Examples are `withSpanAnn`, `tokenSatisfy`, `withHints`, and `isExtensionEnabled`.
These functions became calls in the executable.

Three defects in the pass caused this result:

1. The pass made local copies on dictionaries that a parameter decides.
   For example, `withHints` received a local copy of `Data.Set.insert` on the `Ord` superclass of its `Stream` parameter.
   The copy could not select a method, and it made `withHints` too large to inline.
2. The pass put each local copy in a recursive group.
   The inliner does not copy a recursive binding.
   Thus, a local copy of `getParserState` in `withSpanAnn` stayed a call, and `withSpanAnn` grew from 60 to 110 lines.
3. The pass gave each anonymous function argument a local name, also at calls that it did not change.
   Thus, the inliner saw a variable where it had seen an anonymous function.

On a stride of every 17th corpus module, the instruction increase was 10.8% before the corrections.
The first and third corrections together decreased it to 6.4%.
The second correction removed the remaining increase.
