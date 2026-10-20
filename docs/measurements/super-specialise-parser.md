# Parser super-specialization measurements

Date: 2026-10-20.

These measurements compare `-O2` with `-O2 --super-specialise`.
Both executables use compiler commit `b79d9c325983539619c67b226f9dfdc4442fccd2`.
Both use the default runtime and target `apple-arm64`.
Both builds passed System FC and backend lint.

The benchmark is `aihc-parser-stackage` from benchmarks commit `0f7db16a443af44db67c2e5d0c75b546e5a343f5`.
It uses the standard `benchmark.tsv` sample from the LTS 24.58 parser corpus.
This sample contains nine modules, with a 32 KiB source limit.
It does not measure the full corpus.

The host was worker-m1, with an Apple M1 and macOS 27.0.1 on AC power.
The run held the benchmark watcher's exclusive file lock.
The run used `aihc_bench.process.run_measured` from the same benchmarks commit.
Two warmup pairs preceded twenty measured pairs.
Each pair contained one run of each executable.
The order alternated between pairs.
Each process had `AIHC_RTS_STATS` set.
The measurement tool collected elapsed time, CPU time, RSS, instructions, cycles, and runtime statistics.

All 44 processes returned success with this exact output and no stderr:

```text
modules=9 ok=9 errored=0 imports=63 decls=124
```

The table gives medians from the twenty measured runs.
Negative changes indicate a decrease.
Binary sizes are file sizes after the link step, without a separate strip step.

| Metric | `-O2` | With super-specialization | Change |
| --- | ---: | ---: | ---: |
| Elapsed time | 23.963 ms | 25.124 ms | +4.84% |
| CPU time | 22.499 ms | 23.601 ms | +4.90% |
| Instructions | 251,589,208.5 | 257,211,228.5 | +2.23% |
| Cycles | 71,951,343.5 | 75,436,624 | +4.84% |
| Allocated bytes | 42,628,832 | 46,744,296 | +9.65% |
| Peak heap bytes | 6,680,736 | 6,784,984 | +1.56% |
| Peak RSS bytes | 17,358,848 | 17,104,896 | -1.46% |
| GC count | 12 | 13 | +8.33% |
| Binary bytes | 9,405,608 | 10,287,448 | +9.38% |

Elapsed times ranged from 23.796 to 25.494 ms for the baseline.
They ranged from 24.905 to 25.789 ms with super-specialization.
These short runs include process startup.
The instruction and allocation results also show an increase in work for this sample.
The result does not establish performance for other inputs.

Both plans now include the later top-level dictionary pass from PR #2790.
These measurements replace the earlier measurements from before that merge.
The combined pipeline shows a regression for this sample.
The option remains off by default.

The specialization pass made 1,990 copies and rewrote 3,330 calls.
Its limits prevented 4,372 further requests.
The pass ran after the first small inliner and before demand analysis.
General defunctionalization remained in GRIN conversion.

Build commands:

```sh
aihc build benchmarks/benchmarks/aihc-parser-stackage \
  -O2 --target apple-arm64 --lint --output /tmp/parser-baseline
aihc build benchmarks/benchmarks/aihc-parser-stackage \
  -O2 --super-specialise --target apple-arm64 --lint --output /tmp/parser-special
```

The measurement command passed this corpus directory to each executable:

```text
/nix/store/v5gx1nb2jgrg1mjwp2z0az66sdz09wvr-aihc-parser-corpus-lts-24.58
```

[Raw measurements](super-specialise-parser.json) contain all measured samples, executable hashes, and exact sizes.

## Confirmation run

A second run used the same executable hashes after a fresh check of origin/main at `6d69924fd`.
It used the same method, with two warmup pairs and twenty measured pairs.
All 44 processes produced the expected output.

| Metric | `-O2` | With super-specialization | Change |
| --- | ---: | ---: | ---: |
| Median elapsed time | 24.759 ms | 25.939 ms | +4.77% |
| Median CPU time | 23.221 ms | 24.272 ms | +4.53% |
| Median instructions | 251,109,479.5 | 256,882,728 | +2.30% |
| Allocated bytes | 42,628,832 | 46,744,296 | +9.65% |
| Binary bytes | 9,405,608 | 10,287,448 | +9.38% |

The second run confirms the regression on this sample.
[Confirmation samples](super-specialise-parser-confirmation.json) contain the complete results.
