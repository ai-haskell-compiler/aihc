# Parser super-specialization measurements

Date: 2026-10-21.

These measurements compare `-O2` with `-O2 --super-specialise`.
The compiler source is commit `a6ac517ec5aa23a45e774f0a545b84fc0c1749e1`.
It includes origin/main through `e7984c55f`.
Both executables use the default runtime and target `apple-arm64`.
Both builds passed System FC and backend lint.
A baseline rebuild with the final compiler produced the same executable hash.

The benchmark is `aihc-parser-stackage` from benchmarks commit `0f7db16a443af44db67c2e5d0c75b546e5a343f5`.
It uses the standard `benchmark.tsv` sample from the LTS 24.58 parser corpus.
This sample contains nine modules, with a 32 KiB source limit.
It does not measure the full corpus.

The host was worker-m1, with an Apple M1 and macOS 27.0.1 on AC power.
Each run held the benchmark watcher's exclusive file lock.
The measurement tool was `aihc_bench.process.run_measured` from the same benchmarks commit.
Two warmup pairs preceded twenty measured pairs.
Each pair contained one run of each executable.
The order alternated between pairs.
Each process had `AIHC_RTS_STATS` set.
The tool collected elapsed time, CPU time, RSS, instructions, cycles, and runtime statistics.

All 44 processes returned success with this exact output and no stderr:

```text
modules=9 ok=9 errored=0 imports=63 decls=124
```

The table gives medians from the twenty measured runs.
Negative changes indicate a decrease.
Binary sizes are file sizes after the link step, without a separate strip step.

| Metric | `-O2` | With super-specialization | Change |
| --- | ---: | ---: | ---: |
| Elapsed time | 24.042 ms | 23.630 ms | -1.71% |
| CPU time | 22.536 ms | 22.123 ms | -1.83% |
| Instructions | 251,067,915 | 228,129,340 | -9.14% |
| Cycles | 71,817,696.5 | 70,458,079.5 | -1.89% |
| Allocated bytes | 42,627,312 | 41,448,752 | -2.76% |
| Peak heap bytes | 6,612,072 | 6,233,496 | -5.73% |
| Peak RSS bytes | 17,309,696 | 16,482,304 | -4.78% |
| GC count | 12 | 11 | -8.33% |
| Binary bytes | 9,405,608 | 9,122,648 | -3.01% |

Elapsed times ranged from 23.873 to 28.548 ms for the baseline.
They ranged from 23.384 to 26.075 ms with super-specialization.
These short runs include process startup and have occasional outliers.
The instruction and allocation results also show less work for this sample.
The result does not establish performance for other inputs.

## Confirmation run

A second run used the same executable hashes and measurement method.
It used two warmup pairs and twenty measured pairs.
All 44 processes produced the expected output.

| Metric | `-O2` | With super-specialization | Change |
| --- | ---: | ---: | ---: |
| Elapsed time | 24.164 ms | 23.678 ms | -2.01% |
| CPU time | 22.625 ms | 22.215 ms | -1.81% |
| Instructions | 251,355,288 | 228,223,806 | -9.20% |
| Allocated bytes | 42,627,312 | 41,448,752 | -2.76% |
| Binary bytes | 9,405,608 | 9,122,648 | -3.01% |

[Raw measurements](super-specialise-parser.json) and [confirmation samples](super-specialise-parser-confirmation.json) contain all measured samples, executable hashes, and exact sizes.

## Pipeline repair

The previous pipeline increased elapsed time by 4.77% in its confirmation run.
Late dictionary cleanup exposed strict calls after demand analysis had finished.
The optional plan now repeats demand analysis and simplification after that cleanup.
Common-subexpression elimination, full laziness, and constant lifting then restore sharing.
Dictionary producers remain generic to preserve shared identities for later consumer specialization.

Local allocation measurements isolated these effects:

| Optional pipeline | Allocated bytes |
| --- | ---: |
| Extra sharing passes only | 46,396,976 |
| Extra demand analysis, simplification, and sharing passes | 42,705,480 |
| Those passes with generic dictionary producers | 41,458,464 |

All three executables produced the expected output.
These local allocations have small environment-related differences from the worker-m1 measurements.
The table does not compare local elapsed times.

The final early pass made 1,255 copies and rewrote 1,793 calls.
Its limits prevented 2,225 further requests.
The pass still runs after the first small inliner and before demand analysis.
General defunctionalization remains in GRIN conversion.
The option remains off by default.

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
