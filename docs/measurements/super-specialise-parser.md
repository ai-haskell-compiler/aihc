# Parser super-specialization measurements

Date: 2026-10-20.

These measurements compare `-O2` with `-O2 --super-specialise`.
Both executables use compiler commit `ddbbc9335894a032ef87ce22187cf3f8f88ac3a0`.
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
| Elapsed time | 24.554 ms | 23.726 ms | -3.37% |
| CPU time | 23.002 ms | 22.225 ms | -3.38% |
| Instructions | 256,661,544 | 248,832,604.5 | -3.05% |
| Cycles | 73,284,013 | 70,808,837.5 | -3.38% |
| Allocated bytes | 47,929,632 | 43,515,768 | -9.21% |
| Peak heap bytes | 6,687,000 | 6,538,728 | -2.22% |
| Peak RSS bytes | 17,399,808 | 16,793,600 | -3.48% |
| GC count | 13 | 12 | -7.69% |
| Binary bytes | 9,406,392 | 10,154,504 | +7.95% |

Elapsed times ranged from 24.322 to 26.125 ms for the baseline.
They ranged from 23.553 to 24.030 ms with super-specialization.
These short runs include process startup.
The instruction and allocation results also show a decrease in work for this sample.
The result does not establish performance for other inputs.

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
