# Revision history for miniterion

## 0.1.2.1 -- 2026-02-20

* Show time from OLS linear regression, R², and range values in
  benchmark results.

* Show ``variance introduced by outliers ...`` message.

* Show outlier counts with ``-v2`` option.

* Modify CSV format to follow the template used in Criterion.

* Modify timeout detection to refer to the benchmark start and end
  time instead of the sum of the measurement durations.

* Modify glob pattern match to support brace expressions.

* Modify the output of ``-v3`` option to show divided time.

* Modify ``--verbosity`` option to recognize verbosity level 3.

* Perform minor GC after running target function to update RTSStats.

* Add `--output` (`-o` for short) option to write HTML report.

* Add `--json` option to write JSON summary.

* Add `--iters` option to run benchmarks without analysis.

* Remove `--time-mode` option, always keep track of wall time and cpu
  time in JSON data.

* Export ``defaultMainWith``, ``defaultConfig``, ``Config`` and the
  data types used in the fields of ``Config``.

* Silence the outputs in tests.

## 0.1.1.1 -- 2024-05-29

* Suppress warning messages in ghc 9.10.

## 0.1.1.0 -- 2023-09-20

* Update version bounds of ``deepseq``.

* Reorder exported entities.

* Add "dev" flag for internal development of miniterion.

* Some documentation updates.

## 0.1.0.0 -- 2023-09-15

* Initial release.
