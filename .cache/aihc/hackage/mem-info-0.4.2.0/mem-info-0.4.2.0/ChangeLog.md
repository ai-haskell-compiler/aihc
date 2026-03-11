# Revision history for mem-info

`mem-info` uses [PVP Versioning][1].

## 0.4.2.0 -- 2026-02-11

- Add the function `selfId` to System.MemInfo to allow the current process
  to access its own process information

- Adds `printUsages` and `defaultRoot` as exports to System.MemInfo

- Add an example executable `mem_pids` that uses System.MemInfo functions as
  library functions

## 0.4.1.2 -- 2026-01-14

- Relax the upper bound on the containers dependency

## 0.4.1.1 -- 2025-06-04

- Relax the upper bound on the optparse-applicative dependency

## 0.4.1.0 -- 2025-03-03

- Add a --version flag that prints the version and exits

## 0.4.0.0 -- 2025-03-02

- Add option --proc-root

  Used to specify an alternate process root than the default, _/proc_. This
  allows reporting of process status for other machines whose proc information
  has been mounted or synced to a local filesystem

- Breaking changes:

  - [ProcNamer][2] takes an additional argument that specifies the proc filesystem root


## 0.3.1.0 -- 2025-02-18

- Add option -m (--min-reported)

  This filters out processes with low memory by specifying a lower bound for
  output. Correct filter amounts are specified as quantities along with a unit;
  one of KiB, MiB, GiB or TiB

## 0.3.0.1 -- 2025-01-17

- Fix test data generation in QuickCheck test

## 0.3.0.0 -- 2024-03-17

- Extended the dependency bounds to allow all bytestring 0.12.x

- Remove the check for __root__ when no pids are specified

  - previously, an error occurred if no pids were specified without sudo
  - after this, all processes of the current user are shown

- Add an option -y (--output-style) that allows the output to be formatted as
  CSV with all values in KiB

## 0.2.0.0 -- 2024-01-28

- Simplify the output when the -d (--discriminate-by-pid) flag is used

- Add options -b (--order-by) and -r (--reverse) to change the ordering of the
  output

## 0.1.0.1 -- 2024-01-17

- Adjusted dependency bounds to remove any stale dependencies

## 0.1.0.0 -- 2024-01-16

* Initial version.

[1]: https://pvp.haskell.org
[2]: https://hackage.haskell.org/package/mem-info/docs/System-MemInfo.html#t:ProcNamer
