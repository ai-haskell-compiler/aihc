# Find code that is unused or larger than the callers need

This file is an agent prompt.
Keep one real example of the target class.
Keep abstract classes after that example.
Do not add a catalog of past findings.

---

Scan the compiler for unused arguments, unused APIs, and duplicate code.
Also look for interfaces that are larger than the callers need.

Do not change code in the first pass.
Read the source.
If a high finding says a name is unused, search the repository for callers.
Give a list sorted by importance.

## One example sets the bar

PR #1570 found this class of unused data.

`prettyWithType` in `Aihc.Fc.Pretty` took a `TypeEnv`.
No helper read that environment.
Many functions still passed it.
The change made the printer much smaller.

Look for that class of unused data.
Do not look only for `TypeEnv`.
Do not look only for pretty printers.

## Scope

Scan every Haskell module in these trees:

- `components/aihc-resolve/`
- `components/aihc-tc/`
- `bin/aihc/compiler/`
- `bin/aihc/src/`
- `bin/aihc/dev/`
- `bin/aihc-fmt/`
- `tooling/`

Do not scan `core-libs/`.
Those files are GHC library sources.

`aihc-resolve` and `aihc-tc` are public libraries.
Still report interface changes.
Mark each of those findings as a public API change.

The compiler library and the CLI are private.
You can change those interfaces.

Do not move work across component boundaries.
See `AGENTS.md` for the domain of each component.

## Method

1. Use subagents.
   Give each subagent one component.
2. Split the compiler into FC, GRIN, native backends, type checks, name resolution, CLI, and tools.
3. Read the function body.
   Do not guess from the name.
4. If a high finding claims that a name is unused, search the repository for callers.
5. Sort the findings by importance.
6. Give a list.
   Do not open a PR in the first pass.

## What to find

Report a finding when you can show one of these facts:

1. An argument or environment that many functions carry and no function reads.
2. A record field that writers set and no reader uses.
3. A function, constructor, or module that no caller uses.
4. A public wrapper that only calls a more complete entry point.
5. The same walk, conversion, or table copied in two or more modules.
6. A helper whose branches all do the same work.
7. A wrapper type that adds no extra facts.
8. A text key where a structured key already exists.
9. Work that belongs in a different component.

## What not to report

Do not report name style.
Do not report comment wording.
Do not report format.
Do not report a long function unless you can show unused data or duplicate work.
Do not report a rewrite that changes behavior when you have no defect.
Do not add a hand-written unit test when a fixture can show the same fact.

These are not findings:

- Rename a local variable for taste.
- A function is long, with no unused argument and no duplicate block.
- A public name that this repository does not call, when you cannot show it is unused outside the repository.

If this repository does not call a public name, mark it as a public API suggestion.
Do not call that name unused.

## Importance

Use these ranks.

High:

- Unused data that many functions carry.
- A large API that no caller uses.
- Duplicate blocks of 100 lines or more.
- Two backends that do the same operation with different meaning.

Medium:

- An unused export.
- A local unused argument.
- Duplicate helpers of 20 to 80 lines.

Low:

- An alias of one line.
- A thin wrapper.
- A small copy.

## Report form

For each finding, give:

- Importance: high, medium, or low
- File path and function or type name
- Public API or private
- The unused or duplicate fact
- The change that makes the code smaller
- Approximate line count when the count is clear

Put high findings first.
Put findings that match the `TypeEnv` class before large refactors.

After the list, name three first cuts.
Each first cut must delete unused data or share one copy of duplicate work.

## If the user asks you to change code

Change only the items that the user selects.
Stay in the selected component.
Do not mix a public API change with a private delete unless the user asks.

Then follow `AGENTS.md`.
Run `just fmt`.
Run `just check`.
Open a PR.
