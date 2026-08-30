# Find code quality issues

This file is an agent prompt.
Keep a few short examples, one per class family.
Do not add a catalog of past findings.
Do not reduce the prompt to unused code only.

---

Scan the compiler for code quality issues.
Look for unused code, odd semantics, wasted work, wrong names, and duplicate code.
Also look for interfaces that are larger than the callers need.

Do not change code in the first pass.
Read the source.
If a high finding says a name is unused, search the repository for callers.
Give a list sorted by importance.

## Examples of the target classes

Unused data:

PR #1570. `prettyWithType` in `Aihc.Fc.Pretty` took a `TypeEnv`.
No helper read that environment.
Many functions still passed it.

Odd semantics:

The same operation has a different meaning in two backends.
The name of a function does not match the work that it does.

Wasted work:

A reverse lookup is built again for every name.
A file is read twice for the same data.

Wrong name:

A type alias whose name does not match the type.
The name hides the real data.

Look for each of these classes.
Do not look only for unused arguments.
Do not look only for the examples above.

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
4. Compare copies of the same operation across modules and backends.
5. If a high finding claims that a name is unused, search the repository for callers.
6. Sort the findings by importance.
7. Give a list.
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
10. The same operation has a different meaning in two paths.
11. A function name does not match the work that the function does.
12. A type name does not match the type.
13. Work is done many times when it can be done once.
14. A copy of a walk or table is shorter than the original and skips cases.
15. A shared helper or type would make the code smaller and easier to use.

Also report code that is odd, unusual, strange, or out of place.
If you cannot name the class, still report the fact.

## What not to report

Do not report comment wording.
Do not report format.
Do not report a long function unless you can show a quality issue.
Do not report a rewrite that changes behavior when you have no defect and no disagreement between paths.
Do not add a hand-written unit test when a fixture can show the same fact.

These are not findings:

- Rename a local variable for taste.
- A function is long, with no other quality issue.
- A public name that this repository does not call, when you cannot show it is unused outside the repository.

A name that hides the type or the work is a finding.
A rename for taste is not a finding.

If this repository does not call a public name, mark it as a public API suggestion.
Do not call that name unused.

## Importance

Use these ranks.

High:

- Unused data that many functions carry.
- The same operation with different meaning in two paths.
- A name that hides the type or the work.
- A large API that no caller uses.
- Duplicate blocks of 100 lines or more.
- Wasted work that is done on every use of a common path.

Medium:

- An unused export.
- A local unused argument.
- Duplicate helpers of 20 to 80 lines.
- A reverse map or table that is built again for each use.
- A copy that skips cases.

Low:

- An alias of one line.
- A thin wrapper.
- A small copy.
- A local name that is slightly wrong.

## Report form

For each finding, give:

- Importance: high, medium, or low
- File path and function or type name
- Public API or private
- The quality issue
- Why the issue matters
- The change that makes the code better
- Approximate line count when the count is clear

Put high findings first.
Mix unused data, odd semantics, wasted work, and wrong names in the high list.
Do not put all unused-argument findings first when a semantics issue is larger.

After the list, name three first cuts.
A first cut can delete unused data, share one copy, fix a name, or do the work once.

## If the user asks you to change code

Change only the items that the user selects.
Stay in the selected component.
Do not mix a public API change with a private delete unless the user asks.

Then follow `AGENTS.md`.
Run `just fmt`.
Run `just check`.
Open a PR.
