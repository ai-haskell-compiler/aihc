# Examples

Each example shows a small Haskell module and its intermediate programs.
Select a tab to see the program in that language.

The manual build compiles each example with the compiler libraries of the same commit.
Thus, the output on this page always agrees with the compiler of this version of the manual.
If the compiler cannot compile an example, the manual build fails.
--8<-- "settings.md"

The Haskell sources are in the directory [docs/aihc-manual/pipeline-examples](https://github.com/ai-haskell-compiler/aihc/tree/main/docs/aihc-manual/pipeline-examples).
To add an example, add a directory with an `Example.hs` file and a `description.md` file.
An example can use the types of `GHC.Types`, but not the Prelude.

--8<-- "examples.md"
