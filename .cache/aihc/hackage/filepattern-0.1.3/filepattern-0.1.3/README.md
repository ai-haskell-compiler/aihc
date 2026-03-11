# FilePattern [![Hackage version](https://img.shields.io/hackage/v/filepattern.svg?label=Hackage)](https://hackage.haskell.org/package/filepattern) [![Stackage version](https://www.stackage.org/package/filepattern/badge/nightly?label=Stackage)](https://www.stackage.org/package/filepattern) [![Build status](https://img.shields.io/github/workflow/status/ndmitchell/filepattern/ci/master.svg)](https://github.com/ndmitchell/filepattern/actions)

A library for matching files using patterns such as `src/**/*.png` for all `.png` files recursively under the `src` directory. There are two special forms:

* `*` matches part of a path component, excluding any separators.
* `**` as a path component matches an arbitrary number of path components.

Some examples:

* `test.c` matches `test.c` and nothing else.
* `*.c` matches all `.c` files in the current directory, so `file.c` matches, but `file.h` and `dir/file.c` don't.
* `**/*.c` matches all `.c` files anywhere on the filesystem, so `file.c`, `dir/file.c`, `dir1/dir2/file.c` and `/path/to/file.c` all match, but `file.h` and `dir/file.h` don't.
* `dir/*/*` matches all files one level below `dir`, so `dir/one/file.c` and `dir/two/file.h` match, but `file.c`, `one/dir/file.c`, `dir/file.h` and `dir/one/two/file.c` don't.

More complete semantics are given in the documentation for the matching function [`?==`](https://hackage.haskell.org/package/filepattern/docs/System-FilePattern.html#v:-63--61--61-).

## Features

* All matching is _O(n)_. Most functions precompute some information given only one argument. There are also functions to provide bulk matching of many patterns against many paths simultaneously, see [`step`](https://hackage.haskell.org/package/filepattern/docs/System-FilePattern.html#v:step) and [`matchMany`](https://hackage.haskell.org/package/filepattern/docs/System-FilePattern.html#v:matchMany).
* You can obtain the parts that matched the `*` and `**` special forms using [`match`](https://hackage.haskell.org/package/filepattern/docs/System-FilePattern.html#v:match), and substitute them into other patterns using [`substitute`](https://hackage.haskell.org/package/filepattern/docs/System-FilePattern.html#v:substitute).
* You can search for files using a minimal number of IO operations, using the [System.FilePattern.Directory module](https://hackage.haskell.org/package/filepattern-0.1.1/docs/System-FilePattern-Directory.html).

## Related work

* Another Haskell file pattern matching library is [Glob](https://hackage.haskell.org/package/Glob), which aims to be closer to the [POSIX `glob()` function](http://man7.org/linux/man-pages/man7/glob.7.html), with forms such as `*`, `?`, `**/` (somewhat different to the `filepattern` equivalent) and `[:alpha:]`. A complete guide is [in the documentation](https://hackage.haskell.org/package/Glob/docs/System-FilePath-Glob.html#v:compile). Compared to `filepattern`, the `Glob` library is closer to a regular expression library - definitely more powerful, potentially harder to use.
* The [`shake` library](https://shakebuild.com/) has contained a `FilePattern` type since the beginning. This library evolved from that code, with significant improvements.
* The semantics are heavily inspired by [VS Code](https://code.visualstudio.com/docs/editor/codebasics#_advanced-search-options), [Git](https://git-scm.com/docs/gitignore) and the [NPM package Glob](https://www.npmjs.com/package/glob).
