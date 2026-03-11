![CI](https://github.com/vaclavsvejcar/vcs-ignore/workflows/CI/badge.svg)
[![Hackage version](http://img.shields.io/hackage/v/vcs-ignore.svg)](https://hackage.haskell.org/package/vcs-ignore)
[![Stackage version](https://www.stackage.org/package/vcs-ignore/badge/lts?label=Stackage)](https://www.stackage.org/package/vcs-ignore)

# vcs-ignore
`vcs-ignore` is small Haskell library used to find, check and process files ignored by selected _VCS_.

## 1. Table of Contents
<!-- TOC -->

- [1. Table of Contents](#1-table-of-contents)
- [2. Use as Library](#2-use-as-library)
    - [2.1. Listing all files/directories ignored by VCS](#21-listing-all-filesdirectories-ignored-by-vcs)
    - [2.2. Walking files/directories ignored by VCS](#22-walking-filesdirectories-ignored-by-vcs)
    - [2.3. Checking if path is ignored by VCS](#23-checking-if-path-is-ignored-by-vcs)
- [3. Use as Executable](#3-use-as-executable)
    - [3.1. Checking if path is ignored by VCS](#31-checking-if-path-is-ignored-by-vcs)

<!-- /TOC -->


## 2. Use as Library
Because this library is really simple to use, following example should be enough to understand how to use it for your project.

### 2.1. Listing all files/directories ignored by VCS
```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore ( Git, Repo(..), listRepo )

example :: IO [FilePath]
example = do
  repo <- scanRepo @Git "path/to/repo"
  listRepo repo
```

### 2.2. Walking files/directories ignored by VCS
```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.Maybe       ( catMaybes )
import System.Directory ( doesFileExist )
import Data.VCS.Ignore  ( Git, Repo(..), walkRepo )

onlyFiles :: IO [FilePath]
onlyFiles = do
  repo <- scanRepo @Git "path/to/repo"
  catMaybes <$> walkRepo repo walkFn
 where
  walkFn path = do
    file <- doesFileExist path
    pure (if file then Just path else Nothing)

```

### 2.3. Checking if path is ignored by VCS
```haskell
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Test where

import Data.VCS.Ignore ( Git, Repo(..) )

checkIgnored :: IO Bool
checkIgnored = do
  repo <- scanRepo @Git "path/to/repo"
  isIgnored repo "/some/path/.DS_Store"
```

## 3. Use as Executable
While `vcs-ignore` is mainly intended to be used as a library, it also comes with small executable called `ignore` that can be used standalone to verify whether given path is ignored or not.

```
$ ignore --help
vcs-ignore, v0.0.2.0 :: https://github.com/vaclavsvejcar/vcs-ignore

Usage: ignore (-p|--path PATH) [--debug] [-v|--version] [--numeric-version]
  library for handling files ignored by VCS systems

Available options:
  -p,--path PATH           path to check
  --debug                  produce more verbose output
  -v,--version             show version info
  --numeric-version        show only version number
  -h,--help                Show this help text
```

### 3.1. Checking if path is ignored by VCS
To verify if path is ignored by _VCS_, just call the `ignore` executable with `-p` parameter inside the _VCS_ repository like this:

```
$ ignore -p .stack-work/some-file
Found repository at: /path/to/repo
Path '.stack-work/some-file' IS NOT ignored

$ echo $?
1
```

As you can see, `ignore` executable prints result in human readable form as well as it sets the exit code to `1` if the file is __not__ ignored.
