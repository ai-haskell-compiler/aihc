# Getting started

This page shows how to build the compiler and how to compile a first program.

## Requirements

AIHC does not have a binary release.
Build the compiler from source.

The repository has a Nix flake.
The flake gives a development environment with GHC, Cabal, Clang, and the other tools.
Install [Nix](https://nixos.org/download/) and enable flakes.

## Build the compiler

Clone the repository and enter the development environment:

```bash
git clone https://github.com/ai-haskell-compiler/aihc.git
cd aihc
nix develop
```

Build the `aihc` executable:

```bash
cabal build exe:aihc
```

Do not use `cabal build all`.
Some packages in the repository build only with AIHC.

Find the executable:

```bash
cabal list-bin exe:aihc
```

Put the directory of the executable on your `PATH`, or run the commands below with `cabal run aihc --`.

## Build a program

Write a main module:

```haskell title="Main.hs"
module Main (main) where

main :: IO ()
main = putStrLn "Hello from AIHC"
```

Build the program:

```bash
aihc build Main.hs --output hello
./hello
```

The first build installs the core libraries into the AIHC store.
This step takes several minutes.
The next builds use the stored libraries.

## Build a Cabal package

`aihc build` accepts a local package directory or a Hackage package name:

```bash
aihc build ./my-package
aihc build hlint-3.10
```

AIHC builds each executable of the package.
It solves the dependency plan and writes it to `aihc.lock`.
See [Command line](command-line.md) for the options.

## Select a target

AIHC compiles to these targets:

| Target | Description |
| --- | --- |
| `apple-arm64` | Native code for macOS on ARM64. |
| `linux-amd64` | Native code for Linux on AMD64. |
| `llvm` | LLVM IR, compiled with Clang. |
| `wasm32-wasip3` | WebAssembly with the WASI preview 3 interface. |

The default target is the native target of the host.
Use `--target` to select a different target:

```bash
aihc build Main.hs --target wasm32-wasip3 --output hello.wasm
```
