---
hide:
  - navigation
  - toc
---

<span class="aihc-eyebrow">AI-written Haskell Compiler</span>

# AIHC Manual

AIHC is a Haskell compiler that AI models write.
It compiles Haskell source code to native machine code, to LLVM IR, and to WebAssembly.
This manual has two parts.

<div class="aihc-cards" markdown>

<div class="aihc-card" markdown>

### [User guide](users/index.md)

Build and install Haskell programs with the `aihc` command.
Read this part if you use the compiler.

</div>

<div class="aihc-card" markdown>

### [Developer manual](developers/index.md)

Learn how the compiler works inside.
Read this part if you change the compiler or its run-time system.

</div>

</div>

## Project links

| Site | Content |
| --- | --- |
| [github.com/ai-haskell-compiler/aihc](https://github.com/ai-haskell-compiler/aihc) | Source code, issues, and progress counts |
| [blog.aihc.app](https://blog.aihc.app/) | Weekly progress notes |
| [perf.aihc.app](https://perf.aihc.app/) | Compile time, artifact size, and run time compared with GHC |
| [Discord](https://discord.gg/uGWkhMCZrZ) | Chat with the AIHC community |

## Status

AIHC is under construction.
The compiler can build many Hackage packages, but it cannot build all of them.
The `base` library is incomplete.
The [README](https://github.com/ai-haskell-compiler/aihc#readme) gives the current progress counts.

## Language

This manual uses ASD-STE100 Simplified Technical English.
Each sentence is short and gives one instruction or one fact.
