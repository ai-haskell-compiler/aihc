# `mintty`
[![Hackage](https://img.shields.io/hackage/v/mintty.svg)][Hackage: mintty]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/mintty.svg)](http://packdeps.haskellers.com/reverse/mintty)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Linux build](https://github.com/RyanGlScott/mintty/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/mintty/actions?query=workflow%3AHaskell-CI)
[![Windows build](https://ci.appveyor.com/api/projects/status/kj3knsx19ebh9wly?svg=true)](https://ci.appveyor.com/project/RyanGlScott/mintty)

[Hackage: mintty]:
  http://hackage.haskell.org/package/mintty
  "mintty package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

MinTTY is a Windows-specific terminal emulator for the widely used Cygwin and MSYS projects, which provide Unix-like environments for Windows. MinTTY consoles behave differently from native Windows consoles (such as `cmd.exe` or PowerShell) in many ways, and in some cases, these differences make it necessary to treat MinTTY consoles differently in code.

The `mintty` library provides a simple way to detect if your code in running in a MinTTY console on Windows. It exports `isMinTTY`, which does the right thing 90% of the time (by checking if standard error is attached to MinTTY), and it also exports `isMinTTYHandle` for the other 10% of the time (when you want to check is some arbitrary handle is attached to MinTTY). As you might expect, both of these functions will simply return `False` on any non-Windows operating system.
