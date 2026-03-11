# `keycode`
[![Hackage](https://img.shields.io/hackage/v/keycode.svg)][Hackage: keycode]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/keycode.svg)](http://packdeps.haskellers.com/reverse/keycode)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/RyanGlScott/keycode/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/keycode/actions?query=workflow%3AHaskell-CI)

[Hackage: keycode]:
  http://hackage.haskell.org/package/keycode
  "keycode package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

Keyboard events in web browsers are often represented as keycodes, which (1) are difficult to remember, and (2) sometimes vary from browser to browser. This package allows one to look up a key press's keycode and get a plain English description of the key that was pressed, to reduce confusion.
