hslua-core
==========

[![Build status][]][1] [![AppVeyor Status][]][2] [![Hackage][]][3]

Basic building blocks to interface Haskell and Lua in a
Haskell-idiomatic style.

  [Build status]: https://img.shields.io/github/workflow/status/hslua/hslua/CI.svg?logo=github
  [1]: https://github.com/hslua/hslua/actions
  [AppVeyor Status]: https://ci.appveyor.com/api/projects/status/ldutrilgxhpcau94/branch/main?svg=true
  [2]: https://ci.appveyor.com/project/tarleb/hslua-r2y18
  [Hackage]: https://img.shields.io/hackage/v/hslua-core.svg
  [3]: https://hackage.haskell.org/package/hslua-core

Overview
--------

[Lua][] is a small, well-designed, embeddable scripting language.
It has become the de-facto default to make programs extensible and
is widely used everywhere from servers over games and desktop
applications up to security software and embedded devices. This
package provides the basic building blocks for coders to embed Lua
into their programs.

This package is part of [HsLua][], a Haskell framework built
around the embeddable scripting language [Lua][].

  [Lua]: https://lua.org/
  [HsLua]: https://hslua.org/

Interacting with Lua
--------------------

HsLua core provides the `Lua` type to define Lua operations. The
operations are executed by calling `run`. A simple “Hello, World”
program, using the Lua `print` function, is given below:

``` haskell
import HsLua.Core.Lua as Lua

main :: IO ()
main = Lua.run prog
  where
    prog :: Lua ()
    prog = do
      Lua.openlibs  -- load Lua libraries so we can use 'print'
      Lua.getglobal "print"            -- push print function
      Lua.pushstring "Hello, World!"   -- push string argument
      Lua.call
        (NumArgs 1)    -- number of arguments passed to the function
        (NumResults 0) -- number of results expected
                       -- as return values
```

### The Lua stack

Lua’s API is stack-centered: most operations involve pushing
values to the stack or receiving items from the stack. E.g.,
calling a function is performed by pushing the function onto the
stack, followed by the function arguments in the order they should
be passed to the function. The API function `call` then invokes
the function with given numbers of arguments, pops the function
and parameters off the stack, and pushes the results.

    ,----------.
    |  arg 3   |
    +----------+
    |  arg 2   |
    +----------+
    |  arg 1   |
    +----------+                  ,----------.
    | function |    call 3 1      | result 1 |
    +----------+   ===========>   +----------+
    |          |                  |          |
    |  stack   |                  |  stack   |
    |          |                  |          |

This package provides all basic building blocks to interact with
the Lua stack. If you’d like more comfort, please consider using
the `hslua-packaging` and `hslua-classes` packages.

Error handling
--------------

Errors and exceptions must always be caught and converted when
passing language boundaries. The exception type which can be
handled is encoded as the type `e` in the monad `LuaE e`. Only
exceptions of this type may be thrown; throwing different
exceptions across language boundaries will lead to a program
crash.

Exceptions must support certain operations as defined by the
`LuaError` typeclass. The class ensures that errors can be
converted from and to Lua values, and that a new exception can be
created from a String message.
