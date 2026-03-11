miniutter
=========

[![Hackage](https://img.shields.io/hackage/v/miniutter.svg)](https://hackage.haskell.org/package/miniutter)

This library helps in generating simple present tense
English sentences from short, parametrized descriptions.
In a typical use, the structure of a clause is fixed,
but the particular words to be used vary in arbitrary ways.
The main goal of the library is to minimize the API
complexity and the code size of programs that use it.
The library doesn't attempt to ban incorrect English sentences,
but just make the creation of the simple correct ones easy
and predictable.

The library is available from Hackage[1] and it's homepage
and issue tracker is on github[2].

Further information
-------------------

The library emerged when the needs of the LambdaHack[3] game engine
outgrew its rudimentary grammar code written by Andres Loeh.
The library uses minimorph[4] by Eric Kow to tackle English spelling.
More information about natural language processing libraries in Haskell
is gathered at the Haskell wiki[5].

[1]: http://hackage.haskell.org/package/miniutter
[2]: https://github.com/Mikolaj/miniutter
[3]: http://hackage.haskell.org/package/LambdaHack
[4]: http://hackage.haskell.org/package/minimorph
[5]: http://www.haskell.org/haskellwiki/Applications_and_libraries/Linguistics
