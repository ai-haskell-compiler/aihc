Between
=======

[![Hackage](http://img.shields.io/hackage/v/between.svg)][Hackage: between]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/between.svg)](http://packdeps.haskellers.com/reverse/between)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/between.svg)](https://travis-ci.org/trskop/between)


Description
-----------

It turns out that this combinator

````Haskell
f ~@~ g = (f .) . (. g)
````

is a powerful thing. It was abstracted from following (commonly used)
pattern `f . h . g` where `f` and `g` are fixed.

This library not only define `~@~` combinator, but also some derived
combinators that can help us to easily define a lot of things including
lenses. See [lens package][Hackage: lens] for detais on what lenses are.

Function `Data.Function.on` can be implemented using `~@~` as:

````Haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g = (id ~@~ g ~@~ g) f
````

If function @on3@ existed in /base/ then it could be defined as:

````Haskell
on3 :: (b -> b -> b -> d) -> (a -> b) -> a -> a -> a -> d
on3 f g = (id ~@~ g ~@~ g ~@~ g) f
````

For more examples see documentation.


Documentation
-------------

Stable releases with API documentation are available on
[Hackage][Hackage: between].


Building Options
----------------

* `-fpedantic` (disabled by default)

  Pass additional warning flags to GHC.


License
-------

The BSD 3-Clause License, see [LICENSE][] file for details.


Contributions
-------------

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact author using GitHub or by e-mail (see `.cabal` file for
that).



[Hackage: between]:
    https://hackage.haskell.org/package/between
[Hackage: lens]:
    http://hackage.haskell.org/package/lens
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
[LICENSE]:
  https://github.com/trskop/between/blob/master/LICENSE
  "License of between package."
