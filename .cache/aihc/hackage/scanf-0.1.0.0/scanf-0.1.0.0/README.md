# Haskell scanf [![Build Status](https://travis-ci.org/Lysxia/scanf.svg?branch=master)](https://travis-ci.org/Lysxia/scanf)

A lightweight library for one-off parsing and printing.

```haskell
scanf [fmt|%d lazy %s and %d strict %s|]
      "3 lazy functions and 2 strict fields"
  = Just (3 :+ "functions" :+ 2 :+ "fields" :+ ())
```

Quasiquotes are optional. Format strings can also be defined with pure
Haskell combinators.

```haskell
printf (fmt_ (int . " lazy " % string . " and " % int . " strict " % string)
       (8 :+ "dogs" :+ 9 :+ "cats" :+ ())
  = "8 lazy dogs and 9 strict cats"
```

With quasiquotes, the following conversion strings are supported:

- `%d`: signed integer (`Int`)
- `%l`: signed integer (`Integer`, unbounded)
- `%f`: floating point (`Double`)
- `%s`: string of non-space characters (`String`)
- `%c`: single character (`Char`)
- `%%`: parse/print a literal percent character

N.B.: in `scanf`, spaces in the format string match any number of whitespace
character until the next nonspace character.

---

Though it also offers a `printf` function, *scanf* is thus named as
there are only few other preexisting Haskell implementations,
whereas many Haskell *printf* libraries can already be found on Hackage.

Similar to:

- [xformat](https://hackage.haskell.org/package/xformat) (scanf and printf)
- [fmt](https://hackage.haskell.org/package/fmt) (printf)
- [formatting](https://hackage.haskell.org/package/formatting) (printf)
