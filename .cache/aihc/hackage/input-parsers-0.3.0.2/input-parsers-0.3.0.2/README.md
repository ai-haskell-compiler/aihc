input-parsers
=============

### An extension of the [parsers](http://hackage.haskell.org/package/parsers) library ###

The [parsers](http://hackage.haskell.org/package/parsers) library provides a number of subclasses of the
[`Alternative`](https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative) type class,
as well as lots of combinators useful for writing actual parsers.

What those classes like
[Parsing](http://hackage.haskell.org/package/parsers/docs/Text-Parser-Combinators.html#t:Parsing) and
[CharParsing](https://hackage.haskell.org/package/parsers/docs/Text-Parser-Char.html#t:CharParsing) lack is the
ability to express certain efficient parser primitives like Attoparsec's
[takeWhile](https://hackage.haskell.org/package/attoparsec/docs/Data-Attoparsec-Text.html#v:takeWhile). To rectify for
this failing and enable more efficient parsers to be expressed, the present package `input-parsers` adds type classes
[InputParsing](http://hackage.haskell.org/package/input-parsers/docs/Text-Parser-Input.html#t:InputParsing) and
[InputCharParsing](http://hackage.haskell.org/package/input-parsers/docs/Text-Parser-Input.html#t:InputCharParsing). The
common characteristic of almost all their methods is that their parse result has the same type as the parser input,
and is a prefix of the input.

The present package also exports the class
[DeterministicParsing](http://hackage.haskell.org/package/input-parsers/docs/Text-Parser-Deterministic.html#t:DeterministicParsing),
which provides a number of parser methods that are guaranteed to succeed with a single (typically longest possible)
result. This is most useful for writing the lexical layer of a parser, but it can help avoid ambiguities and
inefficiencies in general.

Finally, the package provides the class
[`Position`](http://hackage.haskell.org/package/input-parsers/docs/Text-Parser-Input-Position.html#t:Position) to
abstract over the position the parser reached in the input stream.
