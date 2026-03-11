# Tophat

Tophat is a template engine for Haskell. It has two components:

- a preprocessor for Haskell code
  ([source](https://github.com/jcranch/tophat/tree/main/app))
  which embeds templates directly into it, and

- a small, fairly self-contained library
  ([webpage](https://hackage.haskell.org/package/tophat),
   [source](https://github.com/jcranch/tophat/tree/main/src))
  of template combinators, intended for use with it containing all the
  usual imperative control structures (`if`, `for`, and so on).

## An example

Here's a short example. The Haskell source code is as follows (in the
[examples](https://github.com/jcranch/tophat/tree/main/examples)
directory of the repository):

    {-# LANGUAGE OverloadedStrings #-}

    module Main where

    import Data.Text (Text)
    import qualified Data.Text as T
    import qualified Data.Text.IO as T
    import Tophat

    aliceTheCamel :: Template Int Text
    aliceTheCamel = {{alicethecamel.tpt.txt}}

    main :: IO ()
    main = T.putStr $ runTemplate aliceTheCamel 3

Meanwhile, in the file `alicethecamel.tpt.txt`, we put the following:

    $forH (\ n -> reverse [0..n])$
      $forH (replicate 3)$
        Alice the camel has $embedShow id$ humps,
      $endfor$
      $ifH (> 0)$
        So go, Alice, go!
      $endif$
      $ifH (== 0)$
        Because Alice is a horse.
      $endif$
    $endfor$

This outputs a version of a
[classic nursery rhyme](https://allnurseryrhymes.com/alice-the-camel/).
The spacing is idiosyncratic: the template above was formatted to make
it easy to read. If we were writing HTML or another language where
whitespace is insignificant, it would be fine, but the proper
formatting of nursery rhymes is important. Adjusting the spacing makes
it more correct but less pleasant.

    $forH (\ n -> reverse [0..n]) >>> forH (replicate 3)$
    Alice the camel has $embedShow id$ humps,$endfor$
    $ifH (> 0)$So go, Alice, go!$endif$$ifH (== 0)$Because Alice is a horse.$endif$
    $endfor$

Alternatively we can take charge of newlines, and whitespace around
newlines, for ourselves, by adding a postprocessor:

    $procH (T.replace ("\\n" "\n") . T.concat . fmap T.strip . T.lines)$
      $forH (\ n -> reverse [0..n])$
        $forH (replicate 3)$
          Alice the camel has $embedShow id$ humps,\n
        $endfor$
        $ifH (> 0)$
          So go, Alice, go!
        $endif$
        $ifH (== 0)$
          Because Alice is a horse.
        $endif$\n\n
      $endfor$
    $endproc$

This replacement of whitespace is useful enough that we've placed it
in `Tophat.Text`.

## Comparisons with other templating engines

### Advantages

#### Interoperability

The code in Tophat templates is just Haskell, and so can be mixed
freely with the other code in the project.

As a rule of thumb, template logic should be kept fairly simple. But
it is certainly pleasant to be able to do straightforward computations
in templates, and for those calculations to have exactly the same
syntax as if they were done elsewhere.

#### Power and extensibility

Relatedly, the power of Haskell means that the control structures
provided are unusually powerful. For example, the "for" command works
over any instance of the Foldable typeclass.

However, the framework is fairly simple, and it is easy to define new
control structures if desired.

#### Flexibility

Any type which is an instance of `IsString` (from `Data.String`) can be
used: Tophat templates can generate any of the main string types, or
more exotic ones, with equal ease.

### Disadvantages

#### Novelty

Since other template languages are not Haskell, templates written for
other engines will need some converting to work with Tophat.

#### New syntax

Since the templates are used directly from Haskell source code, the
`{{` and `}}` brackets which introduce a template are new syntax. This
may mess up editor syntax highlighting, hlint, and other tools which
depend on analysing the lexical structure of Haskell source code.

### Benchmarking

I don't have any good data.

### Alternatives

I don't know of any package that takes the same approach to
templating, but the following are all entirely legitimate
alternatives:

- [Heist](https://hackage.haskell.org/package/heist)

- [Hakyll templates](https://jaspervdj.be/hakyll/reference/Hakyll-Web-Template.html)

- [Haiji](https://hackage.haskell.org/package/haiji), using Jinja2 syntax

- [Shakespeare](http://hackage.haskell.org/package/shakespeare), as used in Yesod

- [Heterocephalus](https://hackage.haskell.org/package/heterocephalus), which uses Template Haskell

## Forcing recompilation

Since the code that will be compiled depends on templates, it is
necessary to recompile whenever the templates change, and not just
when the source file changes. There are various ways of going about
this.

### GHC options

GHC can be asked to always recompile the file:

    {-# OPTIONS_GHC -fforce-recomp #-}

### Stack

Stack has
[options](https://docs.haskellstack.org/en/stable/faq/#using-custom-preprocessors)
that allow extra source files to be declared

### Template Haskell

Template Haskell also has
[options](http://hackage.haskell.org/packages/archive/template-haskell/2.7.0.0/doc/html/Language-Haskell-TH-Syntax.html#v:addDependentFile)
for declaring dependent files.

## Why the name?

It stands for "thunks of pure Haskell as templates", but let's not
worry about that.
