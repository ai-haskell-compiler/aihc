# io-embed
This library allows you to embed the result of an `IO` computation at compile time - as long as said computation is internally typed as: `Char`, `String`, `Integer`, `Rational`, or `ByteString`.

## Installation
This package is available through both [Cabal](https://hackage.haskell.org/package/io-embed-0.1.0.0) and Stackage.

## Usage
Here's how you could embed the contents of a file using `IOEmbed`:

```haskell
{-# LANGUAGE TemplateHaskell #-}

fileContent = $(embedIO $ readFile "./README.md")
```

Alternativelly, you could use `IOEmbedLit` by hand-crafting a [`Lit`](https://hackage.haskell.org/package/template-haskell-2.22.0.0/docs/Language-Haskell-TH-Syntax.html#t:Lit). This is useful if you want to embed something other than the types listed above.

```haskell
{-# LANGUAGE TemplateHaskell #-}

fileContentL = $(embedIOLit $ StringL <$> readFile "./README.md")
```