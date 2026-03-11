[![Build Status](https://travis-ci.org/sheyll/function-builder.svg?branch=master)](https://travis-ci.org/sheyll/function-builder)

[![Hackage](https://img.shields.io/hackage/v/function-builder.svg?style=flat)](http://hackage.haskell.org/package/function-builder)

# A library for making Monoid writing functions with variable number of parameters

**Also known as: Holey monoids.**

This library is made to be useful especially for library authors, who want to provide users
with building blocks to create functions that compose a monoidal structure
from their parameters in a type safe way. Think of `printf`.

**This library** allows the author of such a library to easily add the
building blocks, allowing users to build **poly variadic functions**, i.e. with parameters
depending on the order and composition of these building blocks.

Several `FunctionBuilder` values sharing a common monoidal output type can be composed
to a big `FunctionBuilder` value, in order to build an **output function** that
has a flexible number and types of parameters depending, on the individual
`FunctionBuilder`s used. This output function can be obtained by `toFunction`.

`FunctionBuilder`s can also be composed via standard type classes.

This module gives you ready-made `Functor`, `Applicative`, `Semigroup`, `Monoid` and Category` instances;

For example, this library could be used to build a string formatting
library, that allows users to compose arbitrary, _printf-style_ render **functions**
from reusable building blocks, such that they can be re-combined in order to make
get functions, that can be applied to parameters that fill place holders, like e.g.:

     module AStringFormatter where

     str :: String -> FunctionBuilder String next next
     str = immediate

     renderInt :: FunctionBuiler String next (Int -> next)
     renderInt = deferred show

     renderFloat :: FunctionBuiler String next (Float -> next)
     renderFloat = ...

Then the user of YourStringFormatter can write:

     module CpuTempFormatter where

     import AStringFormatter

     renderCpuTemp :: Int -> Float -> String
     renderCpuTemp =
       toFunction (str "CPU " . renderInt . str " Temperature: " . renderFloat)

## Similar Libraries

* [polyToMonoid](http://hackage.haskell.org/package/polyToMonoid)

* [HoleyMonoid](http://hackage.haskell.org/package/HoleyMonoid)

* [formatting](http://hackage.haskell.org/package/formatting)

* [category-printf](http://hackage.haskell.org/package/category-printf-0.1.1.0)
 I learned from the author of that library Cale Gibbard in [this reddit comment](https://www.reddit.com/r/haskellquestions/comments/an2qoh/quantifiedconstraints_what_is_it_i_am_not_getting/efsla4g) that the `Format` type in that library (called `FunctionBuilder` in this library) is an alias for

     "(...)the cokleisli category for the comonad of functions on a monoid, which is already in the comonad library"
