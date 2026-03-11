-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Colorful
-- Copyright   :  Daniel Mendler (c) 2017,
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This library provides styled text output using ANSI
-- escape sequences. The colored text is modeled
-- as nested Colored values, which form a Monoid.
-- As a result the colored code has a relatively concise form.
--
-- For rendering, the Colored Monoid is flattended and
-- then printed out. The library keeps track of a stack of the active
-- styles internally, such that correct and minimal escape sequences are generated.
--
-- This library is used by
-- <https://hackage.haskell.org/package/wl-pprint-console wl-pprint-console>,
-- which is a pretty printer with support for annotations.
--
-- Warning: Windows support is currently not implemented, but
-- is planned (by using ansi-terminal or the ffi).
--
-- Example:
--
-- > basicExample :: IO ()
-- > basicExample = do
-- >   term <- getTerm
-- >   printColoredS term $ Style Underline (Style Bold "Basic Example\n")
-- >     <> Style Bold "Bold"
-- >     <> Style Italic (Bg Red "Italic Red")
-- >     <> Style Underline "Underline"
-- >   putChar '\n'
--
-- For many more examples, see the
-- <https://github.com/minad/colorful-monoids/blob/master/example.hs example.hs> file.
-----------------------------------------------------------

module Data.Monoid.Colorful (
  -- * The Monoid
  Colored(..)
  , Style(..)
  , Color(..)

  -- * Terminal type
  , Term(..)
  , hGetTerm
  , getTerm

  -- * Colorful printing to file handle
  , hPrintColored
  , printColored
  , hPrintColoredIO
  , printColoredIO
  , hPrintColoredS
  , printColoredS

  -- * Show with ANSI escape sequences
  , showColored
  , showColoredM
  , showColoredS

  -- * Reexport from Data.Semigroup
  , (<>)
) where

import Data.Monoid.Colorful.Nested
import Data.Semigroup ((<>))
