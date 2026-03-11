{-|
  Copyright   :  (C) 2022-2023, Peter Lebbing
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Peter Lebbing <peter@digitalbrains.com>

  =Efficient interpolation for "Prettyprinter"

  This module provides efficient interpolation of
  [@string-interpolate@](https://hackage.haskell.org/package/string-interpolate)
  quasi quoters when used with
  [@prettyprinter@s](https://hackage.haskell.org/package/prettyprinter)
  'Prettyprinter.Doc'uments.

  The normal quasi quoters from @string-interpolate@ do work when used as a
  @Doc@. Newlines are even converted to @Prettyprinter.@'Prettyprinter.line'.
  However, this method is inefficient. The following code functions correctly:

  @
  {\-\# LANGUAGE OverloadedStrings \#-\}
  {\-\# LANGUAGE QuasiQuotes \#-\}

  module Main where

  import Data.String.Interpolate
  import Data.Text (Text)
  import Prettyprinter

  f :: 'Text'
  f = "world"

  g :: Doc ()
  g = ['i'|Hello #{f}!|]

  main :: IO ()
  main = print g
  @

  However, what happens under the hood is that @f@ is converted to 'String', the
  interpolated string is built manipulating @String@s, and then the output is
  converted to 'Data.Text.Text' in
  [@prettyprinter@](https://hackage.haskell.org/package/prettyprinter). The
  following code is much better:

  @
  g = 'pretty' ([i|Hello #{f}!|] :: Text)
  @

  Now, the interpolated string is constructed as @Text@, and this is passed
  cleanly into @Doc@ which also uses @Text@ as its underlying type for
  representation of text. At no point is @f@ converted to @String@, and the
  string construction benefits from the performance of @Text@. And again,
  newlines are converted to 'Prettyprinter.line'.

  This module defines wrapper quasi quoters that automatically perform the
  @pretty@ invocation, and can simply be used as:

  @
  g = ['di'|Hello #{f}!|]
  @
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Prettyprinter.Interpolate
  ( di
  , __di
  , diii
  , __di'E
  , __di'L
  , diii'E
  , diii'L
    -- ** Deprecated
  , d__i'E
  , d__i'L
  ) where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (Pretty(pretty))
#else
import Data.Text.Prettyprint.Doc (Pretty(pretty))
#endif

import Data.String.Interpolate
import Data.Text (Text)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Name)

wrapper :: Name -> QuasiQuoter -> QuasiQuoter
wrapper nm wrapped = QuasiQuoter
  { quoteExp = \s -> [| pretty ($(quoteExp wrapped s) :: Text) |]
  , quotePat = const $ errQQType nm "pattern"
  , quoteType = const $ errQQType nm "type"
  , quoteDec = const $ errQQType nm "declaration"
  }

-- | Wrapper around the 'i' quasi quoter, producing a t'Prettyprinter.Doc'
--
-- Newlines in the text are converted to 'Prettyprinter.line'.
di :: QuasiQuoter
di = wrapper 'di i

-- | Wrapper around the '__i' quasi quoter, producing a t'Prettyprinter.Doc'
--
-- Newlines in the text are converted to 'Prettyprinter.line'.
__di :: QuasiQuoter
__di = wrapper '__di __i

-- | Wrapper around the 'iii' quasi quoter, producing a t'Prettyprinter.Doc'
--
-- Newlines in the text are converted to 'Prettyprinter.line'.
diii :: QuasiQuoter
diii = wrapper 'diii iii

-- | Wrapper around the '__i'E' quasi quoter, producing a t'Prettyprinter.Doc'
--
-- Newlines in the text are converted to 'Prettyprinter.line'.
__di'E :: QuasiQuoter
__di'E = wrapper '__di'E __i'E

-- | Wrapper around the '__i'L' quasi quoter, producing a t'Prettyprinter.Doc'
--
-- Newlines in the text are converted to 'Prettyprinter.line'.
__di'L :: QuasiQuoter
__di'L = wrapper '__di'L __i'L

-- | Wrapper around the 'iii'E' quasi quoter, producing a t'Prettyprinter.Doc'
--
-- Newlines in the text are converted to 'Prettyprinter.line'.
diii'E :: QuasiQuoter
diii'E = wrapper 'diii'E iii'E

-- | Wrapper around the 'iii'L' quasi quoter, producing a t'Prettyprinter.Doc'
--
-- Newlines in the text are converted to 'Prettyprinter.line'.
diii'L :: QuasiQuoter
diii'L = wrapper 'diii'L iii'L

d__i'E :: QuasiQuoter
d__i'E = wrapper 'd__i'E __i'E
{-# DEPRECATED d__i'E "'d__i'E' is a deprecated alias for '__di'E' and will be removed in prettyprinter-interp 0.3" #-}

d__i'L :: QuasiQuoter
d__i'L = wrapper 'd__i'L __i'L
{-# DEPRECATED d__i'L "'d__i'L' is a deprecated alias for '__di'L' and will be removed in prettyprinter-interp 0.3" #-}

errQQ :: Name -> String -> Q a
errQQ nm msg = fail $ show nm <> ": " <> msg

errQQType :: Name -> String -> Q a
errQQType nm ty = errQQ nm $ "This QuasiQuoter cannot be used as a " <> ty
