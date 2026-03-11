{-# LANGUAGE UndecidableInstances #-} -- for RevCharsToSymbol'

module Singleraeh.Symbol where

import Singleraeh.List ( SList(..) )
import Singleraeh.Maybe ( SMaybe(..) )
import Singleraeh.Tuple ( STuple2(..) )
import GHC.TypeLits
import Unsafe.Coerce ( unsafeCoerce )

sConsSymbol :: SChar ch -> SSymbol str -> SSymbol (ConsSymbol ch str)
sConsSymbol ch str =
    withSomeSSymbol (fromSChar ch : fromSSymbol str) unsafeCoerce

-- TODO test!!!
sUnconsSymbol
    :: SSymbol str
    -> SMaybe (STuple2 SChar SSymbol) (UnconsSymbol str)
sUnconsSymbol sstr =
    case fromSSymbol sstr of
      []        -> unsafeCoerce SNothing
      ch : str' -> unsafeCoerce $ SJust $ STuple2
        (withSomeSChar ch unsafeCoerce)
        (withSomeSSymbol str' unsafeCoerce)

-- | Re-construct the output from 'UnconsSymbol'.
type family ReconsSymbol msym where
    ReconsSymbol (Just '(ch, sym)) = ConsSymbol ch sym
    ReconsSymbol Nothing           = ""

sReconsSymbol
    :: SMaybe (STuple2 SChar SSymbol) msym
    -> SSymbol (ReconsSymbol msym)
sReconsSymbol = \case
  SJust (STuple2 ch sym) -> sConsSymbol ch sym
  SNothing               -> SSymbol @""

-- TODO we can write this with @Foldl (FlipSym1 ConsSymbolSym) "" chs@, but
-- we need more singletons then. check it out later :)
type RevCharsToSymbol chs = RevCharsToSymbol' "" chs
type family RevCharsToSymbol' sym chs where
    RevCharsToSymbol' sym (ch : chs) = RevCharsToSymbol' (ConsSymbol ch sym) chs
    RevCharsToSymbol' sym '[]        = sym

revCharsToSymbol :: SList SChar chs -> SSymbol (RevCharsToSymbol chs)
revCharsToSymbol = revCharsToSymbol' (SSymbol @"")

revCharsToSymbol'
    :: SSymbol str
    -> SList SChar chs -> SSymbol (RevCharsToSymbol' str chs)
revCharsToSymbol' sacc = \case
  SCons sch sstr -> revCharsToSymbol' (sConsSymbol sch sacc) sstr
  SNil           -> sacc
