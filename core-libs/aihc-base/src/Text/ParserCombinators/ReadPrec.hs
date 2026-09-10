{- HLINT ignore "Use camelCase" -}

-- | Precedence parsers. 'Prelude' builds its 'Read' class on this module.
--
-- The parser itself lives in "GHC.Prim.Read", so that a derived 'Read'
-- instance needs the primitive package only. The conversions to and from
-- "Text.ParserCombinators.ReadP" live here, because that module belongs to
-- the base library.
module Text.ParserCombinators.ReadPrec
  ( ReadPrec,
    Prec,
    minPrec,
    lift,
    prec,
    step,
    reset,
    get,
    look,
    (+++),
    (<++),
    pfail,
    choice,
    readPrec_to_P,
    readP_to_Prec,
    readPrec_to_S,
    readS_to_Prec,
  )
where

import GHC.Prim.Read
  ( Prec,
    ReadPrec,
    choice,
    get,
    look,
    minPrec,
    pfail,
    prec,
    readPrec_to_S,
    readS_to_Prec,
    reset,
    step,
    (+++),
    (<++),
  )
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, readS_to_P)
import Prelude (const, (.))

-- | Run a 'ReadP' parser, ignoring the precedence.
lift :: ReadP a -> ReadPrec a
lift parser = readS_to_Prec (const (readP_to_S parser))

-- | Run a 'ReadPrec' parser at a given precedence.
readPrec_to_P :: ReadPrec a -> Prec -> ReadP a
readPrec_to_P parser = readS_to_P . readPrec_to_S parser

-- | Build a 'ReadPrec' parser from one that is given the precedence.
readP_to_Prec :: (Prec -> ReadP a) -> ReadPrec a
readP_to_Prec make = readS_to_Prec (readP_to_S . make)
