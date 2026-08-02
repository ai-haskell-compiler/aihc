module Text.ParserCombinators.ReadPrec
  ( ReadPrec,
    Prec,
    minPrec,
    prec,
    step,
    reset,
    get,
    look,
    (+++),
    (<++),
    pfail,
    choice,
    readPrec_to_S,
    readS_to_Prec,
  )
where

import Prelude
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
