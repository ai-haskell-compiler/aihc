{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.Symbol
Description : Symmetry Operation Symbols
Copyright   : (c) Jun Narumi, 2018-2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental
-}
module Data.Matrix.SymmetryOperationsSymbols.Symbol (
    Symbol(..)
  ) where

import Control.Monad


data Symbol
   = Id  --  '1'
   | T   --   t
   | Inv -- '-1'
   | M   --  'm'
   | A   --   a
   | B   --   b
   | C   --   c
   | D   --   d
   | G   --   g
   | N   --   n
   | R2  --  '2'
   | R3  --  '3'
   | R4  --  '4'
   | R6  --  '6'
   | RI3 -- '-3'
   | RI4 -- '-4'
   | RI6 -- '-6'
   deriving (Show,Eq)

instance Read Symbol where
  readsPrec _ = join . sequence readSymbols
    where
      readSymbols = [
        \n -> [ ( Id,st) | ("1",st) <- lex n ],
        \n -> [ (  T,st) | ("t",st) <- lex n ],
        \n -> [ (  M,st) | ("m",st) <- lex n ],
        \n -> [ (  A,st) | ("a",st) <- lex n ],
        \n -> [ (  B,st) | ("b",st) <- lex n ],
        \n -> [ (  C,st) | ("c",st) <- lex n ],
        \n -> [ (  D,st) | ("d",st) <- lex n ],
        \n -> [ (  G,st) | ("g",st) <- lex n ],
        \n -> [ (  N,st) | ("n",st) <- lex n ],
        \n -> [ ( R2,st) | ("2",st) <- lex n ],
        \n -> [ ( R3,st) | ("3",st) <- lex n ],
        \n -> [ ( R4,st) | ("4",st) <- lex n ],
        \n -> [ ( R6,st) | ("6",st) <- lex n ],
        \n -> [ (Inv,st) | ("-",nn) <- lex n, ("1",st) <- lex nn ],
        \n -> [ (RI3,st) | ("-",nn) <- lex n, ("3",st) <- lex nn ],
        \n -> [ (RI4,st) | ("-",nn) <- lex n, ("4",st) <- lex nn ],
        \n -> [ (RI6,st) | ("-",nn) <- lex n, ("6",st) <- lex nn ]
        ]
                                              
fromSymbol :: Symbol -> String
fromSymbol  Id =  "1"
fromSymbol   T =  "t"
fromSymbol Inv = "-1"
fromSymbol   M =  "m"
fromSymbol   A =  "a"
fromSymbol   B =  "b"
fromSymbol   C =  "c"
fromSymbol   D =  "d"
fromSymbol   G =  "g"
fromSymbol   N =  "n"
fromSymbol  R2 =  "2"
fromSymbol  R3 =  "3"
fromSymbol  R4 =  "4"
fromSymbol  R6 =  "6"
fromSymbol RI3 = "-3"
fromSymbol RI4 = "-4"
fromSymbol RI6 = "-6"


