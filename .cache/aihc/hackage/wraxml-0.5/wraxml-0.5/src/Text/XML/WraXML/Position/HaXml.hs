{- |
Conversion from HaXml position to our Position type.
We cannot convert back efficiently,
since we would have to increment the line number repeatedly.
Thus such a conversion is missing.
-}
module Text.XML.WraXML.Position.HaXml where

import           Text.XML.HaXml.Posn (Posn, posnColumn, posnLine, posnFilename, )

import qualified Text.XML.Basic.Position as Pos


{-
fromPosition :: Pos.T -> Posn
fromPosition p =
   Pn (Pos.fileName p) (Pos.row p) (Pos.column p) Nothing
-}

toPosition :: Posn -> Pos.T
toPosition p =
   Pos.new (posnFilename p) (pred $ posnLine p) (pred $ posnColumn p)
