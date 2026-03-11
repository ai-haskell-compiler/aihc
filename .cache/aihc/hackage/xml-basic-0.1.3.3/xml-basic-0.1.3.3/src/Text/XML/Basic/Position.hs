{- |
Module      :  Text.XML.Basic.Position

Maintainer  :  tagsoup@henning-thielemann.de
Stability   :  provisional
Portability :  portable

Position in a file.

Cf. to Text.ParserCombinators.Parsec.Pos
-}

module Text.XML.Basic.Position (
    T, FileName, Row, Column,
    new, initialize,
    row, column, fileName,
    updateOnChar, updateOnString,
    toReportText,
   ) where

import qualified Data.Accessor.Basic as Accessor
-- import Data.Accessor.Basic ((^=), )

import Data.List (foldl')


type FileName = String
type Row      = Int
type Column   = Int

{- |
Position in a file consisting of file name, row and column coordinates.
Upper left is (0,0), but show routines can display this with different offsets.
-}
data T =
   Cons {
      fileName_ :: FileName,
      row_      :: !Row,
      column_   :: !Column
   } deriving (Eq,Ord)


new :: FileName -> Row -> Column -> T
new = Cons

initialize :: FileName -> T
initialize fn = new fn 0 0


-- * access functions

fileName :: Accessor.T T FileName
fileName = Accessor.fromSetGet (\fn p -> p{fileName_ = fn}) fileName_

row :: Accessor.T T Row
row = Accessor.fromSetGet (\n p -> p{row_ = n}) row_

column :: Accessor.T T Column
column = Accessor.fromSetGet (\n p -> p{column_ = n}) column_


-- * update position according to read characters

updateOnString :: T -> String -> T
updateOnString pos string =
   foldl' (flip updateOnChar) pos string

updateOnChar   :: Char -> T -> T
updateOnChar char (Cons name r c) =
   let (newRow, newColumn) =
          case char of
            '\n' -> (succ r, 0)
            '\t' -> (r, c + 8 - mod c 8)
            _    -> (r, succ c)
   in  Cons name newRow newColumn
--   in  (row ^= newRow) $ (column ^= newColumn) $ pos


-- * update position according to read characters

{- |
Convert the file position to a format
that development environments can understand.
-}
toReportText :: T -> String
toReportText (Cons name r c) =
   concatMap (++":") [name, show (r+1), show (c+1)]

instance Show T where
  showsPrec p (Cons name r c) =
     showParen (p > 10)
        (showString $ unwords $
            "Position.new" : show name : show r : show c : [])
