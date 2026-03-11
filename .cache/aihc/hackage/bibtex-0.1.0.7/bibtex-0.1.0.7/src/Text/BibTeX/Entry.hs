module Text.BibTeX.Entry where

import Data.Char (toLower, )
import Data.Tuple.HT (mapFst, )


data T =
   Cons {
      entryType :: String,
      identifier :: String,
      fields :: [(String, String)]
   }
   deriving (Show)

{- |
Convert the name style \"Surname, First name\" into \"First name Surname\".
-}
flipName :: String -> String
flipName name =
   let (surname, firstName) = break (','==) name
   in  dropWhile (flip elem ", ") firstName ++ " " ++ surname

lowerCaseFieldNames :: T -> T
lowerCaseFieldNames entry =
   entry {fields = map (mapFst (map toLower)) $ fields entry}
