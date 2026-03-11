module Text.BibTeX.Format where

import qualified Text.BibTeX.Entry as Entry

import Data.List (intersperse, )
import Data.List.HT (switchR, )


entry :: Entry.T -> String
entry (Entry.Cons entryType bibId items) =
   let formatItem (name, value) =
         "  "++name++" = {"++value++"},\n"
   in  "@" ++ entryType ++ "{" ++ bibId ++ ",\n" ++
       concatMap formatItem items ++
       "}\n"


enumerate :: [String] -> String
enumerate =
   switchR "" $ \xs0 lastWord0 ->
   flip (switchR lastWord0) xs0 $ \xs1 lastWord1 ->
   foldr
      (\word -> (word ++) . (", " ++))
      (lastWord1 ++ " and " ++ lastWord0) xs1

authorList :: [String] -> String
authorList =
   concat . intersperse " and "

commaSepList :: [String] -> String
commaSepList = sepList ','

sepList :: Char -> [String] -> String
sepList sep = concat . intersperse (sep:" ")
