-- | Utilities for printing lists
module Numeric.Probability.PrintList where


import Data.List (intersperse)


----------------------------------------------------------------------
-- PRINT UTILITIES
----------------------------------------------------------------------

newtype Lines a = Lines [a]

instance Show a => Show (Lines a) where
  show (Lines xs) = printList ("","\n","") show xs

asLines :: [a] -> Lines a
asLines = Lines


showNQ :: Show a => a -> String
showNQ = filter ('"'/=) . show

indent :: Int -> Int -> [Char]
indent i l = take (i*l) (repeat ' ')

printList :: ([a],[a],[a]) -> (b -> [a]) -> [b] -> [a]
printList (sep0,sep1,sep2) f xs =
   sep0++concat (intersperse sep1 (map f xs))++sep2


asTuple, asSeq, asList, asSet, asLisp,
  asString, asPlain, asPlain' :: (a -> [Char]) -> [a] -> [Char]

asTuple = printList ("(",",",")")
asSeq   = printList ("",",","")
asList  = printList ("[",",","]")
asSet   = printList ("{",",","}")
asLisp  = printList ("("," ",")")
asPlain  f xs = if null xs then "" else printList (" "," ","") f xs
asPlain' f xs = if null xs then "" else printList (""," ","") f xs
asString = printList ("","","")
-- asLines = printList ["","\n",""]

asCases :: Int -> (a -> [Char]) -> [a] -> [Char]
asCases l =
   let ind = indent 4 l
   in  printList ("\n"++ind++"   ","\n"++ind++" | ","")

asDefs :: [Char] -> (a -> [Char]) -> [a] -> [Char]
asDefs n = printList ("\n"++n,"\n"++n,"\n")

asParagraphs :: (a -> [Char]) -> [a] -> [Char]
asParagraphs = printList ("\n","\n\n","\n")
