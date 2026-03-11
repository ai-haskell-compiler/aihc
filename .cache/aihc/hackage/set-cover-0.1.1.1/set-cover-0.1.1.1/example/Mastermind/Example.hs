module Mastermind.Example where

import Mastermind.Guess (
   countEval,
   defaultAssignFlags,
   AssignFlag(UniqueSymbol),
   AssignFlags,
   EvalSumm(EvalSumm),
   )


import qualified Control.Monad.Trans.State as MS

import qualified Data.EnumSet as EnumSet
import Data.Tuple.HT (mapSnd)


data T a =
   Cons {
      flags_ :: AssignFlags,
      width_ :: Int,
      alphabet_ :: [a],
      guesses_ :: [([a], EvalSumm)]
   }

consDup, consUnique :: [a] -> [([a], EvalSumm)] -> T a
consDup set guesses =
   let width = maximum $ map (length . fst) guesses
   in Cons defaultAssignFlags width set guesses

consUnique set guesses =
   case consDup set guesses of
      ex -> ex {flags_ = EnumSet.insert UniqueSymbol $ flags_ ex}

apply ::
   (AssignFlags -> Int -> [a] -> [([a], EvalSumm)] -> b) -> T a -> b
apply f (Cons flags width set guesses) = f flags width set guesses


cover :: T Char
cover =
   consDup ['a'..'z'] $
      ("prxyt", EvalSumm 0 1) :
      ("smbjy", EvalSumm 0 0) :
      ("krpcu", EvalSumm 0 2) :
      ("ccxlz", EvalSumm 1 0) :
      ("rltxk", EvalSumm 0 1) :
      ("epakz", EvalSumm 0 1) :
      ("acivr", EvalSumm 1 2) :
      ("cqqar", EvalSumm 2 0) :
      ("cfver", EvalSumm 4 0) :
      ("wnhgd", EvalSumm 0 0) :
      []

coverContra :: T Char
coverContra =
   cover{
      guesses_ = ("ocver", EvalSumm 3 0) : guesses_ cover
   }

cafe :: T Char
cafe =
   consUnique ['a'..'f'] $
      ("cbad", EvalSumm 1 1) :
      ("fbec", EvalSumm 0 3) :
      ("beaf", EvalSumm 0 3) :
      []

master :: T Char
master =
   consDup ['a'..'z'] $
   map (mapSnd (MS.evalState countEval)) $
   ("aaaayw", "x") :
   ("bbbdcw", "") :
   ("eefeym", "oo") :
   ("iuzamf", "oo") :
   ("gvarfe", "ooo") :
   ("paqfes", "xxo") :
   ("vamsej", "ooxx") :
   ("amgses", "ooox") :
   ("majgep", "xxx") :
   []

haskell :: T Char
haskell =
   consDup ['a'..'z'] $
   map (mapSnd (MS.evalState countEval)) $
      ("dkryqnx", "o") :
      ("dcyuyjj", "") :
      ("bxfbrsf", "o") :
      ("hgqmihi", "x") :
      ("itkwkkm", "o") :
      ("ixzrlkk", "oo") :
      ("kgslggl", "xxoo") :
      ("ggglvxw", "o") :
      ("llskehl", "xxxxoo") :
      []

no00, no01, no02, no03, no04 :: T Char
no00 =
   consDup ['a'..'f'] $
      ("adab", EvalSumm 0 0) :
      []

no01 =
   consDup ['a'..'c'] $
      ("aaab", EvalSumm 1 1) :
      []

no02 =
   consDup ['a'..'f'] $
      ("ffce", EvalSumm 1 2) :
      ("dade", EvalSumm 2 0) :
      ("dcfe", EvalSumm 2 1) :
      []

no03 = consDup ['a'..'c'] [("ab", EvalSumm 1 0)]
no04 = consDup ['a'..'c'] [("ab", EvalSumm 1 1)]
