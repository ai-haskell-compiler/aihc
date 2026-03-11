-- | Processes both /options/ and /positional arguments/.  Functions
-- here return both any successful results and any errors.  Ordinarily
-- you will not need this module; instead, see "Multiarg" for most
-- uses or "Multiarg.Mode" for commands that have more than one mode.
module Multiarg.Limeline where

import Multiarg.Types
import Multiarg.Maddash
-- GHC 7.10 exports Word from the Prelude
import Prelude hiding (Word)

data PosArg a = PosArg a
  deriving (Eq, Ord, Show)

instance Functor PosArg where
  fmap f (PosArg a) = PosArg (f a)

-- | Processes a command line where /options/ are interspersed with
-- /positional arguments/.  A /stopper/ is not returned; all
-- /words/ after a /stopper/ are treated as
-- /positional arguments/.
interspersed
  :: [(ShortName, ArgSpec a)]
  -> [(LongName, ArgSpec a)]
  -> (String -> a)
  -> [Word]
  -> ([Either [Output a] (PosArg a)], Maybe OptName)
interspersed shorts longs fTok = go
  where
    go toks = (map Left outs ++ outsRest, err)
      where
        (outs, ei) = processWords shorts longs toks
        (outsRest, err) = case ei of
          Left (opt, _) -> ([], Just opt)
          Right [] -> ([], Nothing)
          Right ((Word x):xs)
            | x == "--" ->
                ( map (\(Word t) -> Right . PosArg . fTok $ t) xs
                , Nothing )
            | otherwise -> ( (Right . PosArg . fTok $ x) : rest
                           , mayErrRest )
            where
              (rest, mayErrRest) = go xs

