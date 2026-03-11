-- | Maddash is a Mealy finite state machine that processes /options/.
-- Ordinarily you will not need this module; instead, see "Multiarg"
-- for most uses or "Multiarg.Mode" for commands that have more than
-- one mode.
--
-- The machine consists of the following parts:
--
-- * The set of states, in 'State'
--
-- * The start state, which is 'Ready'
--
-- * The input alphabet, which is all 'Word's.  A 'Word' is an
-- input /word/ from the command line.
--
-- * The output alphabet, which is 'Pallet'.  A 'Pallet' indicates
-- whether its input is not an option at all with 'NotAnOption'.  This
-- indicates that the input 'Word' was not a short option and was not
-- a long option; that is, it was not a single dash followed by a
-- non-dash character and it was not a double dash followed by another
-- character.  (Neither a single dash alone nor a double dash alone is
-- an option.)  Anything else is an option and will return 'Full',
-- which is a list of 'Output'.  Each 'Output' indicates either an
-- error or a good result.
--
-- * The transition function and the output function are combined into
-- a single function, 'processWord'.

module Multiarg.Maddash
  ( -- * /Options/ and /option arguments/
    OptName(..)
  , optSpec
  , ArgSpec(..)
  , ShortName
  , LongName
  , shortName
  , longName
  , shortNameToChar
  , longNameToString

  -- * Machine components
  , Output(..)
  , Pallet(..)
  , State(..)
  , isReady
  , isPending
  , processWord

  -- * Multi-word processor
  , processWords

  -- * Errors
  , OptArg(..)
  , OptionError(..)
  ) where

import Control.Applicative
import Multiarg.Types
-- GHC 7.10 exports Word from the Prelude
import Prelude hiding (Word)

-- * Machine components

data Output a
  = Good a
  | OptionError OptionError
  deriving (Eq, Ord, Show)

instance Functor Output where
  fmap f (Good a) = Good (f a)
  fmap _ (OptionError e) = OptionError e

data Pallet a
  = NotAnOption
  | Full [Output a]
  deriving (Eq, Ord, Show)

instance Functor Pallet where
  fmap _ NotAnOption = NotAnOption
  fmap f (Full os) = Full (map (fmap f) os)

data State a
  = Ready
  -- ^ Accepting new words

  | Pending OptName (Word -> ([Output a], State a))
  -- ^ In the middle of processing an /option/; this function will be
  -- applied to the next word to get a result


instance Functor State where
  fmap _ Ready = Ready
  fmap f (Pending o g)
    = Pending o (\t -> let (os, st') = g t
                       in (map (fmap f) os, fmap f st'))

instance Show (State a) where
  show Ready = "Ready"
  show (Pending o _) = "Pending - " ++ show o

isReady :: State a -> Bool
isReady Ready = True
isReady _ = False

isPending :: State a -> Bool
isPending (Pending _ _) = True
isPending _ = False

-- | Process a single word in the machine.
processWord
  :: [(ShortName, ArgSpec a)]
  -> [(LongName, ArgSpec a)]
  -> State a
  -> Word
  -> (Pallet a, State a)
processWord shorts longs st inp = case st of
  Pending _ f -> (Full os, st')
    where
      (os, st') = f inp
  Ready -> case procOpt of
    Just (os, st') -> (Full os, st')
    Nothing -> (NotAnOption, Ready)
    where
      procOpt = procShort shorts inp <|> procLong longs inp

-- * Multi-word processor

-- | Processes multiple /words/ in the machine.  Processing ends with
-- the first /word/ that is 'NotAnOption'.  This first /word/ that is
-- 'NotAnOption', and all remaining /words/, are returned in the
-- result.  A list of all lists of 'Output' are also returned, with
-- one list for each input 'Word' that was processed.  Each of these
-- lists may be of any length.  For instance, if the input /word/ is
-- the /flag/ for a /long option/ that takes two /option arguments/,
-- the corresponding list will be empty.  If the input /word/ is a
-- /flag/ for a /short option/, this list may have more than one
-- element.
processWords
  :: [(ShortName, ArgSpec a)]
  -> [(LongName, ArgSpec a)]
  -> [Word]
  -> ([[Output a]], Either (OptName, Word -> ([Output a], State a)) [Word])
processWords shorts longs = go Ready
  where
    go Ready [] = ([], Right [])
    go (Pending opt f) [] = ([], Left (opt, f))
    go st (t:ts) = (rs, eiToksPend)
      where
        (pallet, st') = processWord shorts longs st t
        (rs, eiToksPend) = case pallet of
          NotAnOption -> ([], Right (t:ts))
          Full out -> (out : outRest, ei)
            where
              (outRest, ei) = go st' ts

-- * Errors

data OptionError
  = BadOption OptName
  | LongArgumentForZeroArgumentOption LongName OptArg
  -- ^ The user gave an argument for a long option that does not take
  -- an argument.
  deriving (Eq, Ord, Show)

-- * All exported types and functions above this line

-- * Internal functions - not exported

-- | Examines a word to determine if it is a short option.  If so,
-- processes it; otherwise, returns Nothing.
procShort
  :: [(ShortName, ArgSpec a)]
  -> Word
  -> Maybe ([Output a], State a)
procShort shorts inp = fmap (getShortOpt shorts) (isShort inp)

getShortOpt
  :: [(ShortName, ArgSpec a)]
  -> (ShortName, ShortTail)
  -> ([Output a], State a)
getShortOpt shorts (short, rest) = case lookup short shorts of
  Nothing -> ( [OptionError (BadOption (OptName (Left short))) ], Ready)
  Just arg -> procShortOpt shorts short arg rest

procShortOpt
  :: [(ShortName, ArgSpec a)]
  -> ShortName
  -> ArgSpec a
  -> ShortTail
  -> ([Output a], State a)
procShortOpt opts _ (ZeroArg a) inp = (this : rest, st)
  where
    this = Good a
    (rest, st) = case splitShortTail inp of
      Nothing -> ([], Ready)
      Just (opt,arg) -> getShortOpt opts (opt, arg)

procShortOpt _ shrt (OneArg f) (ShortTail inp) = case inp of
  [] -> ([], Pending opt g)
    where
      g tok = ([res], Ready)
        where
          res = Good . f . optArgToString $ arg
          arg = wordToOptArg tok
  xs -> ([res], Ready)
    where
      res = Good . f . optArgToString $ optArg
      optArg = OptArg xs
  where
    opt = OptName (Left shrt)

procShortOpt _ shrt (TwoArg f) (ShortTail inp) = ([], Pending opt g)
  where
    g tok1 = case inp of
      [] -> ([], Pending opt h)
        where
          h tok2 = ([res], Ready)
            where
              oa2 = wordToOptArg tok2
              res = Good $ f (optArgToString oa1) (optArgToString oa2)

      xs -> ([res], Ready)
        where
          res = Good $ f (optArgToString tokArg) (optArgToString oa1)
          tokArg = OptArg xs
      where
        oa1 = wordToOptArg tok1
    opt = OptName (Left shrt)

procShortOpt _ shrt (ThreeArg f) (ShortTail inp) = ([], Pending opt g)
  where
    opt = OptName (Left shrt)
    g tok1 = ([], Pending opt h)
      where
        oa1 = wordToOptArg tok1
        h tok2 = case inp of
          [] -> ([], Pending opt i)
            where
              i tok3 = ([res], Ready)
                where
                  oa3 = wordToOptArg tok3
                  res = Good $ f (optArgToString oa1) (optArgToString oa2)
                                 (optArgToString oa3)
          tokInp -> ([res], Ready)
            where
              tokArg = wordToOptArg (Word tokInp)
              res = Good $ f (optArgToString tokArg) (optArgToString oa1)
                             (optArgToString oa2)
          where
            oa2 = wordToOptArg tok2

procLong
  :: [(LongName, ArgSpec a)]
  -> Word
  -> Maybe ([Output a], State a)
procLong longs inp = fmap (procLongOpt longs) (isLong inp)

procLongOpt
  :: [(LongName, ArgSpec a)]
  -> (LongName, Maybe OptArg)
  -> ([Output a], State a)
procLongOpt longs (inp, mayArg) = case lookup inp longs of
  Nothing -> ( [OptionError (BadOption . OptName . Right $ inp)], Ready)
  Just (ZeroArg r) -> ([result], Ready)
    where
      result = case mayArg of
        Nothing -> Good r
        Just arg -> OptionError (LongArgumentForZeroArgumentOption inp arg)

  Just (OneArg f) -> case mayArg of
    Nothing -> ([], Pending opt run)
      where
        run tok = ([out], Ready)
          where
            out = Good $ f (optArgToString arg1)
            arg1 = wordToOptArg tok
    Just arg -> ([out], Ready)
      where
        out = Good $ f (optArgToString arg)

  Just (TwoArg f) -> ([], Pending opt g)
    where
      g gTok = case mayArg of
        Just arg1 -> ([out], Ready)
          where
            out = Good $ f (optArgToString arg1) (optArgToString gArg)
        Nothing -> ([], Pending opt h)
          where
            h hTok = ([out], Ready)
              where
                out = Good $ f (optArgToString gArg)
                               (optArgToString hArg)
                hArg = wordToOptArg hTok
        where
          gArg = wordToOptArg gTok

  Just (ThreeArg f) -> ([], Pending opt g)
    where
      g gTok = ([], Pending opt h)
        where
          gArg = wordToOptArg gTok
          h hTok = case mayArg of
            Just arg1 -> ([out], Ready)
              where
                out = Good $ f (optArgToString arg1) (optArgToString gArg)
                               (optArgToString hArg)
            Nothing -> ([], Pending opt i)
              where
                i iTok = ([out], Ready)
                  where
                    iArg = wordToOptArg iTok
                    out = Good $ f (optArgToString gArg)
                                   (optArgToString hArg)
                                   (optArgToString iArg)
            where
              hArg = wordToOptArg hTok
  where
    opt = OptName (Right inp)

-- * end
