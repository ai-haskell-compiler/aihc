module Sound.MIDI.Parser.Stream
   (T(..), run, runIncomplete, runPartial,
    ByteList(..),
    PossiblyIncomplete, UserMessage, ) where


import Control.Monad.Trans.State
   (State, runState, evalState, get, put, )
import Control.Monad.Trans.Class (lift, )
import Control.Monad (liftM, when, ap, )
import Control.Applicative (Applicative, pure, (<*>), )

import qualified Sound.MIDI.Parser.Report as Report
import qualified Sound.MIDI.Parser.Class as Parser
import Sound.MIDI.Parser.Class (UserMessage, PossiblyIncomplete, )
import qualified Sound.MIDI.Parser.Exception as Exception
import qualified Sound.MIDI.Parser.Warning   as Warning

import qualified Sound.MIDI.IO as MIO

import Data.Word (Word8)
import qualified Data.List as List

import qualified Numeric.NonNegative.Wrapper as NonNeg

import Prelude hiding (replicate, until, drop, )



{-
Instead of using Report and write the monad instance manually,
we could also use WriterT monad for warnings and ErrorT monad for failure handling.
-}
newtype T str a =
   Cons {decons :: Warning.T (State str) a}



runPartial :: Parser.Fragile (T str) a -> str -> (Report.T a, str)
runPartial parser input =
   flip runState input $ Warning.run $ decons $ Exception.run parser

run :: ByteStream str => Parser.Fragile (T str) a -> str -> Report.T a
run parser input =
   flip evalState input $ Warning.run $ decons $ Exception.run $
      (do a <- parser
          lift $
             Parser.isEnd >>= \end ->
                Parser.warnIf (not end) "unparsed data left over"
          return a)

{- |
Treat errors which caused an incomplete data structure as warnings.
This is reasonable, because we do not reveal the remaining unparsed data
and thus further parsing is not possible.
-}
runIncomplete :: ByteStream str =>
   Parser.Partial (Parser.Fragile (T str)) a -> str -> Report.T a
runIncomplete parser input =
   flip run input $
      lift . Parser.warnIncomplete =<< parser



fromState :: State str a -> T str a
fromState p =
   Cons $ lift p


instance Functor (T str) where
   fmap = liftM

instance Applicative (T str) where
   pure = Cons . return
   (<*>) = ap

instance Monad (T str) where
   return = pure
   x >>= y = Cons $ decons . y =<< decons x


class ByteStream str where
   switchL :: a -> (Word8 -> str -> a) -> str -> a
   drop :: NonNeg.Integer -> str -> str

newtype ByteList = ByteList MIO.ByteList
   deriving Show

instance ByteStream ByteList where
   switchL n j (ByteList xss) =
      case xss of
         (x:xs) -> j x (ByteList xs)
         _ -> n
   drop n (ByteList xs) = ByteList $ List.genericDrop n xs

instance ByteStream str => Parser.EndCheck (T str) where
   isEnd = fromState $ liftM (switchL True (\ _ _ -> False)) get

instance ByteStream str => Parser.C (T str) where
   getByte =
      switchL
         (Parser.giveUp "unexpected end of data")
         (\s ss -> lift (fromState (put ss)) >> return s) =<<
      lift (fromState get)

{-
   skip n = sequence_ (genericReplicate n Parser.getByte)
-}
   skip n = when (n>0) $
      do s <- lift $ fromState get
         switchL
            (Parser.giveUp "skip past end of part")
            (\ _ rest -> lift $ fromState $ put rest)
            (drop (n-1) s)

   warn = Cons . Warning.warn


{-
laziness problems:
fst $ runPartial (Parser.try (undefined :: T ByteList String)) $ ByteList []
fst $ runPartial (Monad.liftM2 (,) (return 'a') (Parser.try (return "bla" :: T ByteList String))) $ ByteList []
fst $ runPartial (Monad.liftM2 (,) (return 'a') (Parser.handleMsg id undefined)) $ ByteList []
evalState (sequence $ repeat $ return 'a') ""
fst $ runPartial (sequence $ repeat $ return 'a') ""

fmap snd $ Report.result $ fst $ runPartial (Parser.appendIncomplete (return (undefined,'a')) (return (undefined,"bc"))) (ByteList $ repeat 129)
fmap snd $ Report.result $ fst $ runPartial ((return (undefined,'a'))) (ByteList $ repeat 129)
fmap snd $ Report.result $ fst $ runPartial (Parser.zeroOrMoreInc (return (Nothing,'a'))) (ByteList $ repeat 129)
fmap snd $ Report.result $ fst $ runPartial (Parser.zeroOrMoreInc (return (undefined,'a'))) (ByteList $ repeat 129)
fmap snd $ Report.result $ fst $ runPartial (Parser.zeroOrMore Parser.getByte) (ByteList $ repeat 129)
either error snd $ Report.result $ fst $ runPartial (Parser.zeroOrMore Parser.getByte) (ByteList $ repeat 129)
Report.result $ run (Parser.zeroOrMore Parser.getByte) (ByteList $ repeat 129)
Report.result $ runIncomplete (Parser.zeroOrMore Parser.getByte) (ByteList $ repeat 129)
Report.result $ runIncomplete (Parser.replicate 1000000 (liftM ((,) Nothing) Parser.getByte)) (ByteList $ repeat 129)
Report.result $ runIncomplete (Parser.until (128==) Parser.getByte) (ByteList $ repeat 129)
-}
