{-
This module Sound.MIDI.Parser.Stream share significant portions of code.
-}
module Sound.MIDI.Parser.ByteString
   (T(..), run, runIncomplete, {- runPartial, -}
    PossiblyIncomplete, UserMessage, ) where


import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Get as Binary
import Data.Binary.Get (Get, runGet, )

import Control.Monad.Trans.Class (lift, )
import Control.Monad (liftM, ap, )
import Control.Applicative (Applicative, pure, (<*>), )

import qualified Sound.MIDI.Parser.Report as Report

import qualified Sound.MIDI.Parser.Class as Parser
import Sound.MIDI.Parser.Class (UserMessage, PossiblyIncomplete, )
import qualified Sound.MIDI.Parser.Exception as Exception
import qualified Sound.MIDI.Parser.Warning   as Warning

import Data.Int (Int64)
import qualified Numeric.NonNegative.Wrapper as NonNeg

import Prelude hiding (replicate, until, )



newtype T a = Cons {decons :: Warning.T Get a}


{-
runPartial :: T a -> B.ByteString -> (Report.T a, B.ByteString)
runPartial parser input =
   flip runGetState input (decons parser)
-}


run :: Parser.Fragile T a -> B.ByteString -> Report.T a
run parser input =
   flip runGet input $ Warning.run $ decons $ Exception.run $
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
runIncomplete ::
   Parser.Partial (Parser.Fragile T) a -> B.ByteString -> Report.T a
runIncomplete parser input =
   flip run input $
      lift . Parser.warnIncomplete =<< parser


fromGet :: Get a -> T a
fromGet p =
   Cons $ lift p


instance Functor T where
   fmap = liftM

instance Applicative T where
   pure = Cons . return
   (<*>) = ap

instance Monad T where
   return = pure
   x >>= y = Cons $ decons . y =<< decons x


instance Parser.EndCheck T where
   isEnd = fromGet Binary.isEmpty

instance Parser.C T where
--   getByte = fromGet Binary.getWord8
-- a get getMaybeWord8 would be nice in order to avoid double-checking
   getByte =
      do end <- lift $ fromGet Binary.isEmpty
         if end
           then Parser.giveUp "unexpected end of ByteString"
           else lift $ fromGet Binary.getWord8

   skip n =
      let toSize x =
            let y = if x > fromIntegral (maxBound `asTypeOf` y)
                      then error "skip: number too big"
                      else fromIntegral x
            in  y
      in  lift $ fromGet $ skip $ toSize $ NonNeg.toNumber n

   warn = Cons . Warning.warn

{- |
In contrast to Binary.skip this one does not fail badly and it works with Int64.
I hope that it is not too inefficient.
-}
skip :: Int64 -> Get ()
skip n = Binary.getLazyByteString n >> return ()
-- Binary.skip n
