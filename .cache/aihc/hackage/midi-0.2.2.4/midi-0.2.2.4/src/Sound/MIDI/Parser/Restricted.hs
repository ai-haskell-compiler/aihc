{- |
Parser which limits the input data to a given number of bytes.
We need this for parsing MIDI tracks and some MetaEvents,
where the length of a part is fixed by a length specification.
-}
module Sound.MIDI.Parser.Restricted
   (T(..), run, runFragile, ) where

import qualified Sound.MIDI.Parser.Class as Parser

import qualified Control.Monad.Exception.Synchronous  as Sync
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.State (StateT(runStateT), gets, get, put, )
import Control.Monad (when, )
import Control.Applicative (Applicative, pure, (<*>), )

import qualified Numeric.NonNegative.Wrapper as NonNeg

import Prelude hiding (replicate, until, )


run :: Parser.C parser =>
   NonNeg.Integer -> T parser a -> parser a
run maxLen p =
   do (x,remaining) <- runStateT (decons p) maxLen
      Parser.warnIf
         (remaining>0)
         ("unparsed bytes left in part (" ++ show remaining ++ " bytes)")
      return x

runFragile :: Parser.C parser =>
   NonNeg.Integer -> Parser.Fragile (T parser) a -> Parser.Fragile parser a
runFragile len = Sync.mapExceptionalT (run len)


lift :: Monad parser => Parser.Fragile parser a -> Parser.Fragile (T parser) a
lift = Sync.mapExceptionalT Trans.lift



newtype T parser a =
   Cons {decons :: StateT NonNeg.Integer parser a}

instance Functor parser => Functor (T parser) where
   fmap f = Cons . fmap f . decons

instance (Applicative parser, Monad parser) => Applicative (T parser) where
   pure = Cons . pure
   Cons f <*> Cons a = Cons $ f <*> a

instance Monad parser => Monad (T parser) where
   return = Cons . return
   x >>= y = Cons $ decons . y =<< decons x

instance Trans.MonadTrans T where
   lift = Cons . Trans.lift


getRemaining :: Monad parser => Parser.Fragile (T parser) NonNeg.Integer
getRemaining = Trans.lift $ Cons get

putRemaining :: Monad parser => NonNeg.Integer -> Parser.Fragile (T parser) ()
putRemaining = Trans.lift . Cons . put


instance Parser.EndCheck parser => Parser.EndCheck (T parser) where
   isEnd =
     {- if remaining>0 then we do not check
        whether there are actually more bytes in the stream
        because that will be catched anyway on the next getByte or skip -}
     Cons (gets (0==))
--       if remaining==0 then return True else lift Parser.isEnd

instance Parser.C parser => Parser.C (T parser) where
   getByte =
     getRemaining >>= \remaining ->
       do when (remaining==0)
             (lift $ Parser.giveUp "unexpected end of part")
{- in principle not necessary, because Parser.getByte must check for remaining bytes
          end <- lift Parser.isEnd
          when end
             (lift $ Parser.giveUp "part longer than container")
-}
          putRemaining (remaining-1)
          lift Parser.getByte

   skip n =
     getRemaining >>= \remaining ->
       if n>remaining
         then lift $ Parser.giveUp "skip beyond end of part"
         else putRemaining (remaining-n) >>
              lift (Parser.skip n)

   warn = Cons . Trans.lift . Parser.warn
