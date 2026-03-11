module Sound.MIDI.Parser.Class
   (EndCheck, isEnd,
    C, getByte, skip,
    warn, warnIf, warnIncomplete, Exc.giveUp, Exc.try,
    until, zeroOrMore, zeroOrMoreInc, replicate,
    emptyList, PossiblyIncomplete, UserMessage,
    Fragile, Partial,
    {- for debugging
    absorbException, appendIncomplete,
    -}
    ) where


import Sound.MIDI.Parser.Report (UserMessage)
import qualified Sound.MIDI.Parser.Exception as Exc
import qualified Control.Monad.Exception.Asynchronous as Async
import qualified Control.Monad.Exception.Synchronous  as Sync

import Control.Monad.Trans.Class (lift, )
import Control.Monad.Trans.State (StateT, )
import Control.Monad (liftM, liftM2, when, )

import Data.Word (Word8)

import qualified Numeric.NonNegative.Wrapper as NonNeg

import Prelude hiding (replicate, until, )



class Monad parser => EndCheck parser where
   isEnd   :: parser Bool

-- would be probably better placed in Parser.Status
instance EndCheck parser => EndCheck (StateT st parser) where
   isEnd = lift $ isEnd


class EndCheck parser => C parser where
   getByte :: Fragile parser Word8
   skip    :: NonNeg.Integer -> Fragile parser ()
   warn    :: UserMessage -> parser ()


{- |
@PossiblyIncomplete@ represents a value like a list
that can be the result of an incomplete parse.
The case of an incomplete parse is indicated by @Just message@.

It is not possible to merge this functionality in the parser monad,
because then it is not possible to define monadic binding.
-}
type PossiblyIncomplete a = Async.Exceptional UserMessage a


type Fragile parser   = Sync.ExceptionalT UserMessage parser
type Partial  parser a = parser (PossiblyIncomplete a)


warnIf :: C parser => Bool -> UserMessage -> parser ()
warnIf b msg = when b (warn msg)

{- |
Emit a warning if a value is said to be incomplete.
Be careful using this function,
because an incomplete value often means
that subsequent parse actions will process data from the wrong position.
Only use this function if you
either know that the parse is complete also if the parsed value is incomplete
or if there are no subsequent parse actions to run.

This function cannot fail.
-}
warnIncomplete :: C parser => PossiblyIncomplete a -> parser a
warnIncomplete ~(Async.Exceptional me a) =
   do maybe (return ()) warn me
      return a


{- |
This function will never fail.
If the element parser fails somewhere,
a prefix of the complete list is returned
along with the error message.
-}
zeroOrMore :: EndCheck parser =>
   Fragile parser a -> Partial parser [a]
zeroOrMore p =
   let go =
         isEnd >>= \b ->
            if b
              then return emptyList
              else absorbException
                      (liftM2 (\ x -> fmap (x:)) p (lift go))
   in  go


zeroOrMoreInc :: EndCheck parser =>
   Partial (Fragile parser) a -> Partial parser [a]
zeroOrMoreInc p =
   let go =
         isEnd >>= \b ->
            if b
              then return emptyList
              else absorbException
                      (appendIncomplete p go)
   in  go


{- |
Parse until an element is found, which matches a condition.
The terminating element is consumed by the parser
but not appended to the result list.
If the end of the input is reached without finding the terminating element,
then an Incomplete exception (Just errorMessage) is signaled.
-}
until :: EndCheck parser =>
   (a -> Bool) -> Fragile parser a -> Partial parser [a]
until c p =
   let go =
         isEnd >>= \b ->
            if b
              then
                return $ Async.broken
                   "Parser.until: unexpected end of input" []
              else
                absorbException $
                   p >>= \x ->
                     if c x
                       then return emptyList
                       else liftM (fmap (x:)) (lift go)
   in  go


{- |
This function will never fail.
It may however return a list that is shorter than requested.
-}
replicate ::
   C parser =>
   NonNeg.Int ->
   Partial (Fragile parser) a ->
   Partial parser [a]
replicate m p =
   let go n =
         if n==0
           then return emptyList
           else absorbException
                   (appendIncomplete p (go (n-1)))
   in  go m


emptyList :: PossiblyIncomplete [a]
emptyList = Async.pure []

{- |
The first parser may fail, but the second one must not.
-}
appendIncomplete ::
   Monad parser =>
   Partial (Fragile parser) a ->
   Partial parser [a] ->
   Partial (Fragile parser) [a]
appendIncomplete p ps =
   do ~(Async.Exceptional me x) <- p
      lift $ liftM (fmap (x:)) $
         maybe ps (\_ -> return (Async.Exceptional me [])) me

absorbException ::
   Monad parser =>
   Partial (Fragile parser) [a] ->
   Partial parser [a]
absorbException =
   Sync.resolveT (\errMsg -> return $ Async.broken errMsg [])
