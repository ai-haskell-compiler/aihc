module Data.Spreadsheet.Parser where

import qualified Data.Spreadsheet.CharSource as CharSource

import qualified Control.Monad.Exception.Asynchronous as Async

import Data.Functor.Identity (Identity, )
import Control.Monad (liftM, liftM2, )

import Data.Maybe (fromMaybe, )


type T source a = source a

type Straight        source a = source Identity a
type Fallible        source a = source Maybe    a
type Partial         source a = source Identity (PossiblyIncomplete a)
type PartialFallible source a = source Maybe    (PossiblyIncomplete a)

type PossiblyIncomplete a = Async.Exceptional UserMessage a

type UserMessage = String


satisfy ::
   (CharSource.C source) =>
   (Char -> Bool) -> Fallible source Char
satisfy p =
   do c <- CharSource.get
      if p c
        then return c
        else CharSource.stop

char :: (CharSource.C source) =>
   Char -> Fallible source ()
char c = satisfy (c==) >> return ()

string :: (CharSource.C source) =>
   String -> Fallible source ()
string s = mapM_ char s

many :: (CharSource.C source) =>
   Fallible source a -> Straight source [a]
many p =
   let go =
         liftM (fromMaybe []) $
         CharSource.try
            (liftM2 (:) p (CharSource.fallible go))
   in  go

appendIncomplete ::
   CharSource.C source =>
   PartialFallible source a ->
   Partial source [a] ->
   PartialFallible source [a]
appendIncomplete p ps =
   do ~(Async.Exceptional me x) <- p
      CharSource.fallible $ liftM (fmap (x:)) $
         maybe ps (\_ -> return (Async.Exceptional me [])) me

absorbException ::
   (CharSource.C source) =>
   PartialFallible source [a] ->
   Partial source [a]
absorbException =
   liftM (fromMaybe (Async.pure [])) .
   CharSource.try

manyIncomplete :: CharSource.C source =>
   PartialFallible source a -> Partial source [a]
manyIncomplete p =
   let go = absorbException (appendIncomplete p go)
   in  go

sepByIncomplete :: CharSource.C source =>
   Fallible source sep -> PartialFallible source a -> Partial source [a]
sepByIncomplete sep p =
   absorbException $
   appendIncomplete p $
   manyIncomplete (sep >> p)

between :: (CharSource.C source) =>
   UserMessage ->
   Fallible source open -> Fallible source close ->
   Partial source a -> PartialFallible source a
between msg open close p =
   open >>
   CharSource.fallible (terminated msg close p)

terminated :: (CharSource.C source) =>
   UserMessage ->
   Fallible source close ->
   Partial source a -> Partial source a
terminated msg close p =
   do enclosed <- p
      term <- CharSource.try close
      return (enclosed `Async.maybeAbort`
              maybe (Just msg) (const Nothing) term)


-- mplus
eitherOr :: (CharSource.C source) =>
   Fallible source a -> Fallible source a -> Fallible source a
eitherOr x y =
   CharSource.fallible (CharSource.try x) >>= maybe y return

deflt :: (CharSource.C source) =>
   Straight source a -> Fallible source a -> Straight source a
deflt x y =
   maybe x return =<< CharSource.try y
