{-# LANGUAGE ExistentialQuantification #-}
{- |
Process chunks of data in the IO monad.
Typical inputs are strict storable vectors and piecewise constant values,
and typical outputs are strict storable vectors.
You may also combine several of these types using the Zip type constructor.

We may substitute IO by ST in the future, but I am uncertain about that.
On the one hand, the admissible IO functionality is very restricted,
only memory manipulation is allowed,
on the other hand we use ForeignPtrs that are not part of ST framework.
-}
module Synthesizer.CausalIO.Process (
   T(Cons),
   fromCausal,
   mapAccum,
   Synthesizer.CausalIO.Process.traverse,
   runCont,
   runStorableChunkyCont,
   zip,
   continue,
   continueChunk,
   ) where

import qualified Synthesizer.Causal.Process as Causal

import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Zip as Zip

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV

import Foreign.Storable (Storable, )

import qualified Control.Monad.Trans.State as MS
import qualified Control.Arrow    as Arr
import qualified Control.Category as Cat
import Control.Arrow ((^<<), (&&&), )
import Control.Monad (mplus, )

import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )

import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO, )

import Prelude hiding (zip, )


{-
Like the Causal arrow but unlike the Causal arrow from @synthesizer-llvm@,
we are not using a parameter type @p@.
In order to parameterize the process,
you simply use a plain Haskell function, i.e. @p -> T a b@.
This way, we do not need the Parameter type from @synthesizer-llvm@.
However, the internal state type can depend
on the value of parameters.
This may be an advantage or a disadvantage, I do not know.
-}
data T a b =
   forall state.
   Cons
      {-
      If the transition function returns a chunk
      that is shorter than the input,
      then this is the last chunk.
      This way we do not need a MaybeT IO.
      -}
      (a -> state -> IO (b, state))
      (IO state)
      {-
      The delete function must not do anything serious,
      e.g. close files,
      because it might not be called.
      Something like 'touchForeignPtr' is reasonable.
      -}
      (state -> IO ())


instance Cat.Category T where
   id = Arr.arr id
   (Cons nextB createB deleteB) .
          (Cons nextA createA deleteA) = Cons
      (\a (sa0,sb0) -> do
         (b,sa1) <- nextA a sa0
         (c,sb1) <- nextB b sb0
         return (c,(sa1,sb1)))
      (do
         sa <- createA
         sb <- createB
         return (sa,sb))
      (\(sa,sb) ->
         deleteA sa >> deleteB sb)

instance Arr.Arrow T where
   arr f = Cons
      (\ a () -> return (f a, ()))
      (return ())
      (\ () -> return ())
   first (Cons next create delete) = Cons
      (\(b,d) sa0 ->
         do (c,sa1) <- next b sa0
            return ((c,d), sa1))
      create
      delete

fromCausal ::
   (Monoid b) =>
   Causal.T a b -> T a b
fromCausal (Causal.Cons next start) = Cons
   (\a s0 ->
      return $
      case MS.runStateT (next a) s0 of
         Nothing -> (mempty, s0)
         Just (b,s1) -> (b,s1))
   (return $ start)
   (\ _ -> return ())

mapAccum ::
   (a -> state -> (b, state)) ->
   state ->
   T a b
mapAccum next start =
   Cons
      (\a s -> return $ next a s)
      (return start)
      (\ _ -> return ())

{-
The parameter order is chosen this way,
because the 'next' function definition might be large
and can be separated with a ($).
-}
traverse ::
   state ->
   (a -> MS.State state b) ->
   T a b
traverse start next =
   Cons
      (\a s -> return $ MS.runState (next a) s)
      (return start)
      (\ _ -> return ())


{- |
This function converts a process
into a function on lazy storable vectors.
To this end it must call unsafePerformIO,
that is, the effects of all functions called in the process
must not be observable.

I am not sure, we need this function at all.
-}
runCont ::
   (CutG.Transform a, CutG.Transform b) =>
   T a b -> IO (([a] -> [b]) -> [a] -> [b])
runCont (Cons next create delete) =
   return $
      \ procRest sig ->
      unsafePerformIO $ do
         let go xt s0 =
               unsafeInterleaveIO $
               case xt of
                  [] -> delete s0 >> return []
                  x:xs -> do
                     (y,s1) <- next x s0
                     (if CutG.length y > 0
                        then fmap (y:)
                        else id) $
                        (if CutG.length y < CutG.length x
                           then return $ procRest $
                                CutG.drop (CutG.length y) x : xs
                           else go xs s1)
         go sig =<< create

{- |
The same restrictions as for 'runCont' apply.
-}
runStorableChunkyCont ::
   (Storable a, Storable b) =>
   T (SV.Vector a) (SV.Vector b) ->
   IO ((SVL.Vector a -> SVL.Vector b) ->
       SVL.Vector a -> SVL.Vector b)
runStorableChunkyCont proc =
   flip fmap (runCont proc) $ \f cont ->
      SVL.fromChunks .
      f (SVL.chunks . cont . SVL.fromChunks) .
      SVL.chunks


zip ::
   (Arr.Arrow arrow) =>
   arrow a b -> arrow a c -> arrow a (Zip.T b c)
zip ab ac =
   uncurry Zip.Cons ^<< ab &&& ac


instance (CutG.Transform a, CutG.Read b, Semigroup b) => Semigroup (T a b) where
   (<>) = append (<>)

{- |
@mappend@ should be used sparingly.
In a loop it will have to construct types at runtime
which is rather expensive.
-}
instance (CutG.Transform a, CutG.Read b, Monoid b) => Monoid (T a b) where
   mempty = Cons
      (\ _a () -> return (mempty, ()))
      (return ())
      (\() -> return ())
   mappend = append mappend

append ::
   (CutG.Transform a, CutG.Read b) =>
   (b -> b -> b) -> T a b -> T a b -> T a b
append app
      (Cons nextX createX deleteX)
      (Cons nextY createY deleteY) = Cons
   (\a s ->
      case s of
         Left s0 -> do
            (b1,s1) <- nextX a s0
            let lenA = CutG.length a
                lenB = CutG.length b1
            case compare lenA lenB of
               LT -> error "CausalIO.Process.mappend: output chunk is larger than input chunk"
               EQ -> return (b1, Left s1)
               GT -> do
                  deleteX s1
                  (b2,s2) <- nextY (CutG.drop lenB a) =<< createY
                  return (app b1 b2, Right s2)
         Right s0 -> do
            (b1,s1) <- nextY a s0
            return (b1, Right s1))
   (fmap Left createX)
   (either deleteX deleteY)


data State a b =
   forall state.
   State
      (a -> state -> IO (b, state))
      (state -> IO ())
      state -- the only difference to (T a b) is the IO


forceMaybe :: (Maybe a -> b) -> Maybe a -> b
forceMaybe f ma =
   case ma of
      Nothing -> f Nothing
      Just a -> f $ Just a

{- |
If the first process does not produce any output,
then the continuing process will not be started.
-}
continue ::
   (CutG.Transform a, SigG.Transform sig b) =>
   T a (sig b) -> (b -> T a (sig b)) -> T a (sig b)
continue (Cons nextX createX deleteX) procY = Cons
   (\a s ->
      case s of
         Left (lastB0, s0) -> do
            (b1,s1) <- nextX a s0
            let lenA = CutG.length a
                lenB = CutG.length b1
                lastB1 =
                   mplus (fmap snd $ SigG.viewR b1) lastB0
                cont lastB = (b1, Left (lastB,s1))
            case compare lenA lenB of
               LT -> error "CausalIO.Process.continue: output chunk is larger than input chunk"
               EQ -> return $ forceMaybe cont lastB1
               GT ->
                  case lastB1 of
                     Nothing -> return (mempty, Left (lastB1,s1))
                     Just lastB ->
                        case procY lastB of
                           Cons nextY createY deleteY -> do
                              deleteX s1
                              (b2,s2) <- nextY (CutG.drop lenB a) =<< createY
                              return (mappend b1 b2, Right (State nextY deleteY s2))
         Right (State nextY deleteY s0) -> do
            (b1,s1) <- nextY a s0
            return (b1, Right (State nextY deleteY s1)))
   (do
      sa <- createX
      return (Left (Nothing, sa)))
   (\s ->
      case s of
         Left (_lastB,s0) -> deleteX s0
         Right (State _ deleteY s0) -> deleteY s0)

{- |
Pass the last non-empty output chunk
as parameter to the continuing process.
This breaks the abstraction from the chunk sizes,
but we need it for implementing vectorized processing.
-}
continueChunk ::
   (CutG.Transform a, CutG.Transform b) =>
   T a b -> (b -> T a b) -> T a b
continueChunk (Cons nextX createX deleteX) procY = Cons
   (\a s ->
      case s of
         Left (lastB0, s0) -> do
            (b1,s1) <- nextX a s0
            let lenA = CutG.length a
                lenB = CutG.length b1
                cont lastB = (b1, Left (lastB,s1))
            case compare lenA lenB of
               LT -> error "CausalIO.Process.continueChunk: output chunk is larger than input chunk"
               EQ ->
                  -- force the decision on lenB, otherwise thunks will accumulate
                  return $ if lenB==0 then cont lastB0 else cont b1
               GT ->
                  if lenB==0
                    then return $ cont lastB0
                    else
                       case procY b1 of
                          Cons nextY createY deleteY -> do
                             deleteX s1
                             (b2,s2) <- nextY (CutG.drop lenB a) =<< createY
                             return (mappend b1 b2, Right (State nextY deleteY s2))
         Right (State nextY deleteY s0) -> do
            (b1,s1) <- nextY a s0
            return (b1, Right (State nextY deleteY s1)))
   (do
      sa <- createX
      return (Left (mempty, sa)))
   (\s ->
      case s of
         Left (_lastB,s0) -> deleteX s0
         Right (State _ deleteY s0) -> deleteY s0)
