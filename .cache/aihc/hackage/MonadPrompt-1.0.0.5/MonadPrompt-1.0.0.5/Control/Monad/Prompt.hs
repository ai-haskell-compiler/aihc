{- | Implementation of monads that allow the computation
to 'Control.Monad.Prompt.prompt' for further input.

(c) 2008 Bertram Felgenhauer & Ryan Ingram
Released as open source under a 3 clause BSD license. See the LICENSE
file in the source code distribution for further information.

RecPromptT added by Cale Gibbard, contributed under the same license.

MonadPrompt monads allow you to pass some object of the prompt
type in, and get a result of the prompt's answer type out.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Prompt (
    MonadPrompt(..),
    Prompt,
      runPromptC,
      runPrompt,
      runPromptM,
    RecPrompt,
      unRecPrompt,
      runRecPromptC,
      runRecPrompt,
      runRecPromptM,
    PromptT,
      runPromptT,
      runPromptTM,
      runPromptTM',
      Lift(..),
      unPromptT,
      liftP,
    RecPromptT,
      unRecPromptT,
      runRecPromptT
      
) where
import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM)
import Control.Monad.Trans (MonadTrans(..))

{- |You can construct a monad very simply with prompt, by putting
all of its effects as terms in a GADT, like the following example:

@
data PromptState s a where
    Put :: s -> PromptState s ()
    Get :: PromptState s s
@

You then use 'prompt' to access effects:

@
postIncrement :: MonadPrompt (PromptState Int) m => m Int
postIncrement =
  do x <- prompt Get
     prompt (Put (x+1))
     return x
@

The advantage of Prompt over implementing effects directly:

1. Prompt is pure; it is only through the observation function
   runPromptC that you can cause effects.

2. You don't have to worry about the monad laws; they are
   correct by construction and you cannot break them.

3. You can implement several observation functions for the same
   type.  See, for example, <http://paste.lisp.org/display/53766>
   where a guessing game is implemented with an IO observation
   function for the user, and an AI observation function that
   plays the game automatically.

In these ways Prompt is similar to Unimo, but bind and return
are inlined into the computation, whereas in Unimo they are
handled as a term calculus.
See <http://sneezy.cs.nott.ac.uk/fplunch/weblog/?p=89>
-}
class Monad m => MonadPrompt p m | m -> p where
    prompt :: p a -> m a
{-
For any prompt p, Prompt p is an instance of MonadPrompt p.
-}
newtype Prompt p r = Prompt {
    runP :: forall b . (r -> b) -> (forall a . p a -> (a -> b) -> b) -> b
}

instance Monad (Prompt p) where
    return a = Prompt $ \done _   -> done a
    f >>= g  = Prompt $ \done prm -> runP f (\x -> runP (g x) done prm) prm

instance Functor (Prompt p) where
    fmap = liftM

instance Applicative (Prompt p) where
    pure  = return
    (<*>) = ap

instance MonadPrompt p (Prompt p) where
    prompt p = Prompt $ \done prm -> prm p done

{- |'runPromptC' is the observation function for prompts.  It takes
two functions as arguments:

1. @ret@ will be called with the final result of the computation,
   to convert it to the answer type.

2. @prm@ will be called if there are any effects; it is passed
   a prompt and a continuation function.  prm can apply
   the effect requested by the prompt and call the continuation.

In some cases prm can return the answer type directly; it
may be useful to abort the remainder of the computation, or
save off the continuation to be called later.  There is a great
example of using this to implement a UI for peg solitaire in Bertram
Felgenhauer's post to Haskell-Cafe at
<http://www.haskell.org/pipermail/haskell-cafe/2008-January/038301.html>
-}

runPromptC :: forall p r b. -- prompt, computation result, answer type
              (r -> b)      -- ^ handler when there is no further computation
           -> (forall a . p a -> (a -> b) -> b)
                            -- ^ handler for prompts
           -> Prompt p r    -- ^ a prompt-based computation
           -> b             -- ^ answer
runPromptC ret prm p = runP p ret prm

{- |'runPrompt' takes a way of converting prompts to an element in a pure
fashion and calculates the result of the prompt -}

runPrompt :: (forall a. p a -> a) -> Prompt p r -> r
runPrompt prm = runPromptC id (\p cont -> cont $ prm p)

{- |'runPromptM' is similar to 'runPrompt' but allows the computation to happen in any monad. -}

runPromptM :: Monad m => (forall a . p a -> m a) -> Prompt p r -> m r
runPromptM prm = runPromptC return (\p cont -> prm p >>= cont)

{- | 'RecPrompt' is for prompts which are dependent on the prompt monad.

For example, a 'MonadPlus' prompt:

@
data PromptPlus m a where
  PromptZero :: PromptPlus m a
  PromptPlus :: m a -> m a -> PromptPlus m a

instance MonadPlus (RecPrompt PromptPlus) where
  mzero = prompt PromptZero
  mplus x y = prompt (PromptPlus x y)
@
-}
newtype RecPrompt p r = RecPrompt { unRecPrompt :: Prompt (p (RecPrompt p)) r }

instance Monad (RecPrompt p) where
    return  = RecPrompt . return
    m >>= f = RecPrompt $ unRecPrompt m >>= (unRecPrompt . f)

instance Functor (RecPrompt p) where
    fmap    = liftM

instance Applicative (RecPrompt p) where
    pure    = return
    (<*>)   = ap

instance MonadPrompt (p (RecPrompt p)) (RecPrompt p) where
    prompt  = RecPrompt . prompt

{- | Runs a recursive prompt computation. This is similar to 'runPromptC', but for recursive prompt types. -}
runRecPromptC :: forall p r b. -- prompt, computation result, answer type
                 (r -> b)      -- ^ handler when there is no further computation
              -> (forall a . p (RecPrompt p) a -> (a -> b) -> b)
                               -- ^ handler for prompts
              -> RecPrompt p r -- ^ a prompt-based computation
              -> b             -- ^ answer
runRecPromptC ret prm = runPromptC ret prm . unRecPrompt

{- | Run a recursive prompt computation in a pure fashion, similar to 'runPrompt'.  -}
runRecPrompt :: (forall a. p (RecPrompt p) a -> a) -> RecPrompt p r -> r
runRecPrompt prm = runPrompt prm . unRecPrompt

{- | Run a recursive prompt computation in an arbitrary monad, similar to 'runPromptM'. -}
runRecPromptM :: Monad m => (forall a . p (RecPrompt p) a -> m a) -> RecPrompt p r -> m r
runRecPromptM prm = runPromptM prm . unRecPrompt

{- | Prompt can also be used to define monad transformers.

You will notice the lack of a @Monad m@ constraint; this is allowed
because Prompt doesn't use the underlying monad at all; instead
the observation function (generally implemented via 'runPromptT')
will have the constraint.

-}

newtype PromptT p m a = PromptT { unPromptT :: Prompt (Lift p m) a }

{- | A higher-kinded Either, used in defining 'PromptT'. -}
data Lift p m a = Effect (p a) | Lift (m a)

instance Monad (PromptT p m) where
   return  = PromptT . return
   m >>= f = PromptT $ unPromptT m >>= (unPromptT . f)

instance Functor (PromptT p m) where
   fmap    = liftM

instance Applicative (PromptT p m) where
   pure    = return
   (<*>)   = ap

instance MonadPrompt p (PromptT p m) where
   prompt  = PromptT . prompt . Effect

instance MonadTrans (PromptT p) where
   lift = PromptT . prompt . Lift

{- | 'runPromptT' runs a prompt monad transformer. -}
runPromptT :: forall p m r b.
              (r -> b)      -- ^ handler when there is no further computation
           -> (forall a . p a -> (a -> b) -> b)
                            -- ^ handler for prompts
           -> (forall a . m a -> (a -> b) -> b)
                            -- ^ handler for lifted computations
           -> PromptT p m r -- ^ a prompt-based computation
           -> b             -- ^ answer
runPromptT ret prm lft = runPromptC ret prm' . unPromptT where
   prm' (Effect e) = prm e
   prm' (Lift a)   = lft a

{- | 'runPromptTM' is a useful variant of runPromptT when interpreting into another monad -}
runPromptTM :: forall p m r n. (Monad n)
            => (forall a. p a -> n a) -- ^ interpretation for prompts
            -> (forall a. m a -> n a) -- ^ interpretation for lifted computations
            -> PromptT p m r -- ^ a prompt-based computation
            -> n r -- ^ resulting interpretation
runPromptTM prm lft = runPromptT return (\p k -> prm p >>= k) (\l k -> lft l >>= k)

{- | 'runPromptTM'' specialises runPromptTM further for the case that you're interpreting to the base monad by supplying the identity function as the interpretation
     for lifted computations -}
runPromptTM' :: forall p m r. (Monad m)
             => (forall a. p a -> m a) -- ^ interpretation for prompts
             -> PromptT p m r -- ^ a prompt-based computation
             -> m r -- ^ resulting interpretation
runPromptTM' prm = runPromptTM prm id 

{- | You can also lift any Prompt computation into a PromptT (or more generally, any appropriate MonadPrompt instance). This is the kind of place where the advantage of being able to use multiple observation functions on Prompt really shows. -}

liftP :: (MonadPrompt p m) => Prompt p r -> m r
liftP = runPromptM prompt

{- | A recursive variant of the prompt monad transformer. -}
newtype RecPromptT p m a =
  RecPromptT { unRecPromptT :: Prompt (Lift (p (RecPromptT p m)) m) a }

instance Monad (RecPromptT p m) where
   return  = RecPromptT . return
   m >>= f = RecPromptT $ unRecPromptT m >>= (unRecPromptT . f)

instance Functor (RecPromptT p m) where
   fmap    = liftM

instance Applicative (RecPromptT p m) where
   pure    = return
   (<*>)   = ap

instance MonadPrompt (p (RecPromptT p m)) (RecPromptT p m) where
   prompt  = RecPromptT . prompt . Effect

instance MonadTrans (RecPromptT p) where
   lift = RecPromptT . prompt . Lift

{- | Run a recursive prompt monad transformer. -}
runRecPromptT :: forall p r b m.
                 (r -> b)      -- ^ handler when there is no further computation
              -> (forall a . p (RecPromptT p m) a -> (a -> b) -> b)
                               -- ^ handler for prompts
              -> (forall a . m a -> (a -> b) -> b)
                               -- ^ handler for lifted computations
              -> RecPromptT p m r -- ^ a prompt-based computation
              -> b             -- ^ answer
runRecPromptT ret prm lft = runPromptC ret prm' . unRecPromptT where
   prm' (Effect e) = prm e
   prm' (Lift a)   = lft a

