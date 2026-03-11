{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE UnboxedTuples   #-}
module EST (
    EST,
    liftST,
    runEST,
    earlyExitEST,
) where

import GHC.Exts (PromptTag#, State#, control0#, newPromptTag#, oneShot, prompt#, runRW#, unsafeCoerce#)
import GHC.ST   (ST (..))

control0##
    :: PromptTag# a
    -> (((State# s -> (# State# s, b #)) -> State# s -> (# State# s, a #))
                                         -> State# s -> (# State# s, a #))
    -> State# s -> (# State# s, b #)
control0## = unsafeCoerce# control0#

newtype EST e s a = EST_ (forall r. (# PromptTag# (Either e r), State# s #) -> (# State# s, a #))

instance Functor (EST e s) where
    fmap f (EST g) = EST (\st -> case g st of (# s, a #) -> (# s, f a #))

instance Applicative (EST e s) where
    pure x = EST (\(# _, s #) -> (# s, x #))
    EST f <*> EST x = EST
        (\(# t, s0 #) -> case f (# t, s0 #) of {
        (# s1, f' #) -> case x (# t, s1 #) of {
        (# s2, x' #) -> (# s2, f' x' #) }})

instance Monad (EST e s) where
    EST m >>= k = EST
        (\(# t, s0 #) -> case m (# t, s0 #) of {
        (# s1, x #) -> case k x of {
        EST f -> f (# t, s1 #) }})

pattern EST :: (forall r. (# PromptTag# (Either e r), State# s #) -> (# State# s, a #)) -> EST e s a
pattern EST f <- EST_ f
  where EST f = EST_ (oneShot f)
{-# COMPLETE EST #-}

liftST :: ST s a -> EST e s a
liftST (ST f) = EST (\ (# _, s #) -> f s)

earlyExitEST :: e -> EST e s any
earlyExitEST e = EST (\(# tag, s0 #) -> control0## tag (\_k s -> (# s, Left e #)) s0)

runEST :: forall e a. (forall s. EST e s a) -> Either e a
runEST (EST f) = runRW#
    -- create tag
    (\s0 -> case newPromptTag# s0 of {
    -- prompt
    (# s1, tag #) -> case prompt# tag
         -- run the `f` inside prompt,
         -- and once we get to the end return `Right` value
         (\s2 -> case f (# tag, s2 #) of (# s3, a #) -> (# s3, Right a #)) s1 of {
    (# _, a #) -> a }})
