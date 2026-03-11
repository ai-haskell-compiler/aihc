{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Here we implement a monad transformer
which adds exception handling and
labelling of actions (using "Control.Monad.Label")
in order to extend exceptions with a kind of call stack.
-}
module Control.Monad.Exception.Label where

import qualified Control.Monad.Exception.Synchronous as Exception
import qualified Control.Monad.Label as Label

import Control.Monad.Exception.Synchronous (ExceptionalT, mapExceptionT, )
import Control.Monad.Label (LabelT, )
import Control.Applicative (Applicative, )

import Control.Monad (liftM, )
import Control.Monad.Fix (MonadFix, )
import Control.Monad.Trans.Class (MonadTrans, lift, )


data LabeledException l e =
   LabeledException {labels :: [l], exception :: e}

newtype LabeledExceptionalT l e m a =
   LabeledExceptionalT
      {runLabeledExceptionalT :: LabelT l (ExceptionalT (LabeledException l e) m) a}
      deriving (Functor, Applicative, Monad, MonadFix)


runLabelT :: (Monad m) =>
   LabeledExceptionalT l e m a ->
   [l] ->
   ExceptionalT (LabeledException l e) m a
runLabelT =
   Label.runLabelT . runLabeledExceptionalT

labelT :: (Monad m) =>
   ExceptionalT (LabeledException l e) m a ->
   LabeledExceptionalT l e m a
labelT =
   LabeledExceptionalT . lift -- Label.LabelT . ReaderT


stripLabelT :: (Monad m) =>
   LabeledExceptionalT l e m a -> ExceptionalT e m a
stripLabelT action =
   mapExceptionT exception (runLabelT action [])

decorateLabelT :: (Monad m) =>
   ExceptionalT e m a -> LabeledExceptionalT l e m a
decorateLabelT =
   labelT . mapExceptionT (LabeledException [])

getLabels :: (Monad m) =>
   LabeledExceptionalT l e m [l]
getLabels = LabeledExceptionalT $ Label.askT


throwT :: (Monad m) =>
   e -> LabeledExceptionalT l e m a
throwT e =
   do l <- getLabels
      labelT $ Exception.throwT (LabeledException l e)


{- |
Currently 'catchT' calls the exception handler with a full call stack.
Since 'catchT' handles exceptions locally
it may however clear the call stack before calling the inner action
and a re-throw should append the inner call stack to the outer one.
For this semantics, a difference list would be more efficient for labels.
-}
catchT :: (Monad m) =>
   LabeledExceptionalT l e0 m a ->
   ([l] -> e0 -> LabeledExceptionalT l e1 m a) ->
   LabeledExceptionalT l e1 m a
catchT action handler =
   do ls <- getLabels
      labelT $ Exception.catchT
         (runLabelT action ls)
         (\(LabeledException l e) -> runLabelT (handler l e) ls)


{- |
If the enclosed monad has custom exception facilities,
they could skip the cleanup code.
Make sure, that this cannot happen by choosing an appropriate monad.
-}
bracketT :: (Monad m) =>
   l ->
   LabeledExceptionalT l e m h ->
   (h -> LabeledExceptionalT l e m ()) ->
   (h -> LabeledExceptionalT l e m a) ->
   LabeledExceptionalT l e m a
bracketT label open close action =
   do ls <- liftM (label:) getLabels
      labelT $
         Exception.bracketT
            (runLabelT open ls)
            (\h -> runLabelT (close h) ls)
            (\h -> runLabelT (action h) ls)


instance MonadTrans (LabeledExceptionalT l e) where
   lift m = labelT $ lift m
