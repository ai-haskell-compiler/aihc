{-# LANGUAGE
      OverloadedStrings
  #-}

-- | We provide two global monads: one merely storing errors, and one
-- also incorporating IO.

module Report where

import Prelude hiding (Applicative(..), Foldable(..))
import Control.Applicative (Applicative(..))
import Control.Monad (join, (<=<))
import Data.Foldable (Foldable(..))
import Data.Text.Lazy (Text, intercalate, unpack)
import System.Exit (die)


newtype Failure = Failure { failureText :: Text }

report :: [Failure] -> Text
report = intercalate "\n\n" . fmap failureText


data E a =
  Failed [Failure] |
  Clean a

failure :: Text -> E a
failure = Failed . pure . Failure

instance Functor E where
  fmap _ (Failed l) = Failed l
  fmap f (Clean x) = Clean $ f x

instance Applicative E where
  pure = Clean
  liftA2 _ (Failed k) (Failed l) = Failed (k <> l)
  liftA2 _ (Failed k) (Clean _) = Failed k
  liftA2 _ (Clean _) (Failed l) = Failed l
  liftA2 f (Clean x) (Clean y) = Clean $ f x y

instance Monad E where
  return = pure
  Clean x >>= f = f x
  Failed l >>= _ = Failed l
  (>>) = (*>)


dist :: E (IO a) -> IO (E a)
dist (Clean x) = Clean <$> x
dist (Failed l) = pure $ Failed l


newtype I a = I { ioe :: IO (E a) }

instance Functor I where
  fmap f (I x) = I (fmap f <$> x)

instance Applicative I where
  pure = I . pure . pure
  I f <*> I x = I (liftA2 (<*>) f x)

instance Monad I where
  return = pure
  I x >>= f = I ((fmap join . dist . fmap (ioe . f)) =<< x)
  (>>) = (*>)


liftE :: E a -> I a
liftE = I . pure

liftIO :: IO a -> I a
liftIO = I . fmap pure


doOrDie :: I a -> IO a
doOrDie = f <=< ioe where
  f (Clean x) = pure x
  f (Failed l) = die . unpack $ report l


configure :: Monad m => [m (a -> a)] -> m (a -> a)
configure l = foldl' (.) id <$> sequenceA l
