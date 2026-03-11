-----------------------------------------------------------------------------
-- |
-- Module      :  Text.JSON.Gen
-- Copyright   :  (c) Scrive 2011
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  andrzej@scrive.com
-- Stability   :  development
-- Portability :  portable
--
-- Abusing monadic 'do' notation library for generating JSON object.
-- Hard-bound to 'Text.JSON.JSValue' from json package from hackage.
--
-- Main ideas
--
-- * Overloaded function 'value' to set values in underlying JSON -
-- 'Bool', 'Int', 'String', lists, etc.
--
-- * JSON generation may not be pure with 'valueM'. You can perform
-- some IO while generating JSON. This is usefull skip useless inner
-- binding.
--
-- * Compositionality - use 'object' to easy create JSON objects. The
-- 'objects' function is there to support arrays of objects.
--
-- * Monadic notation - it really looks nicer then composition with
-- '.' or some magic combinator
--
-- > runJSONGen $ do
-- >     value "a" "a"
-- >     value "b" [1,2,3]
-- >     object "c" $ do
-- >         value "x" True
-- >         value "y" False
--
-- Will generate json object:
--
-- > {a : "a", b: [1,2,3], c: {x: true, y : false}}
--

module Text.JSON.Gen (
    module Text.JSON.ToJSValue
    -- * Basic types
  , JSONGen
  , JSONGenT
    -- * Runners
  , runJSONGen
  , runJSONGenT
    -- * Creating JSON's
  , value
  , valueM
  , object
  , objects
  ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Foldable
import Text.JSON
import qualified Data.Sequence as S

import Text.JSON.ToJSValue

-- --------------------------------------------------------------

-- | Basic types
type JSONGen = JSONGenT Identity

-- | A monad that keeps currently constructed JSON.
newtype JSONGenT m a = JSONGenT (StateT (S.Seq (String, JSValue)) m a)
  deriving (Applicative, Functor, Monad, MonadTrans)


instance MonadIO m => MonadIO (JSONGenT m) where
  liftIO = JSONGenT . liftIO

-- --------------------------------------------------------------

-- | Runner. Example:
--
-- > let js = runJSONGen $ do
-- >            value "abc" "def"
runJSONGen :: JSONGen () -> JSValue
runJSONGen = runIdentity . runJSONGenT


-- | Runner as monad transformer. Example:
--
-- > js <- runJSONGenT $ do
-- >            d <- lift $ getFromOuterMonad
-- >            value "abc" d
runJSONGenT :: Monad m => JSONGenT m () -> m JSValue
runJSONGenT (JSONGenT f) = (JSObject . toJSObject . toList) `liftM` execStateT f S.empty

-- --------------------------------------------------------------

-- | Set pure value under given name in final JSON object. Example:
--
-- > value "key" "value"
value :: (Monad m, ToJSValue a) => String -> a -> JSONGenT m ()
value name val = JSONGenT $ modify (S.|> (name, toJSValue val))

-- | Monadic verion of 'value' using monad transformer. Example:
--
-- > js <- runJSONGenT $ do
-- >          valueM "abc" (getLine)
valueM :: (Monad m, ToJSValue a) => String -> m a -> JSONGenT m ()
valueM name mval = lift mval >>= value name


-- | Embed other JSON object as field in a resulting JSON object. Example:
--
-- > let js = runJSONGen $ do
-- >            object "nested" $ do
-- >                value "abc" "def"
object :: Monad m => String -> JSONGenT m () -> JSONGenT m ()
object name json = JSONGenT $ do
  val <- lift $ runJSONGenT json
  modify (S.|> (name, toJSValue val))


-- | Version for lists of objects. Example:
--
-- > let js = runJSONGen $ do
-- >            objects "nested" [ do
-- >                                 value "abc" "def"
-- >                                 value "x" "y",
-- >                               do
-- >                                 value "qwe" "rty"
-- >                             ]
objects :: Monad m => String -> [JSONGenT m ()] -> JSONGenT m ()
objects name jsons = JSONGenT $ do
  val <- mapM (lift . runJSONGenT) jsons
  modify (S.|> (name, toJSValue val))
