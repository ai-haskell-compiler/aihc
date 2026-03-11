{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

-- | Generic continuation-based parser

module Data.EDN.Class.Parser
  ( parseEither
  , parseM
  , Parser(..)
  , Success
  , Failure
  , Expected
  , Label
  , parserError
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
#if MIN_VERSION_base(4,13,0)
#else
import Data.Semigroup (Semigroup(..))
#endif

import qualified Control.Monad.Fail as Fail
import qualified Data.List as List

-- | Run a 'Parser' reporting to arbitrary 'Monad' with 'fail'.
parseM :: Fail.MonadFail m => (a -> Parser b) -> a -> m b
parseM p v = runParser (p v) (unexpected fail) pure

-- | Run a 'Parser' reporting to an 'Either'.
parseEither :: (a -> Parser b) -> a -> Either String b
parseEither p v = runParser (p v) (unexpected Left) Right

-- | Helper to convert expected labels to a failure value.
unexpected
  :: (String -> a) -- ^ Smart-constructor for failure value
  -> Expected      -- ^ Accumulated expected labels
  -> String        -- ^ Failure message
  -> a
unexpected failWith [] err =
  failWith err
unexpected failWith es err =
  failWith $ unlines
    [ err
    , "expected"
    , "  " <> List.intercalate ", " es
    ]

-- | A continuation-based parser type.
newtype Parser a = Parser
  { runParser
    :: forall f r.
       Failure f r
    -> Success a f r
    -> f r
  }

-- | Megaparsec-style collection of elements expected by combined parser alternatives.
type Expected = [Label]

-- | Single element expected by a parser. 'String' because 'Control.Monad.Fail.MonadFail' method.
type Label = String

-- | Failure continuation.
type Failure f r = Expected -> String -> f r

-- | Success continuation.
type Success a f r = a -> f r

instance Functor Parser where
  fmap f p =
    Parser $ \kf ks ->
      runParser p kf $ \a ->
        ks (f a)
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a =
    Parser $ \_kf ks ->
      ks a
  {-# INLINE pure #-}

  d <*> e =
    d >>= \b -> fmap b e
  {-# INLINE (<*>) #-}

instance Alternative Parser where
  empty =
    Parser $ \kf _ks ->
      kf mempty "empty"
  {-# INLINE empty #-}

  a <|> b =
    Parser $ \kf ks ->
      let
       kf' expected _ = runParser b (kf . mappend expected) ks
      in
       runParser a kf' ks
  {-# INLINE (<|>) #-}

instance Fail.MonadFail Parser where
  fail = parserError
  {-# INLINE fail #-}

instance Monad Parser where
  m >>= g =
    Parser $ \kf ks ->
      let
        ks' a = runParser (g a) kf ks
      in
        runParser m kf ks'
  {-# INLINE (>>=) #-}

  return = pure
  {-# INLINE return #-}

#if MIN_VERSION_base(4,12,0)
#else
  fail = Fail.fail
  {-# INLINE fail #-}
#endif

instance MonadPlus Parser where
  mzero = fail "mzero"
  {-# INLINE mzero #-}

  mplus a b = a <|> b
  {-# INLINE mplus #-}

instance Semigroup (Parser a) where
  (<>) = mplus

instance Monoid (Parser a) where
  mempty  = fail "mempty"
  {-# INLINE mempty #-}

  mappend = (<>)
  {-# INLINE mappend #-}

parserError :: String -> Parser a
parserError msg = Parser $ \kf _ks ->
  kf mempty msg
{-# INLINE parserError #-}
