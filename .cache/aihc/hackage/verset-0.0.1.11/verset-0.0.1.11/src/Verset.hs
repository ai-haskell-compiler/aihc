{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Verset
  (
  -- * Basic functions
    (Data.Function.&)
  , (Data.Function.$)
  , Data.Function.const
  , Data.Function.fix
  , Data.Function.flip
  , Data.Function.on
  , (Prelude.$!)
  , Prelude.seq

  -- * Basic algebraic types

  -- ** Void
  , Data.Void.Void

  -- ** Bool
  , (Data.Bool.&&)
  , (Data.Bool.||)
  , Data.Bool.bool
  , Data.Bool.Bool(False, True)
  , Data.Bool.not
  , Data.Bool.otherwise

  -- ** Maybe
  , (?:)
  , Data.Maybe.catMaybes
  , Data.Maybe.fromMaybe
  , Data.Maybe.isJust
  , Data.Maybe.isNothing
  , Data.Maybe.mapMaybe
  , Data.Maybe.maybe
  , Data.Maybe.Maybe(Nothing, Just)

  -- ** List
  , Data.List.findIndex
  , Data.List.break
  , Data.List.drop
  , Data.List.dropWhile
  , Data.List.dropWhileEnd
  , Data.List.Extra.breakOn
  , Data.List.Extra.breakOnEnd
  , Data.List.Extra.dropEnd
  , Data.List.Extra.groupOn
  , Data.List.Extra.groupSort
  , Data.List.Extra.groupSortBy
  , Data.List.Extra.groupSortOn
  , Data.List.Extra.nubOrd
  , Data.List.Extra.nubOrdBy
  , Data.List.Extra.nubOrdOn
  , Data.List.Extra.spanEnd
  , Data.List.Extra.split
  , Data.List.Extra.splitOn
  , Data.List.Extra.takeEnd
  , Data.List.Extra.takeWhileEnd
  , atMay
  , Data.List.filter
  , Data.List.group
  , Data.List.groupBy
  , Data.List.inits
  , Data.List.intercalate
  , Data.List.intersperse
  , Data.List.isPrefixOf
  , Data.List.isSuffixOf
  , Data.List.iterate
  , Data.List.iterate'
  , Data.List.lookup
  , Data.List.permutations
  , Data.List.repeat
  , Data.List.replicate
  , Data.List.reverse
  , Data.List.scanl
  , Data.List.scanr
  , Data.List.sort
  , Data.List.sortBy
  , Data.List.sortOn
  , Data.List.span
  , Data.List.splitAt
  , Data.List.subsequences
  , Data.List.tails
  , Data.List.take
  , Data.List.takeWhile
  , Data.List.transpose
  , Data.List.unfoldr
  , Data.List.unzip
  , Data.List.unzip3
  , Data.List.zip
  , Data.List.zip3
  , Data.List.zipWith
  , Data.List.zipWith3
  , Data.List.genericLength
  , Data.List.genericTake
  , Data.List.genericDrop
  , Data.List.genericSplitAt
  , Data.List.genericIndex
  , Data.List.genericReplicate
  , Safe.cycleDef
  , Safe.cycleMay
  , Safe.headDef
  , Safe.headMay -- prefer pattern match
  , Safe.initDef
  , Safe.initMay
  , Safe.lastDef
  , Safe.lastMay
  , Safe.tailDef
  , Safe.tailMay -- prefer pattern match

  -- ** NonEmpty
  , Data.List.NonEmpty.NonEmpty((:|))
  -- (<|), -- in lens
  , Data.List.NonEmpty.scanl1
  , Data.List.NonEmpty.scanr1
  , Data.List.NonEmpty.head
  , Data.List.NonEmpty.init
  , Data.List.NonEmpty.last
  , Data.List.NonEmpty.tail
  , Data.List.NonEmpty.cycle

  -- ** Tuple
  , Data.Tuple.fst
  , Data.Tuple.snd
  , Data.Tuple.curry
  , Data.Tuple.uncurry
  , Data.Tuple.swap

  -- ** Either
  , Data.Either.Either(Left, Right)
  , Data.Either.either
  , Data.Either.Extra.fromLeft
  , Data.Either.Extra.fromRight
  , Data.Either.isLeft
  , Data.Either.isRight
  , Data.Either.lefts
  , Data.Either.rights
  , Data.Either.partitionEithers
  , Data.Either.Extra.eitherToMaybe
  , Data.Either.Extra.maybeToEither

  -- * Text types

  -- ** Char and String
  , Data.Char.Char

  -- ** String conversion
  , Data.String.IsString(fromString)


  -- * Container types

  -- ** Map and Set (Ordered)
  , Data.Map.Map
  , Data.Set.Set

  -- ** Seq
  -- ! , Data.Sequence.Seq


  -- * Numeric types

  -- ** Big integers
  , Prelude.Integer
  , Numeric.Natural.Natural

  -- ** Small integers
  , Data.Int.Int
  , Data.Int.Int16
  , Data.Int.Int32
  , Data.Int.Int64
  , Data.Int.Int8
  , Data.Word.Word
  , Data.Word.Word16
  , Data.Word.Word32
  , Data.Word.Word64
  , Data.Word.Word8

  -- ** Floating point
  , Prelude.Float
  , Prelude.Double

  -- * Numeric type classes

  -- ** Num
  , Prelude.Num((+), (-), (*), negate, abs, signum, fromInteger)
  , Prelude.subtract
  , (Prelude.^) -- partial functions!

  -- ** Real
  , Prelude.Real(toRational)
  , Prelude.realToFrac

  -- ** Integral
  , Prelude.Integral(quot, rem, div, mod, quotRem, divMod, toInteger) -- partial functions!
  , Data.Bits.toIntegralSized
  , Prelude.even
  , Prelude.odd

  -- ** Fractional
  , Prelude.Fractional((/), recip, fromRational) -- partial functions
  , (Prelude.^^)

  -- ** Floating
  , Prelude.Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh)
  -- ** RealFrac
  , Prelude.RealFrac(properFraction, truncate, round, ceiling, floor) -- partial functions

  -- ** RealFloat
  , Prelude.RealFloat(floatRadix, floatDigits, floatRange, decodeFloat, encodeFloat, exponent, significand, scaleFloat, isNaN, isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2)

  -- * Read and Show

  -- ** Show
  , Text.Show.Show
  , Data.Functor.Classes.Show1
  , Data.Functor.Classes.Show2
  --, Text.Show.show
  -- ! , showT
  -- ! , showS
  , show

  -- ** Read
  , Text.Read.Read
  , Data.Functor.Classes.Read1
  , Data.Functor.Classes.Read2
  , Text.Read.readMaybe

  -- * Equality and ordering

  -- ** Eq
  , Data.Eq.Eq((==), (/=))
  , Data.Functor.Classes.Eq1
  , Data.Functor.Classes.Eq2

  -- ** Ord
  , Data.Ord.Ord(compare, (<), (>), (<=), (>=), max, min)
  , Data.Functor.Classes.Ord1
  , Data.Functor.Classes.Ord2
  , Data.Ord.Ordering(LT,GT,EQ)
  , Data.Ord.Down(Down)
  , Data.Ord.comparing

  -- ** Enum
  , Prelude.Enum(-- toEnum, succ, pred, -- partial
       fromEnum, enumFrom, enumFromThen,
       enumFromTo, enumFromThenTo)
  , Safe.toEnumMay
  , Safe.toEnumDef
  , Safe.predMay
  , Safe.predDef
  , Safe.succMay
  , Safe.succDef

  -- ** Bounded
  , Prelude.Bounded(minBound, maxBound)

  -- * Algebraic type classes

  -- ** Category
  -- ! , Control.Category.Category(id, (.))
  , Control.Category.Category((.))
  , (Control.Category.<<<)
  , (Control.Category.>>>)

  -- ** Semigroup
  , Data.Semigroup.First(First, getFirst)
  , Data.Semigroup.Last(Last, getLast)
  , Data.Semigroup.Max(Max, getMax)
  , Data.Semigroup.Min(Min, getMin)
  , Data.Semigroup.Semigroup((<>), sconcat, stimes)

  -- ** Monoid
  , Data.Monoid.All(All, getAll)
  , Data.Monoid.Alt(Alt, getAlt)
  , Data.Monoid.Any(Any, getAny)
  , Data.Monoid.Dual(Dual, getDual)
  , Data.Monoid.Endo(Endo, appEndo)
  , Data.Monoid.Monoid(mempty, mappend, mconcat)

  -- ** Functor
  , Control.Applicative.Const(Const, getConst) -- Data.Functor.Const
  , (Data.Functor.<&>)
  , (Data.Functor.<$)
  , (Data.Functor.<$>)
  , (Data.Functor.$>)
  , Data.Functor.fmap
  , Data.Functor.Identity.Identity(Identity, runIdentity)
  , Data.Functor.void

  -- ** Contravariant
  , Data.Functor.Contravariant.Contravariant(
      (>$),
      contramap
      )
  , (Data.Functor.Contravariant.$<)
  , (Data.Functor.Contravariant.>$<)
  , (Data.Functor.Contravariant.>$$<)

  -- ** Foldable
  , Data.Foldable.Foldable(elem, fold, foldMap, foldr, foldr', foldl, foldl', product, sum, toList)
  , Data.Foldable.all
  , Data.Foldable.and
  , Data.Foldable.any
  , Data.Foldable.asum
  , Data.Foldable.concat
  , Data.Foldable.concatMap
  , Data.Foldable.find
  , Data.Foldable.foldlM
  , Data.Foldable.foldrM
  , Data.Foldable.for_
  , Data.Foldable.length
  , Data.Foldable.notElem
  , Data.Foldable.null
  , Data.Foldable.or
  , Data.Foldable.sequenceA_
  , Data.Foldable.traverse_
  , Safe.Foldable.foldl1May
  , Safe.Foldable.foldr1May
  , Safe.Foldable.maximumBound
  , Safe.Foldable.maximumBoundBy
  , Safe.Foldable.maximumBounded
  , Safe.Foldable.maximumByMay
  , Safe.Foldable.maximumMay
  , Safe.Foldable.minimumBound
  , Safe.Foldable.minimumBoundBy
  , Safe.Foldable.minimumBounded
  , Safe.Foldable.minimumByMay
  , Safe.Foldable.minimumMay

  -- ** Traversable
  , Data.Traversable.Traversable(traverse, sequenceA)
  , Data.Traversable.for
  , Data.Traversable.mapAccumL
  , Data.Traversable.mapAccumR

  -- ** Applicative
  , Control.Applicative.Applicative(pure, (<*>), (*>), (<*))
  , Control.Applicative.ZipList(ZipList, getZipList)
  , (Control.Applicative.<**>)
  , Control.Applicative.liftA2
  , Control.Applicative.liftA3
  , pass

  -- ** Alternative
  , Control.Applicative.Alternative((<|>), empty, many {-, some -})
  , Control.Applicative.optional
  , Data.List.NonEmpty.some1

  -- ** Monad
  , (Control.Monad.<=<)
  , (Control.Monad.=<<)
  , (Control.Monad.>=>)
  , (Control.Monad.>>)
  , (Control.Monad.<$!>)
  , Control.Monad.ap
  , (Control.Monad.Extra.&&^)
  , (Control.Monad.Extra.||^)
  , Control.Monad.Extra.allM
  , Control.Monad.Extra.andM
  , Control.Monad.Extra.anyM
  , Control.Monad.Extra.concatMapM
  , Control.Monad.Extra.ifM
  , Control.Monad.Extra.orM
  , Control.Monad.Extra.unlessM
  , Control.Monad.Extra.whenM
  , Control.Monad.filterM
  , Control.Monad.Fix.MonadFix(mfix)
  , Control.Monad.forM
  , Control.Monad.forM_
  , Control.Monad.foldM
  , Control.Monad.foldM_
  , Control.Monad.forever
  , Control.Monad.guard
  , Control.Monad.join
  , Control.Monad.liftM
  , Control.Monad.liftM2
  , Control.Monad.liftM3
  , Control.Monad.liftM4
  , Control.Monad.liftM5
  , Control.Monad.mapAndUnzipM
  , Control.Monad.mfilter
  , Control.Monad.Monad((>>=))
  , Control.Monad.replicateM
  , Control.Monad.replicateM_
  , Control.Monad.unless
  , Control.Monad.when
  , Control.Monad.zipWithM
  , Control.Monad.zipWithM_

  , Control.Exception.Exception
  , Control.Exception.SomeException

  -- ** Bifunctor
  , Data.Bifunctor.Bifunctor(bimap, first, second)

  -- ** Bifoldable
  , Data.Bifoldable.Bifoldable(bifoldr
                              --, bifoldl -- not strict enough
                              , bifoldMap)
  , Data.Bifoldable.bifoldl'
  , Data.Bifoldable.bifoldr'
  , Data.Bifoldable.bitraverse_
  , Data.Bifoldable.bisequenceA_
  , Data.Bifoldable.bifor_

  -- ** Bitraversable
  , Data.Bitraversable.Bitraversable(bitraverse)
  , Data.Bitraversable.bifor
  , Data.Bitraversable.bisequenceA

  -- * Effects and monad transformers
  , Control.Monad.Trans.MonadTrans(lift)

  , Control.Concurrent.ThreadId
  , Control.Concurrent.forkIO
  , Control.Concurrent.forkFinally
  , Control.Concurrent.threadDelay
  , Control.Concurrent.myThreadId
  , Control.Concurrent.STM.atomically

  -- * Generic type classes
  , GHC.Generics.Generic
  , GHC.Generics.Generic1
  , Data.Typeable.Typeable
  , GHC.Real.fromIntegral

  -- * Type level
  , Data.Kind.Type
  , Data.Proxy.Proxy(Proxy)

  -- * IO
  , System.IO.IO
  , Control.Monad.Trans.MonadIO(liftIO)

  -- ** Console
  , print
  , Print
  , hPutStr
  , putStr
  , hPutStrLn
  , putStrLn
  , putErrLn
  , putText
  , putErrText
  , putLText
  , putByteString
  , putLByteString

  -- * Error handling and debugging
  , HasCallStack
  , Control.Monad.Fail.MonadFail
  , undefined
  , error
  , trace
  , traceIO
  , traceId
  , traceM
  , traceShow
  , traceShowId
  , traceShowM

  -- * Time
  , Data.Time.UTCTime
  , Data.Time.LocalTime
  , Data.Time.NominalDiffTime


  -- * Custom
  , Data.UUID.UUID
  , BS.ByteString
  , Data.Text.Text
  , System.IO.FilePath
  , (<<$>>)
  , ordNub
  , identity
  , Control.Monad.Functor

) where

import qualified Control.Applicative
import           Control.Applicative (Applicative, pure)
import qualified Control.Category
import qualified Control.Concurrent
import qualified Control.Exception
import qualified Control.Monad
import           Control.Monad ((>>))
--import qualified Control.Monad.Except
import qualified Control.Monad.Extra
import qualified Control.Monad.Fail
import qualified Control.Monad.Fix
import qualified Control.Monad.Trans
import           Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Bifoldable
import qualified Data.Bifunctor
import qualified Data.Bitraversable
import qualified Data.Bits
import qualified Data.Bool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (Char)
import qualified Data.Either
import qualified Data.Either.Extra
import qualified Data.Eq
import qualified Data.Foldable
import           Data.Foldable (Foldable, foldl', foldr)
import qualified Data.Function
import           Data.Function ((.), ($))
import qualified Data.Functor
import qualified Data.Functor.Classes
import qualified Data.Functor.Contravariant
import           Data.Functor (Functor(fmap))
import qualified Data.Functor.Identity
import qualified Data.Int
import qualified Data.Kind
import qualified Data.List
import qualified Data.List.Extra
import           Data.List (groupBy, sortBy)
import qualified Data.List.NonEmpty
import qualified Data.Map
import qualified Data.Maybe
import           Data.Maybe (fromMaybe)
import qualified Data.Monoid
import qualified Data.Ord
import           Data.Ord (Ord, comparing)
import qualified Data.Proxy
import qualified Data.Semigroup
import           Data.Semigroup (Semigroup((<>)))
import qualified Data.Set
import qualified Data.Set as Set
import           Data.String (IsString(fromString), String)
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy as TxtL
import qualified Data.Text.Lazy.IO as TxtL
import           Data.Text (Text)
import qualified Data.Traversable
import qualified Data.Tuple
import qualified Data.Typeable
import qualified Data.Void
import qualified Data.Word
import qualified Debug.Trace
import qualified GHC.Generics
import qualified GHC.Real
import qualified GHC.Show
import           GHC.Stack (HasCallStack)
import qualified Numeric.Natural
import qualified Prelude
import qualified Safe
import qualified Safe.Foldable
import qualified System.IO
import           System.IO (FilePath)
import qualified Text.Read
import           Text.Show (Show)
import           Prelude (Maybe(..), Bool(..), otherwise, const, (<), (-))
import qualified Data.UUID
import qualified Data.Time
import qualified Control.Concurrent.STM

import qualified Verset.Conv as Conv

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- from intro

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
-- Printable types are those that are instances of class 'Show'; 'print'
-- converts values to strings for output using the 'show' operation and
-- adds a newline.
--
-- For example, a program to print the first 20 integers and their
-- powers of 2 could be written as:
--
-- > main = print ([(n, 2^n) | n <- [0..19]])
--
-- __Note__: This function is lifted to the 'MonadIO' class.
print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . System.IO.print
{-# INLINE print #-}


-- | Throw an undefined error. Use only for debugging.
undefined :: HasCallStack => a
undefined = Prelude.undefined
{-# WARNING undefined "'undefined' should be used only for debugging" #-}


error :: HasCallStack => String -> a
error = Prelude.error
{-# WARNING error "'error' should be used only for debugging" #-}


-- | An infix form of 'fromMaybe' with arguments flipped.
(?:) :: Maybe a -> a -> a
(?:) = Data.Function.flip fromMaybe
infix 1 ?:
{-# INLINE (?:) #-}

-- | @()@ lifted to an 'Control.Applicative.Applicative'.
--
--   @pass = 'Control.Applicative.pure' ()@
pass :: Applicative f => f ()
pass = pure ()
{-# INLINE pass #-}



-- | The 'trace' function outputs the trace message given as its first argument,
-- before returning the second argument as its result.
--
-- For example, this returns the value of @f x@ but first outputs the message.
--
-- > trace ("calling f with x = " ++ show x) (f x)
--
-- The 'trace' function should /only/ be used for debugging, or for monitoring
-- execution. The function is not referentially transparent: its type indicates
-- that it is a pure function but it has the side effect of outputting the
-- trace message.
trace :: Text -> a -> a
trace = Debug.Trace.trace . Txt.unpack
{-# WARNING trace "'trace' should be used only for debugging" #-}

-- | Like 'trace' but returning unit in an arbitrary 'Applicative' context. Allows
-- for convenient use in do-notation.
--
-- Note that the application of 'traceM' is not an action in the 'Applicative'
-- context, as 'traceIO' is in the 'MonadIO' type. While the fresh bindings in the
-- following example will force the 'traceM' expressions to be reduced every time
-- the @do@-block is executed, @traceM "not crashed"@ would only be reduced once,
-- and the message would only be printed once.  If your monad is in 'MonadIO',
-- @traceIO@ may be a better option.
--
-- > ... = do
-- >   x <- ...
-- >   traceM $ "x: " ++ show x
-- >   y <- ...
-- >   traceM $ "y: " ++ show y
traceM :: Applicative m => Text -> m ()
traceM = Debug.Trace.traceM . Txt.unpack
{-# WARNING traceM "'traceM' should be used only for debugging" #-}

-- | Like 'trace', but uses 'show' on the argument to convert it to a 'String'.
--
-- This makes it convenient for printing the values of interesting variables or
-- expressions inside a function. For example here we print the value of the
-- variables @x@ and @z@:
--
-- > f x y =
-- >     traceShow (x, z) $ result
-- >   where
-- >     z = ...
-- >     ...
traceShow :: Show a => a -> b -> b
traceShow = Debug.Trace.traceShow
{-# WARNING traceShow "'traceShow' should be used only for debugging" #-}

-- | Like 'traceM', but uses 'show' on the argument to convert it to a 'String'.
--
-- > ... = do
-- >   x <- ...
-- >   traceShowM $ x
-- >   y <- ...
-- >   traceShowM $ x + y
traceShowM :: (Show a, Applicative m) => a -> m ()
traceShowM = Debug.Trace.traceShowM
{-# WARNING traceShowM "'traceShowM' should be used only for debugging" #-}

-- | The 'traceIO' function outputs the trace message from the IO monad.
-- This sequences the output with respect to other IO actions.
traceIO :: MonadIO m => Text -> m ()
traceIO = liftIO . Debug.Trace.traceIO . Txt.unpack
{-# WARNING traceIO "'traceIO' should be used only for debugging" #-}

-- | Like 'traceShow' but returns the shown value instead of a third value.
traceShowId :: Show a => a -> a
traceShowId = Debug.Trace.traceShowId
{-# WARNING traceShowId "'traceShowId' should be used only for debugging" #-}

-- | Like 'trace' but returns the message instead of a third value.
traceId :: Text -> Text
traceId a = Debug.Trace.trace (Txt.unpack a) a
{-# WARNING traceId "'traceId' should be used only for debugging" #-}

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- From protolude


-- | The identity function, returns the give value unchanged.
identity :: a -> a
identity x = x

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap


class Print a where
  hPutStr :: MonadIO m => System.IO.Handle -> a -> m ()
  putStr :: MonadIO m => a -> m ()
  putStr = hPutStr System.IO.stdout
  hPutStrLn :: MonadIO m => System.IO.Handle -> a -> m ()
  putStrLn :: MonadIO m => a -> m ()
  putStrLn = hPutStrLn System.IO.stdout
  putErrLn :: MonadIO m => a -> m ()
  putErrLn = hPutStrLn System.IO.stderr

instance Print Txt.Text where
  hPutStr = \h -> liftIO . Txt.hPutStr h
  hPutStrLn = \h -> liftIO . Txt.hPutStrLn h

instance Print TxtL.Text where
  hPutStr = \h -> liftIO . TxtL.hPutStr h
  hPutStrLn h v = liftIO $ TxtL.hPutStr h v >> TxtL.hPutStr h "\n"

instance Print BS.ByteString where
  hPutStr = \h -> liftIO . BS.hPutStr h
  hPutStrLn h v = liftIO $ BS.hPutStr h v >> BS.hPutStr h "\n"

instance Print BSL.ByteString where
  hPutStr = \h -> liftIO . BSL.hPutStr h
  hPutStrLn h v = liftIO $ BSL.hPutStr h v >> BSL.hPutStr h "\n"

instance Print [Char] where
  hPutStr = \h -> liftIO . System.IO.hPutStr h
  hPutStrLn = \h -> liftIO . System.IO.hPutStrLn h

-- For forcing type inference
putText :: MonadIO m => Txt.Text -> m ()
putText = putStrLn
{-# SPECIALIZE putText :: Txt.Text -> System.IO.IO () #-}

putLText :: MonadIO m => TxtL.Text -> m ()
putLText = putStrLn
{-# SPECIALIZE putLText :: TxtL.Text -> System.IO.IO () #-}

putByteString :: MonadIO m => BS.ByteString -> m ()
putByteString = putStrLn
{-# SPECIALIZE putByteString :: BS.ByteString -> System.IO.IO () #-}

putLByteString :: MonadIO m => BSL.ByteString -> m ()
putLByteString = putStrLn
{-# SPECIALIZE putLByteString :: BSL.ByteString -> System.IO.IO () #-}

putErrText :: MonadIO m => Txt.Text -> m ()
putErrText = putErrLn
{-# SPECIALIZE putErrText :: Txt.Text -> System.IO.IO () #-}

show :: (Show a, Conv.StringConv String b) => a -> b
show x = Conv.toS (GHC.Show.show x)
{-# SPECIALIZE show :: Show  a => a -> Data.Text.Text  #-}
{-# SPECIALIZE show :: Show  a => a -> Data.Text.Lazy.Text  #-}
{-# SPECIALIZE show :: Show  a => a -> String  #-}


-- O(n * log n)
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x : xs) =
      if x `Set.member` s
        then go s xs
        else x : go (Set.insert x s) xs


atMay :: [a] -> Prelude.Int -> Maybe a
atMay xs n
  | n < 0     = Nothing
             -- Definition adapted from GHC.List
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
{-# INLINABLE atMay #-}
