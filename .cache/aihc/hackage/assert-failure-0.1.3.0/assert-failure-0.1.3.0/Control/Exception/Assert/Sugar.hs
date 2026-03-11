{-# LANGUAGE RankNTypes #-}
-- | Syntactic sugar that improves the usability of 'Control.Exception.assert'
-- and 'error'. The original @assert@ function is here re-exported
-- for convenience.
--
-- Make sure to enable assertions for your cabal package, e.g., by setting
--
-- > ghc-options: -fno-ignore-asserts
--
-- in your .cabal file. Otherwise, some of the functions will have
-- no effect at all.
module Control.Exception.Assert.Sugar
  ( assert, blame, showFailure, swith, allB
    -- * DEPRECATED
  , twith, failure, forceEither
  ) where

import           Control.Exception (assert)
import           Data.Text (Text)
import           Debug.Trace (trace)
import           Prelude
import qualified Text.Show.Pretty as Show.Pretty (ppShow)

infix 1 `blame`
-- | If the condition fails, display the value blamed for the failure.
-- Used as in
--
-- > assert (age < 120 `blame` age) $ savings / (120 - age)
blame :: Show v => Bool -> v -> Bool
{-# INLINABLE blame #-}
blame True _ = True
blame False blamed = trace (blameMessage blamed) False

blameMessage :: Show v => v -> String
blameMessage blamed = "Contract failed and the following is to blame:\n  "
                      ++ Show.Pretty.ppShow blamed

infix 2 `showFailure`
-- | A helper function for 'error'. To be used as in
--
-- > case xs of
-- >   0 : _ -> error $ "insignificant zero" `showFailure` xs
--
-- Fixing the first argument to @String@ instead of anything Showable
-- prevents warnings about defaulting, even when @OverloadedStrings@
-- extension is enabled.
showFailure :: Show v => String -> v -> String
{-# INLINABLE showFailure #-}
showFailure s blamed =
  "Internal failure occurred and the following is to blame:\n  "
  ++ s ++ "\n  "
  ++ Show.Pretty.ppShow blamed

infix 2 `swith`
-- | Syntactic sugar for the pair operation, to be used for 'blame' as in
--
-- > assert (age < 120 `blame` "age too high" `swith` age) $ savings / (120 - age)
--
-- Fixing the first component of the pair to @String@ prevents warnings
-- about defaulting, even when @OverloadedStrings@ extension is enabled.
swith :: String -> v -> (String, v)
{-# INLINE swith #-}
swith s blamed = (s, blamed)

-- | Like 'List.all', but if the predicate fails, blame all the list elements
-- and especially those for which it fails. To be used as in
--
-- > assert (allB (<= height) [yf, y1, y2])
allB :: Show v => (v -> Bool) -> [v] -> Bool
{-# INLINABLE allB #-}
allB predicate l = case all predicate l of
  True -> True
  False -> trace (allBMessage predicate l) False

allBMessage :: Show v => (v -> Bool) -> [v] -> String
{-# INLINABLE allBMessage #-}
allBMessage predicate l =
  "The following items on the list don't respect the contract:\n"
  ++ Show.Pretty.ppShow (filter (not . predicate) l)
  ++ "\nout of all the list items below:\n"
  ++ Show.Pretty.ppShow l

-- * DEPRECATED

infix 2 `twith`
-- | Syntactic sugar for the pair operation, to be used for 'blame' as in
--
-- > assert (age < 120 `blame` "age too high" `twith` age) $ savings / (120 - age)
-- Fixing the first component of the pair to @Text@ prevents warnings
-- about defaulting, even when @OverloadedStrings@ extension is enabled.
{-# DEPRECATED twith
      "consider using 'swith' instead, for simplicity, because GHC optimizes lazy 'String' constants very well." #-}
twith :: Text -> b -> (Text, b)
{-# INLINE twith #-}
twith t b = (t, b)

infix 1 `failure`
-- | Like 'error', but shows the source position (in newer GHCs
-- @error@ shows source position as well, hence deprecation)
-- and also the value to blame for the failure. To be used as in
--
-- > case xs of
-- >   0 : _ -> assert `failure` (xs, "has an insignificant zero")
{-# DEPRECATED failure
      "use 'error' and 'showFailure' instead, now that 'error' prints source positions." #-}
failure :: Show a => (forall x. Bool -> x -> x) -> a -> b
{-# NOINLINE failure #-}
failure asrt blamed =
  let s = "Internal failure occurred and the following is to blame:\n  "
          ++ Show.Pretty.ppShow blamed
  in trace s
     $ asrt False
     $ error "Control.Exception.Assert.Sugar.failure"
         -- Lack of no-ignore-asserts or GHC < 7.4.

infix 1 `forceEither`
-- | Assuming that @Left@ signifies an error condition,
-- check the @Either@ value and, if @Left@ is encountered,
-- fail outright and show the error message (in newer GHCs
-- @error@ shows source position as well, hence deprecation). Used as in
--
-- > assert `forceEither` parseOrFailWithMessage code
forceEither :: Show a => (forall x. Bool -> x -> x) -> Either a b -> b
{-# DEPRECATED forceEither
      "use 'either (error . show) id' instead, now that 'error' prints source positions." #-}
{-# NOINLINE forceEither #-}
forceEither asrt (Left a)  = asrt `failure` a
forceEither _    (Right b) = b
