{-# options_haddock prune #-}

-- |Utility functions for trace-printing values prefixed with the current source location.
module Incipit.Debug where

import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Stack (CallStack, SrcLoc (..), callStack, getCallStack)
import System.IO.Unsafe (unsafePerformIO)

import Incipit.Base (
  Applicative (pure),
  Functor ((<$)),
  HasCallStack,
  IO,
  Monad,
  Semigroup ((<>)),
  Show,
  error,
  fromMaybe,
  putStrLn,
  )
import Incipit.List (last)
import Incipit.String.Conversion (ToString (toString), ToText (toText), show)

srcLoc :: CallStack -> SrcLoc
srcLoc = \case
  (getCallStack -> (_, loc) : _) -> loc
  _ -> error "Debug.srcLoc: empty CallStack"

debugPrint ::
  SrcLoc ->
  Text ->
  IO ()
debugPrint SrcLoc {srcLocModule = (toText -> slm), srcLocStartLine} !msg =
  putStrLn (toString moduleName <> ":" <> show srcLocStartLine <> " " <> toString msg)
  where
    moduleName =
      fromMaybe slm (last (Text.splitOn "." slm))

debugPrintWithLoc ::
  Monad m =>
  SrcLoc ->
  Text ->
  m ()
debugPrintWithLoc loc msg = do
  () <- pure ()
  () <- pure (unsafePerformIO (debugPrint loc msg))
  pure ()

-- |Print a 'Text' in an arbitrary 'Monad'.
dbg ::
  HasCallStack =>
  Monad m =>
  Text ->
  m ()
dbg =
  debugPrintWithLoc (srcLoc callStack)
{-# noinline dbg #-}

-- |Print a value with a 'Show' instance in an arbitrary 'Monad'.
dbgs ::
  ∀ a m .
  HasCallStack =>
  Monad m =>
  Show a =>
  a ->
  m ()
dbgs a =
  debugPrintWithLoc (srcLoc callStack) (show a)
{-# noinline dbgs #-}

-- |Print a value with a 'Show' instance in an arbitrary 'Monad', returning the value.
dbgs_ ::
  ∀ a m .
  HasCallStack =>
  Monad m =>
  Show a =>
  a ->
  m a
dbgs_ a =
  a <$ debugPrintWithLoc (srcLoc callStack) (show a)
{-# noinline dbgs_ #-}

-- |Like 'Debug.Trace.trace', but with 'Text' and with source location prefix.
tr ::
  HasCallStack =>
  Text ->
  a ->
  a
tr msg a =
  unsafePerformIO (a <$ debugPrint (srcLoc callStack) msg)
{-# noinline tr #-}

-- |Like 'Debug.Trace.traceShow', but with source location prefix.
trs ::
  ∀ b a .
  Show b =>
  HasCallStack =>
  b ->
  a ->
  a
trs b a =
  unsafePerformIO (a <$ debugPrint (srcLoc callStack) (show b))
{-# noinline trs #-}

-- |Like 'Debug.Trace.traceShowId', but with source location prefix.
trsi ::
  Show a =>
  HasCallStack =>
  a ->
  a
trsi a =
  unsafePerformIO (a <$ debugPrint (srcLoc callStack) (show a))
{-# noinline trsi #-}
