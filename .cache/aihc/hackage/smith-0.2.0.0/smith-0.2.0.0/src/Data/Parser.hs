{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}

-- | Parse token sequences.
module Data.Parser
  ( Parser
  , Result(..)
  , Slice(..)
  , parseSmallArray
  , parseSmallArrayEither
  , parseSmallArrayEffectfully
    -- * Primitives
  , any
  , anySentinel
  , opt
  , peek
  , peekSentinel
  , optPeek
  , token
  , token2
  , token4
  , effect
  , fail
  , trySatisfy
    -- * Control Flow
  , foldSepBy1
  , foldSepByUntilEoi
  , listSepByUntilEoi
  , foldUntil
  , until
  , sepBy1_
  , sepBy1
  , skipWhile
    -- * End of Input
  , isEndOfInput
  , endOfInput
    -- * Lifting
  , liftEither
  ) where

import Prelude hiding (length,any,fail,until)

import Data.Bool (bool)
import Data.Primitive (SmallArray(..))
import Data.Bytes.Parser (Result(..),Slice(..))
import Data.Parser.Unsafe (Parser(..))
import GHC.Exts (TYPE,Int(I#),Int#)
import GHC.ST (ST(ST),runST)

import qualified Data.Primitive.Contiguous as C
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Data.List as List

type Result# e (a :: TYPE r) =
  (# e
  | (# a, Int#, Int# #) #) -- ints are offset and length

-- | Consumes and returns the next token from the input.
-- Fails if no tokens are left.
any :: e -> Parser a e s a
{-# inline any #-}
any e = uneffectful $ \array off len -> case len of
  0 -> Failure e
  _ ->
    let w = PM.indexSmallArray array off
     in Success (Slice (off + 1) (len - 1) w)

-- | Consumes and returns the next token from the input.
-- Returns the sentinel token if no tokens are left.
anySentinel :: a -> Parser a e s a
{-# inline anySentinel #-}
anySentinel sentinel = uneffectful $ \array off len -> case len of
  0 -> Success (Slice off len sentinel)
  _ ->
    let w = PM.indexSmallArray array off
     in Success (Slice (off + 1) (len - 1) w)

-- | Consume a token from the input or return @Nothing@ if
-- end of the stream has been reached. This parser never fails.
opt :: Parser a e s (Maybe a)
{-# inline opt #-}
opt = uneffectful $ \array off len -> case len of
  0 -> Success (Slice off 0 Nothing)
  _ ->
    let w = PM.indexSmallArray array off
     in Success (Slice (off + 1) (len - 1) (Just w))

-- | Looks at the next token from the input. If the token matches
-- the predicate, consume the token and return @True@. Otherwise,
-- do not consume the token and return @False@. If no tokens
-- remain in the input, return @False@. This parser never fails.
trySatisfy :: (a -> Bool) -> Parser a e s Bool
{-# inline trySatisfy #-}
trySatisfy p = uneffectful $ \array off len -> case len of
  0 -> Success (Slice off 0 False)
  _ -> let w = PM.indexSmallArray array off in
    case p w of
      True -> Success (Slice (off + 1) (len - 1) True)
      False -> Success (Slice off len False)

-- | Lift an effect into a parser.
effect :: ST s b -> Parser a e s b
{-# inline effect #-}
effect (ST f) = Parser
  (\(# _, off, len #) s0 -> case f s0 of
    (# s1, b #) -> (# s1, (# | (# b, off, len #) #) #)
  )

-- | Consumes and returns the next token from the input.
-- Fails if no tokens are left.
fail :: e -> Parser a e s b
{-# inline fail #-}
fail e = Parser (\_ s0 -> (# s0, (# e | #) #) )

-- | Consumes the next token from the input. Fails if it
-- is not equal to the expected value.
token :: Eq a
  => e -- ^ Error message
  -> a -- ^ Expected value of next token
  -> Parser a e s ()
{-# inline token #-}
token e a = do
  b <- any e
  bool (fail e) (pure ()) (a == b)

token2 :: Eq a => e -> a -> a -> Parser a e s ()
token2 e a b = token e a *> token e b

token4 :: Eq a => e -> a -> a -> a -> a -> Parser a e s ()
token4 e a b c d = token e a *> token e b *> token e c *> token e d

-- | Returns the next token from the input without consuming
-- it. Fails if no tokens are left.
peek :: e -> Parser a e s a
{-# inline peek #-}
peek e = uneffectful $ \array off len -> if len > 0
  then
    let w = PM.indexSmallArray array off
     in Success (Slice off len w)
  else Failure e

-- | Variant of 'peek' that returns the sentinel token instead of failing
-- when no tokens are left. It is prudent, but not required, to use
-- this only on input that does not contain the sentinel token.
peekSentinel ::
     a -- ^ Sentinel token
  -> Parser a e s a
{-# inline peekSentinel #-}
peekSentinel sentinel = uneffectful $ \array off len -> if len > 0
  then
    let w = PM.indexSmallArray array off
     in Success (Slice off len w)
  else Success (Slice off len sentinel)

-- | Returns the next token from the input without consuming it. Returns
-- @Nothing@ if at the end of the input.
optPeek :: Parser a e s (Maybe a)
{-# inline optPeek #-}
optPeek = uneffectful $ \array off len -> if len > 0
  then
    let w = PM.indexSmallArray array off
     in Success (Slice off len (Just w))
  else Success (Slice off len Nothing)

-- | Fold over the tokens, repeatedly running @step@ followed
-- by @separator@ until @separator@ returns 'False'. This is
-- strict in the accumulator and always runs @step@ at least
-- once. There is no backtracking; any failure causes the whole
-- combinator to fail.
foldSepBy1 ::
     Parser a e s Bool -- ^ Separator
  -> (b -> Parser a e s b) -- ^ Step
  -> b -- ^ Initial value
  -> Parser a e s b
{-# inline foldSepBy1 #-}
foldSepBy1 sep f b0 = f b0 >>= go
  where
  go !b = sep >>= \case
    True -> f b >>= go
    False -> pure b

foldSepByUntilEoi ::
     Parser a e s () -- ^ Separator (cannot check for end-of-input)
  -> (b -> Parser a e s b) -- ^ Step
  -> b -- ^ Initial value
  -> Parser a e s b
{-# inline foldSepByUntilEoi #-}
foldSepByUntilEoi sep f b0 = isEndOfInput >>= \case
  True -> pure b0
  False -> f b0 >>= go
  where
  go !b = isEndOfInput >>= \case
    True -> pure b
    False -> do
      sep
      f b >>= go

listSepByUntilEoi ::
     Parser a e s () -- ^ Separator (cannot check for end-of-input)
  -> Parser a e s b -- ^ Parse single element
  -> Parser a e s [b]
{-# inline listSepByUntilEoi #-}
listSepByUntilEoi sep f = do
  xs <- foldSepByUntilEoi sep
    (\ !acc -> do
      b <- f 
      pure (b : acc)
    ) []
  pure $! List.reverse xs

-- | Repeatedly run the parser, folding over the results, until a token
-- that satisfies the predicate is encountered. This is strict in the
-- accumulator. Tokens the do not match predicate are not consumed.
-- For example, consider the input sequence:
--
-- > Var "a", Var "x1", Var "x2", CloseParen
--
-- This could be matched by:
--
-- > P.foldUntil
-- >   (== CloseParen)
-- >   (\xs -> P.any ErrMsg >>= \case {Var x -> pure (x : xs); _ -> P.fail ErrMsg})
-- >   []
--
-- The accumulated list would be backwards in this example, and the
-- cursor would be positioned before, not after, @CloseParen@.
foldUntil ::
     (a -> Bool) -- ^ When token satisfies predicate, finish without consuming it
  -> (b -> Parser a e s b) -- ^ Step, consuming and accumulating
  -> b -- ^ Initial value
  -> Parser a e s b
{-# inline foldUntil #-}
foldUntil isTerminator step !b0 = go b0
  where
  go !b = optPeek >>= \case
    Nothing -> pure b
    Just t -> if isTerminator t
      then pure b
      else do
        b' <- step b
        go b'

-- | Variant of 'foldUntil' that collects elements into
-- an array.
until :: (C.ContiguousU arr, C.Element arr b)
  => (a -> Bool) -- ^ When token satisfies predicate, finish without consuming it
  -> Parser a e s b -- ^ Step, producing element for array 
  -> Parser a e s (arr b)
{-# inline until #-}
until isTerminator p = do
  let cap0 = 8
  buf0 <- effect (C.new cap0)
  let go !buf !ix !cap = if ix < cap
        then optPeek >>= \case
          Nothing -> effect (C.resize buf ix >>= C.unsafeFreeze)
          Just t -> if isTerminator t
            then effect (C.resize buf ix >>= C.unsafeFreeze)
            else do
              b <- p
              effect (C.write buf ix b)
              go buf (ix + 1) cap
        else do
          let cap' = cap * 2
          buf' <- effect $ do
            buf' <- C.new cap'
            C.copyMut buf' 0 (C.sliceMut buf 0 cap)
            pure buf'
          go buf' ix cap'
  go buf0 0 cap0

-- | Fold over the tokens, repeatedly running @step@ followed
-- by @separator@ until @separator@ returns 'False'. Collects
-- all parsed elements into an array (@PrimArray@, @Array@, etc.).
-- Consider the elements:
sepBy1 :: (C.ContiguousU arr, C.Element arr b)
  => Parser a e s Bool -- ^ Separator
  -> Parser a e s b -- ^ Element parser
  -> Parser a e s (arr b)
{-# inline sepBy1 #-}
sepBy1 sep p = do
  let cap0 = 8
  buf0 <- effect (C.new cap0)
  b0 <- p
  effect (C.write buf0 0 b0)
  let go !buf !ix !cap = if ix < cap
        then sep >>= \case
          True -> do
            b <- p
            effect (C.write buf ix b)
            go buf (ix + 1) cap
          False -> effect (C.resize buf ix >>= C.unsafeFreeze)
        else do
          let cap' = cap * 2
          buf' <- effect $ do
            buf' <- C.new cap'
            C.copyMut buf' 0 (C.sliceMut buf 0 cap)
            pure buf'
          go buf' ix cap'
  go buf0 1 cap0

-- | Skip tokens for which the predicate is true.
skipWhile ::
     (a -> Bool) -- ^ Predicate
  -> Parser a e s ()
{-# inline skipWhile #-}
skipWhile f = go where
  go = opt >>= \case
    Nothing -> pure ()
    Just t -> case f t of
      True -> go
      False -> internalUnconsume 1

-- | Fold over the tokens, repeatedly running @step@ followed
-- by @separator@ until @separator@ returns 'False'. The results
-- of @step@ are discarded, but in conjunction with @effect@,
-- this can be used to populate an array or a builder. This
-- always runs @step@ at least once.
--
-- > sepBy1 sep step === step *> (sep >>= bool (pure ()) (step *> (sep >>= bool (pure ()) (...))))
--
-- For example, consider this input sequence:
--
-- > Var "a", Comma, Var "x1", Comma, Var "x2", CloseParen
--
-- This could be matched by:
--
-- > P.sepBy1_
-- >   (P.any ErrMsg >>= \case
-- >     {Comma -> True; CloseParen -> False; _ -> P.fail ErrMsg}
-- >   )
-- >   (P.any ErrMsg >>= \case {Var _ -> pure (); _ -> P.fail ErrMsg})
sepBy1_ ::
     Parser a e s Bool -- ^ Separator
  -> Parser a e s b -- ^ Step
  -> Parser a e s ()
{-# inline sepBy1_ #-}
sepBy1_ sep f = f *> go where
  go = sep >>= \case
    True -> f *> go
    False -> pure ()

uneffectful :: (SmallArray a -> Int -> Int -> Result e b) -> Parser a e s b
{-# inline uneffectful #-}
uneffectful f = Parser
  ( \(# arr,off,len #) s0 -> (# s0, unboxResult (f (SmallArray arr) (I# off) (I# len)) #) )

unboxResult :: Result e a -> Result# e a
unboxResult (Success (Slice (I# b) (I# c) a)) = (# | (# a, b, c #) #)
unboxResult (Failure e) = (# e | #)

parseSmallArray ::
     forall a e b. (forall s. Parser a e s b)
  -> SmallArray a
  -> Result e b
parseSmallArray p (SmallArray arr) = runST action
  where
  action :: forall s. ST s (Result e b)
  action = case p @s of
    Parser f -> ST
      (\s0 -> case f (# arr, 0#, (Exts.sizeofSmallArray# arr) #) s0 of
        (# s1, r #) -> (# s1, boxResult r #)
      )

parseSmallArrayEither ::
     forall a e b. (forall s. Parser a e s b)
  -> SmallArray a
  -> Either e b
parseSmallArrayEither p !xs = case parseSmallArray p xs of
  Success (Slice _ _ c) -> Right c
  Failure e -> Left e

parseSmallArrayEffectfully :: Parser a e s b -> SmallArray a -> ST s (Result e b)
parseSmallArrayEffectfully (Parser f) (SmallArray arr) = ST
  (\s0 -> case f (# arr, 0#, (Exts.sizeofSmallArray# arr) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)
  )

boxResult :: Result# e a -> Result e a
boxResult (# | (# a, b, c #) #) = Success (Slice (I# b) (I# c) a)
boxResult (# e | #) = Failure e

-- A copy of unconsume so that the modules do not need to
-- be restructured.
internalUnconsume :: Int -> Parser a e s ()
{-# inline internalUnconsume #-}
internalUnconsume n = uneffectful $ \_ off len ->
  Success (Slice (off - n) (len + n) ())

liftEither :: Either e b -> Parser a e s b
liftEither = \case
  Left e -> fail e
  Right b -> pure b

-- | Returns true if there are no more tokens in the input. Returns false
-- otherwise. Always succeeds.
isEndOfInput :: Parser a e s Bool
isEndOfInput = uneffectful $ \_ off len -> case len of
  0 -> Success (Slice off 0 True)
  _ -> Success (Slice off len False)

endOfInput :: e -> Parser a e s ()
endOfInput e = uneffectful $ \_ off len -> case len of
  0 -> Success (Slice off 0 ())
  _ -> Failure e
