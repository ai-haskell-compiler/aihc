{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts (PromptTag#, control0#, newPromptTag#, prompt#)
import GHC.IO (IO (..), unIO)

-- | A boxed prompt tag, so that the primitives below can be used from
-- ordinary 'IO' code.
data PromptTag a = PromptTag (PromptTag# a)

newPromptTag :: IO (PromptTag a)
newPromptTag = IO (\s -> case newPromptTag# s of (# s', tag #) -> (# s', PromptTag tag #))

-- | Delimit a computation. A prompt that nothing aborts to is a no-op.
prompt :: PromptTag a -> IO a -> IO a
prompt (PromptTag tag) (IO action) = IO (prompt# tag action)

-- | Capture the computation up to the nearest 'prompt' with the tag, remove
-- it together with the prompt, and run the function in the prompt's place.
-- The captured continuation does not reinstall the prompt.
control0 :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control0 (PromptTag tag) f =
  IO (control0# tag (\k -> unIO (f (\(IO action) -> IO (k action)))))

-- | Stop early: the continuation is dropped, so the prompt yields the
-- function's result instead of what the rest of the computation would have
-- produced.
earlyExit :: IO ()
earlyExit = do
  tag <- newPromptTag
  result <-
    prompt tag $ do
      x <- control0 tag (\_ -> pure "stopped before the rest ran")
      pure (x ++ " -- never seen")
  putStrLn result

-- | Resume the same continuation twice. Each resumption runs the rest of
-- the computation again on a fresh copy of the captured frames.
twice :: IO ()
twice = do
  tag <- newPromptTag
  result <-
    prompt tag $ do
      x <- control0 tag $ \k -> do
        a <- k (pure 1)
        b <- k (pure 2)
        pure (a + b)
      pure (x * 10)
  putStrLn ("resumed twice: " ++ show (result :: Int))

-- | A generator. Each yield aborts to the prompt with the value and the rest
-- of the generator; collecting reinstalls the prompt around the resumption,
-- because a resumed continuation does not carry one of its own.
yieldValue :: PromptTag [Int] -> Int -> IO ()
yieldValue tag x = control0 tag (\k -> (x :) <$> prompt tag (k (pure ())))

collect :: PromptTag [Int] -> IO () -> IO [Int]
collect tag generator = prompt tag (generator >> pure [])

generator :: IO ()
generator = do
  tag <- newPromptTag
  values <- collect tag $ do
    yieldValue tag 1
    yieldValue tag 2
    yieldValue tag 3
  putStrLn ("generated: " ++ show values)

main :: IO ()
main = do
  earlyExit
  twice
  generator
