{- ORACLE_TEST xfail reason="infix backtick continuation after do block statement not parsed" -}
{-# LANGUAGE GHC2021 #-}

module RollbarInfixDo where

catch :: IO a -> (String -> IO a) -> IO a
catch = undefined

test :: IO (Maybe Int)
test = do
    x <- return 42
    return (Just x)
    `catch` (\e -> return Nothing)
