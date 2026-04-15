{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

module RollbarInfixDo where

catch :: IO a -> (String -> IO a) -> IO a
catch = undefined

test :: IO (Maybe Int)
test =
  do
    x <- return 42
    return (Just x)
    `catch` (\e -> return Nothing)
