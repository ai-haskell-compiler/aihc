{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

module DoLetInMultilineBindings where

f x = do
  let a =
        case x of
          True -> 1
          False -> 2
      b =
        case x of
          True -> 3
          False -> 4
    in if x then a else b
