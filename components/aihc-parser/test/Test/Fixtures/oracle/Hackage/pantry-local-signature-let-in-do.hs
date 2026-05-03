{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
module PantryLocalSignatureLetInDo where

f x = do
  let
      err = Left x

      test :: Eq a => Maybe a -> a -> Bool
      test (Just y) z = y == z
      test Nothing _ = True

      tests =
        [ test (Just x) x
        , test Nothing x
        ]

   in if and tests then Right x else err
