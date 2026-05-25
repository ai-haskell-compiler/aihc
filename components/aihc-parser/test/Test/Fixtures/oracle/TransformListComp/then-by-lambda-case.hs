{- ORACLE_TEST pass -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TransformListComp #-}

module TransformListCompThenByLambdaCase where

x = [ []
    | let _ = []
    , then []
      `a` \case
        _ -> [] by []
    ]
