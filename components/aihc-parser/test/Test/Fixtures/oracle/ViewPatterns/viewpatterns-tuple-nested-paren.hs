{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsTupleNestedParen where

projectMaybe :: Maybe a -> Maybe a
projectMaybe = id

projectId :: a -> a
projectId x = x

f :: (Maybe a, ()) -> (a, ())
f (projectMaybe -> Just (projectId -> y), ()) = (y, ())
