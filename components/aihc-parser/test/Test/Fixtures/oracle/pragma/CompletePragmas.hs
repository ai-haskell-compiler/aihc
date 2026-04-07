{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module CompletePragmas where

data Choice a = Choice Bool a

pattern LeftChoice :: a -> Choice a
pattern LeftChoice a = Choice False a

pattern RightChoice :: a -> Choice a
pattern RightChoice a = Choice True a

{-# COMPLETE LeftChoice, RightChoice #-}

choiceToInt :: Choice Int -> Int
choiceToInt (LeftChoice n) = n
choiceToInt (RightChoice n) = negate n

data Proxy a = Proxy

class IsEmpty a where
  isEmpty :: a -> Bool

class IsCons a where
  type Elt a
  isCons :: a -> Maybe (Elt a, a)

pattern Empty :: IsEmpty a => a
pattern Empty <- (isEmpty -> True)

pattern Cons :: IsCons a => Elt a -> a -> a
pattern Cons x xs <- (isCons -> Just (x, xs))

instance IsEmpty (Proxy a) where
  isEmpty Proxy = True

instance IsEmpty [a] where
  isEmpty = null

instance IsCons [a] where
  type Elt [a] = a
  isCons [] = Nothing
  isCons (x:xs) = Just (x, xs)

{-# COMPLETE Empty :: Proxy #-}
{-# COMPLETE Empty, Cons :: [] #-}

proxyCase :: Proxy a -> Int
proxyCase Empty = 0

listCase :: [a] -> Int
listCase Empty = 0
listCase (Cons _ _) = 1
