{-# LANGUAGE TypeFamilies #-}

module FamilyBindChecks (familyBindChecks) where

-- | A data family with a data instance and a newtype instance. A pattern
-- on an instance constructor in a @do@ bind or a list comprehension
-- generator matches the representation type, so the desugarer casts the
-- bound value with the family axiom before the match.
data family Store s a

data instance Store s Bool = BoolStore Int Int

newtype instance Store s Char = CharStore Int

-- | The index of the family is an associated type of the monad, so the
-- scrutinee of a bind is an unreduced family application.
class (Monad m) => Owner m where
  type Token m
  freshBool :: Int -> m (Store (Token m) Bool)
  freshChar :: Int -> m (Store (Token m) Char)

instance Owner Maybe where
  type Token Maybe = ()
  freshBool n = Just (BoolStore n (n + 1))
  freshChar n = Just (CharStore n)

sumStores :: (Owner m) => Int -> m Int
sumStores n = do
  stores@(BoolStore first second) <- freshBool n
  CharStore third <- freshChar n
  let BoolStore fourth _ = stores
  return (first + second + third + fourth)

pairs :: [Store () Bool] -> [Int]
pairs stores = [first + second | BoolStore first second <- stores]

familyBindChecks :: Bool
familyBindChecks = sumStores 1 == Just 5 && pairs [BoolStore 1 2, BoolStore 3 4] == [3, 7]
