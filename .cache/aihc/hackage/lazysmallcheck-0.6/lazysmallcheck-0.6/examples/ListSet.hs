module ListSet where

type Set a = [a]

empty :: Set a
empty = []

insert :: Ord a => a -> Set a -> Set a
insert a [] = [a]
insert a (x:xs)
  | a < x = a:x:xs
  | a > x = x:insert a xs
  | a == x = x:xs

set :: Ord a => [a] -> Set a
set = foldr insert empty

ordered [] = True
ordered [x] = True
ordered (x:y:zs) = x <= y && ordered (y:zs)

allDiff [] = True
allDiff (x:xs) = x `notElem` xs && allDiff xs

isSet s = ordered s && allDiff s

-- Properties

infixr 0 -->
False --> _ = True
True --> x = x

prop_insertSet :: (Char, Set Char) -> Bool
prop_insertSet (c, s) = ordered s --> ordered (insert c s)
