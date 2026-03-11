module Data.List.Tools (
  takeUntil
, dropUntil
, mulLists
, defaultElem
, isIncludedElem

, setAt
, modifyAt
) where

takeUntil :: (a -> Bool) -> [a] -> [a]
dropUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []     = []
takeUntil p (x:xs)
  | p x            = [x]
  | otherwise      = x : takeUntil p xs
dropUntil _ []     = []
dropUntil p (x:xs)
  | p x            = xs
  | otherwise      = dropUntil p xs

mulLists :: [[a]] -> [[a]]
mulLists []       = [[]]
mulLists (xs:xss) = [ x:xs_ | x <- xs, xs_ <- mulLists xss ]

defaultElem :: a -> [a] -> [a]
defaultElem dflt []  = [ dflt ]
defaultElem _    lst = lst

isIncludedElem :: Eq a => [a] -> [a] -> Bool
isIncludedElem lst1 lst2 = and $ map (flip elem lst2) lst1

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

modifyAt :: [a] -> Int -> (a -> a) -> [a]
modifyAt xs i f = take i xs ++ [f $ xs !! i] ++ drop (i + 1) xs
