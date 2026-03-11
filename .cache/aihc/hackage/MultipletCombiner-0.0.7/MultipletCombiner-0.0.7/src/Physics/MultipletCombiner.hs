{- |
Module      :  Physics.MultipletCombiner
Copyright   :  (c) Michael Dressel 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Michael Dressel <michael.dressel@kloenplatz.de>
Stability   :  experimental

This module contains operators and functions for
combining SU(n) multiplets according to the
algorithm presented by C.G. Wohl in the PDG book 2021 section 48
<https://pdg.lbl.gov/2022/reviews/rpp2022-rev-young-diagrams.pdf>.

It provides the operators '(><)' and '(>><)' for combining multiplets,
and the function 'multi' and 'multis' to calculate the multiplicities, e.g.:

@

    [1,0] '><' [0,1] = [[1,1],[0,0]]

    'multi' [1,0] = 3

    [1,0] '><' [1,0] '>><' [1,0] = [[3,0],[1,1],[1,1],[0,0]]

    'multis' $ [1,0] '><' [1,0] '>><' [1,0] = [10,8,8,1]

@




Example for combining two multiplets using Young-Diagrams:

@

    (0,0)x(0,0) = (0,0)
    #     a   # a  # a   # a
    # (x) b = #  > # b > # b
    #     c   #    #     # c

    (1,0)x(1,0) = (step ->)   (2,0)      +   (step ->) (0,1)
    # #    a a    # # a a    # # a a           # # a    # # a
    #   x  b    = #       >  # b         +     # a    > # a b
    #      c      #          # c               #        # c

@

-}



module Physics.MultipletCombiner
    (
    -- * Kronecker product like operators
        (><),
        (>><),
        (><^),
    -- * Multiplicity calculation
        multi,
        multis,
    -- * Basic data type
        Tableau,
    -- * Lower level functions
        ytSymbols,
        ytsSymbols,
        showt,
        ytNums,
        ytsNums,
        admis,
        unchain,
        sym2letter,
        appendAt,
        readTab,
        combis,
        tabs1,
        allTsFromSyms,
        allTs
    ) where

import Data.Char
import Data.Ratio

-- | Basic type used for a Tableau/Diagram
newtype Tableau = Tableau [String] deriving (Eq)
instance Show Tableau where
    show (Tableau t) = unlines t

-- | Show like function to display a list of tableaux.
showt :: [Tableau] -> String
showt [] = "----"
showt [t] = show t ++ "----"
showt (t:ts) = show t ++ "----\n" ++ showt ts

-- ytSymbols [0] = ["# ",
--                   "# "]
-- ytSymbols [1] = ["# # ",
--                   "# "]
-- ytSymbols [2] = ["# # ",
--                   "# "]
-- ytSymbols [0,0] = ["# ",
--                     "# ",
--                     "# "]
-- ytSymbols [0,1] = ["# # ",
--                     "# # ",
--                     "# "]
-- ytSymbols [1,0] = ["# # ",
--                     "# ",
--                     "# "]
-- ytSymbols [1,1] = ["# # #",
--                     "# # ",
--                     "# "]
-- ytSymbols [0,2] = ["# # #",
--                     "# # #",
--                     "# "]
-- ytSymbols [1,2] = ["# # # #",
--                     "# # #",
--                     "# "]
-- ytSymbols [2,2] = ["# # # # #",
--                     "# # #",
--                     "# "]
-- ytSymbols [2,1] = ["# # # #",
--                     "# #",
--                     "# "]
-- ytSymbols [2,0] = ["# # #",
--                     "# ",
--                     "# "]

-- | Build a tableau bottom up from it's label.
ytSymbols :: [Int] -> Tableau
ytSymbols [] = Tableau []
ytSymbols is = go (reverse is) (Tableau ["# "])
              where
                go :: [Int] -> Tableau -> Tableau
                go []         t = t
                go (i:is) (Tableau (r:rs)) = go is $ Tableau $
                        concat (r : replicate i "# ") : r : rs
-- | Build multiple tableaux from multiple labels.
ytsSymbols :: [[Int]] -> [Tableau]
ytsSymbols = map ytSymbols


-- | Calculate the number representation from a tableau.
ytNums :: Tableau -> [Int]
ytNums (Tableau [])  = []
ytNums (Tableau [l]) = []
ytNums (Tableau (l:m:ns)) = length l' - length m' : ytNums (Tableau (m:ns))
                    where l' = noBlank l
                          m' = noBlank m
                          noBlank :: String -> String
                          noBlank xs = [ x | x <- xs, x /= ' ']

-- | Calculate the list of labels fro a list of tableaux.
ytsNums :: [Tableau] -> [[Int]]
ytsNums [] = []
ytsNums (t:ts) = case ytNums t of
                            [] -> ytsNums ts
                            is -> is : ytsNums ts

-- | Check for the string for being composed of admissible letters.
--   Admissible and not admissible examples:
--
-- @
--
-- admis "aabacdaebbcbd"  = True
--
-- last letter not admissible
-- admis "abacae"  = False
-- admis "abacdec"  = False
--
-- @

admis :: String -> Bool
admis xs = case unchain 'a' xs of
                Nothing -> False
                Just [] -> True
                Just cs -> admis cs

-- | Extract one strictly ordered chain from the given string, starting
--   at the given character.
unchain :: Char -> String -> Maybe String
unchain _ [] = Just []
unchain x (c:cs) | x==c = unchain (chr (ord x +1)) cs
                 | c<x  = case unchain x cs of
                           Nothing -> Nothing
                           Just cs'-> Just (c:cs')
                 | c>x  = Nothing

-- | Convert a tableau of symbols  into a tableau of letters
sym2letter :: Tableau -> Tableau
sym2letter (Tableau xss) = Tableau $
                                zipWith line2let xss ['a'..]
                where line2let :: String -> Char -> String
                      line2let [] _ = []
                      line2let (x:xs) c | x == '#' = c:line2let xs c
                                        | x == ' ' = x:line2let xs c


-- | Append a string to the i'th line of a tableau.
appendAt :: Int -> String -> Tableau -> Tableau
appendAt _ _ (Tableau []) = Tableau []
appendAt _ [] t = t
appendAt i s (Tableau ts) | i > length ts || i < 1 = Tableau []
                | otherwise = Tableau $ take (i-1) ts ++ [(ts !! (i-1)) ++ s]
                                ++ drop i ts

-- | Produce a list of placing-coordinates of all combinations for a tableau
--   with t rows to place c character.
--
--   E.g.: 3 rows, two characters -> 3*3 possible placements:
--
--       1:1, 1:2, 1:3, 2:1, 2:2, 2:3, 3:1, 3:2, 3:3
combis :: Int -> Int -> [[Int]]
combis t c = go t c [[]]
        where
            go :: Int -> Int -> [[Int]] -> [[Int]]
            go _ 0 is = is
            go 0 _ is = is
            go t c is = go t (c-1) (extend t is)

extend  :: Int -> [[Int]] -> [[Int]]
extend p is = [ x:y  | x <- [1..p], y <- is]

-- | Create a new tableau extended by string s, onto tableau t. Where
--   s is placed at every position given by the list of integers is.
newtab :: String -> Tableau -> [Int] -> Tableau
newtab _ t  [] = t
newtab s t (i:is) = newtab s (appendAt i s t) is

-- | Create multiple new tableau using newtab given one tableau and
--  one line of a right side tableau.
--
--   e.g.: tabs1 (ytSymbols [1,1,1]) "a a "
tabs1 :: Tableau -> String -> [Tableau]
tabs1 t r = go t s (combis j k)
            where
                go :: Tableau -> String -> [[Int]] -> [Tableau]
                go _ _ [] = []
                go t s (is:iss) | rowsOK t' && colsOK t' = t' : go t s iss
                                | otherwise = go t s iss
                                where
                                    t' = newtab s t is
                s = sym r
                j = nlines t
                k = elemrow r

nlines :: Tableau -> Int
nlines (Tableau ts) = length ts

rowsOK :: Tableau -> Bool
rowsOK (Tableau []) = True
rowsOK (Tableau [x]) = True
rowsOK (Tableau (x:y:zs)) = length x >= length y && rowsOK (Tableau (y:zs))

colsOK :: Tableau -> Bool
colsOK (Tableau []) = True
colsOK (Tableau [s]) = True
colsOK (Tableau (x:y:zs)) = col2OK x y && colsOK (Tableau (y:zs))

col2OK :: String -> String -> Bool
col2OK _ [] = True
col2OK [] _ = True
col2OK (l:ls) (r:rs) | l == ' ' || r == ' ' = col2OK ls rs
                     | l == '#' || r == '#' = col2OK ls rs
                     | l == r = False
                     | otherwise = col2OK ls rs

-- | allTs [1,0]  [1,1]
--
--   Create all tableau from two tableaux identified by their labels.
--
--   @
--   putStrLn $ showt $ noDoubs.admisTabs $ allTs [1,1] [1,1]
--   ytsNums $ noDoubs.admisTabs $ allTs [1,1] [1,1]
--   [[2,2],[3,0],[0,3],[1,1],[1,1],[0,0]]
--   @

allTs :: [Int] -> [Int] -> [Tableau]
allTs lt rt | length lt /= length rt = []
            | otherwise = allTsFromSyms (ytSymbols lt)
                ((sym2letter.ytSymbols) rt)

-- | Create all tableaux from two given tableaux.
allTsFromSyms :: Tableau -> Tableau -> [Tableau]
allTsFromSyms lts rts | nlines lts /= nlines rts = []
                      | otherwise = go [lts] rts
                        where
                            go :: [Tableau] -> Tableau-> [Tableau]
                            go  ts (Tableau []) = ts
                            go  [] _  = []
                            go  (t:ts) (Tableau (r:rs)) =
                                        go (tabs1 t r) (Tableau rs) ++
                                                    go ts (Tableau (r:rs))


elemrow :: String -> Int
elemrow = length.strip

sym :: String -> String
sym xs = (head.strip) xs : " "

strip :: String -> String
strip xs = [x | x <- xs, x /= ' ', x /= '#']

-- | Read a string of letters from a given tableau to be checked
--    for admissibility.
readTab :: Tableau -> String
readTab (Tableau []) = ""
readTab (Tableau (l:ls)) = (strip.reverse) l ++ readTab (Tableau ls)

admisTabs :: [Tableau] -> [Tableau]
admisTabs = filter (admis.readTab)

-- | Remove duplicate tableaux but keep different tableaux with
--   even with equal labels.
noDoubs :: [Tableau] -> [Tableau]
noDoubs [] = []
noDoubs (t:ts) | t `elem` ts = noDoubs ts
               | otherwise   = t : noDoubs ts

-- | Produce multiplet structure from combining two SU(n) multiplets
(><) :: [Int] -> [Int] -> [[Int]]
(><) l r | length l /= length r = []
         | otherwise = ytsNums $ noDoubs.admisTabs $ allTs l r

-- | Produce multiplet structure from combining a list of multiplets with
--   another multiplet
(>><) :: [[Int]] -> [Int] -> [[Int]]
(>><) ls r | all (length r ==) [length l | l <- ls] =
                concat [ l >< r | l <- ls ]
           | otherwise = []

-- | Produce multiplet structure by combining one multiplet repeatedly
(><^) :: [Int] -> Int -> [[Int]]
(><^) t n | n < 1 = []
(><^) t n = pot [[]] t n
    where   pot :: [[Int]] -> [Int] -> Int -> [[Int]]
            pot l r 1 = [r]
            pot l r n = pot l r (n-1)  >>< r

-- ghci> [1,0] >< [1,0] >>< [1,0]
-- [[3,0],[1,1],[1,1],[0,0]]

-- | Calculate the multiplicity of a multiplet
multi :: [Int] -> Int
multi [] = 0
multi is = round $ multt (length is) is

multt :: Int -> [Int] -> Ratio Int
multt 0 _ = 1
multt l is = multl l is * multt (l-1) is

multl ::  Int -> [Int] -> Ratio Int
multl _ [] = 1
multl l (i:is) | length (i:is) >= l =
                        ((sum (take l (i:is)) + l) % l) * multl l is
               | otherwise = 1

-- | Calculate the multiplicities of a list of multiplets
multis :: [[Int]] -> [Int]
multis = fmap multi





