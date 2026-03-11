{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.ConstraintsEncoded
Copyright   :  (c) OleksandrZhabenko 2020-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Provides a way to encode the needed constraint with possibly less symbols.
Uses arrays instead of vectors.
-}
module Aftovolio.ConstraintsEncoded (
    -- * Data types
    EncodedContraints (..),
    EncodedCnstrs,

    -- * Functions to work with them

    -- ** Read functions
    readMaybeECG,

    -- ** Process-encoding functions
    decodeConstraint1,
    decodeLConstraints,
    isConstraint1,

    -- ** Modifiers and getters
    getIEl,
    setIEl,

    -- ** Predicates
    isE,
    isP,
    isF,
    isQ,
    isT,
    isSA,
    isSB,
    isV,
    isW,
    isH,
    isR,
    isM,
    isN,
    isD,
    isI,
    isU,
    isG,

    -- * Algebraic general conversion
    validOrdStr,
    generalConversion,
    filterGeneralConv,
) where

import Aftovolio.Constraints
import Data.Char (isDigit, isLetter)
import Data.InsertLeft (InsertLeft (..), splitAtEndG)
import Data.List (groupBy, nub, words)
import Data.Maybe
import Data.Tuple (fst)
import GHC.Arr
import GHC.Base
import GHC.List
import GHC.Num (abs, (+), (-))
import Text.Read (readMaybe)
import Text.Show (Show (..), show)

data EncodedContraints a b d
    = -- | Represents no additional constraint, corresponds to the whole set of theoretically possible permutations.
      E
    | -- | Represents the set of permutations with the fixed positions of some elements.
      P a b
    | -- | Represents the set of permutations with the preserved pairwise order between first and second, second and third, third and fourth elements.
      Q a a a a a
    | -- | Represents the set of permutations with the preserved pairwise order between first and second, second and third elements.
      T a a a a
    | -- | Represents the set of permutations with the preserved position of the elements AFTER the another selected one.
      SA a a b
    | -- | Represents the set of permutations with the preserved position of the elements BEFORE the another selected one.
      SB a a b
    | -- | Represents the set of permutations with the preserved order between first and second elements.
      F a a a
    | -- | Represents the set of permutations with the preserved both distance between and order of the two elements.
      V a a a
    | -- | Represents the set of permutations with the preserved distance between the two elements.
      W a a a
    | -- | Represents the set of permutations with the preserved both distances between and order of the three elements.
      H a a a a
    | -- | Represents the set of permutations with the preserved pairwise distances between the first and second, second and third elements.
      R a a a a
    | -- | Represents the set of permutations with the preserved pairwise distances between the first and second, second and third elements, and additionally the order of the first and second elements.
      M a a a a
    | -- | Represents the set of permutations with the moved fixed positions of some elements (at least one).
      N a d
    | -- | Pepresents the set of permutations with the specified order and distance between the two elements.
      D a a a a
    | -- | Pepresents the set of permutations with the specified distance between the two elements.
      I a a a a
    | -- | Represents the set of permutations with the preserved order of the 5 elements
      U a a a a a a
    | -- | Represents the set of permutations with the groupped more than two elements together
      G a a a
    deriving (Eq, Ord, Show)

validOrdStr0 ::
    String ->
    -- | Number of seen so far \'(\' parentheses
    Int ->
    -- | Number of seen so far \')\' parentheses
    Int ->
    Bool
validOrdStr0 ('E' : ys) n m = validOrdStr0 ys n m
validOrdStr0 (' ' : y : t : ys) n m
    | y `elem` "ABDFGHIMNPQRTUVW" && isDigit t =
        validOrdStr0 (dropWhile isDigit ys) n m
    | y `elem` "-(E" = validOrdStr0 (y : t : ys) n m
    | otherwise = False
validOrdStr0 ('(' : y : t : ys) n m
    | y `elem` "ABDFGHIMNPQRTUVW" && isDigit t =
        validOrdStr0 (dropWhile isDigit ys) (n + 1) m
    | y `elem` "-(E" = validOrdStr0 (y : t : ys) (n + 1) m
    | otherwise = False
validOrdStr0 (')' : y : t : ys) n m
    | y `elem` "ABDFGHIMNPQRTUVW" && isDigit t =
        validOrdStr0 (dropWhile isDigit ys) n (m + 1)
    | y `elem` "-()E" = validOrdStr0 (y : t : ys) n (m + 1)
    | otherwise = False
validOrdStr0 ('-' : y : t : ys) n m
    | y `elem` "ABDFGHIMNPQRTUVW" && isDigit t =
        validOrdStr0 (dropWhile isDigit ys) n m
    | y `elem` "-)" || isDigit y = False
    | otherwise = validOrdStr0 (y : t : ys) n m
validOrdStr0 (x : y : t : ys) n m
    | x `elem` "ABDFGHIMNPQRTUVW" && isDigit y =
        validOrdStr0 (dropWhile isDigit (t : ys)) n m
    | x `elem` "ABDFGHIMNPQRTUVW" = False
    | otherwise = validOrdStr0 (y : t : ys) n (m + 1)
validOrdStr0 (x : ')' : ys) n m
    | isDigit x = validOrdStr0 ys n (m + 1)
    | x == ')' = validOrdStr0 ys n (m + 2)
    | otherwise = False
validOrdStr0 (x : y : _) n m
    | x `elem` "(ABDFGHIMNQRTUVW" = False
    | y `elem` " -(ABDFGHIMNPQRTUVW" = False
    | x == 'P' && not (isDigit y) = False
    | x == ')' && y /= 'E' = False
    | x == 'P' && n == m = True
    | x == ')' && y == 'E' = n == (m + 1)
    | (x `elem` "E -") && y == 'E' = n == m
    | otherwise = False
validOrdStr0 (x : _) n m
    | isDigit x || (x `elem` ")E") = if x == ')' then n == (m + 1) else n == m
    | otherwise = False
validOrdStr0 _ n m = n == m

{- | An extended predicate to check whether the 'String' is a probably correct representation of the
constraints algebraic expression for 'generalConversion' evaluation.
-}
validOrdStr :: String -> Bool
validOrdStr xs = validOrdStr0 xs 0 0
{-# INLINE validOrdStr #-}

stage1Parsing :: String -> [String]
stage1Parsing =
    groupBy
        ( \x y -> x == '(' && y == '(' || isLetter x && isDigit y || x == ')' && y == ')'
        )
{-# INLINE stage1Parsing #-}

-- | At the moment is used only for the list of 'String' without any parentheses in each of them.
convertToBools ::
    Int ->
    Array Int Int ->
    [String] ->
    -- | The result is a 'String' that Haskell can evaluate to 'Bool' (some logical expression).
    String
convertToBools n arr ("-" : yss) = "not " `mappend` (convertToBools n arr yss)
convertToBools n arr (" " : yss) = " || " `mappend` (convertToBools n arr yss)
convertToBools n arr (xs : yss@(ys : _))
    | xs `elem` ["True", "False"] =
        xs
            `mappend` ( case ys of
                            " " -> " "
                            _ -> " && "
                      )
            `mappend` convertToBools n arr yss
    | otherwise =
        let cnstrs = fromMaybe E . readMaybeECG n $ xs
         in show (isConstraint1 True arr cnstrs)
                `mappend` ( case ys of
                                " " -> " "
                                _ -> " && "
                          )
                `mappend` convertToBools n arr yss
convertToBools n arr (xs : _)
    | xs `elem` ["True", "False"] = xs
    | otherwise =
        (show . isConstraint1 True arr . fromMaybe E . readMaybeECG n $ xs)
convertToBools _ _ _ = []

splitNoParenAtDisjunction :: [String] -> [[String]]
splitNoParenAtDisjunction xss@(_ : _)
    | null tss = []
    | otherwise = tss : splitNoParenAtDisjunction wss
  where
    (tss, uss) = break (== "||") xss
    wss = drop 1 uss
splitNoParenAtDisjunction _ = []

noParenString0 :: [String] -> Bool
noParenString0 (xs : ys : ts : yss)
    | xs == "not" =
        case ys of
            "True" -> False
            _ -> noParenString0 yss
    | otherwise =
        case xs of
            "True" -> noParenString0 (ts : yss)
            _ -> False
noParenString0 ("not" : ys : _) = if ys == "True" then False else True
noParenString0 (xs : _)
    | xs == "True" = True
    | otherwise = False
noParenString0 _ = True

noParenString :: [String] -> Bool
noParenString = or . map noParenString0 . splitNoParenAtDisjunction
{-# INLINE noParenString #-}

oneStep :: Int -> Array Int Int -> [String] -> Bool
oneStep m arr = noParenString . words . convertToBools m arr
{-# INLINE oneStep #-}

oneChange :: Int -> Array Int Int -> [String] -> [String]
oneChange m arr xss
    | null wss = [show . oneStep m arr $ xss]
    | otherwise =
        ( (\(jss, _, qss) -> jss `mappend` [show . oneStep m arr $ qss])
            . foldr
                ( \xs (tss, n, rss) ->
                    if xs == "(" && n == 0
                        then (tss, 1, rss)
                        else
                            if any (== '(') xs && n == 0
                                then (drop 1 xs : tss, 1, rss)
                                else case n of
                                    0 -> (tss, 0, xs : rss)
                                    _ -> (xs : tss, 1, rss)
                )
                ([], 0, [])
            $ yss
        )
            `mappend` kss
  where
    (yss, wss) = break (any (== ')')) xss
    kss = case wss of
        ")" : vss -> vss
        ws : vss -> drop 1 ws : vss
        _ -> []

generalConversion :: Int -> String -> Array Int Int -> Bool
generalConversion m xs arr
    | validOrdStr xs =
        (\ks -> if ks == "True" || ks == "E" then True else False)
            . head
            . head
            . dropWhile ((/= 1) . length)
            . drop 1
            . iterate (oneChange m arr)
            . stage1Parsing
            $ xs
    | otherwise = False
{-# INLINE generalConversion #-}

-- | Can be thought of as 'filter' ('generalConversion' ... ) @<arrays>@ but is somewhat more efficient.
filterGeneralConv :: Int -> String -> [Array Int Int] -> [Array Int Int]
filterGeneralConv m cnstrns xs
    | validOrdStr cnstrns =
        let !xss = stage1Parsing cnstrns
         in filter
                ( \arr ->
                    (\ks -> if ks == "True" || ks == "E" then True else False)
                        . head
                        . head
                        . dropWhile ((/= 1) . length)
                        . drop 1
                        . iterate (oneChange m arr) $
                        xss
                )
                xs
    | otherwise = []
{-# INLINE filterGeneralConv #-}

{- | Inspired by the: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Maybe.html
Is provided here as a more general way to read the 'String' into a 'EncodedCnstrs'.
It is up to user to check whether the parameters are in the correct form, the function does
not do the full checking.
-}
readMaybeECG :: Int -> String -> Maybe EncodedCnstrs
readMaybeECG n xs
    | null xs = Nothing
    | n >= 0 && n <= 9 =
        let h = head xs
            ts = filter (\x -> x >= '0' && [x] <= show n) . tail $ xs
         in case h of
                'E' -> Just E
                _ -> f n h ts
    | otherwise = Nothing
  where
    f n c ts
        | c `elem` "DFGHIMQRQTUVW" =
            let ys0 = catMaybes . map (\t -> readMaybe [t] :: Maybe Int) $ ts
                ys = nub ys0
                (jjs, ps) = splitAtEndG 1 ys0
                res
                    | length ys0 >= 3 && (c `elem` "DI") =
                        let qs = take 2 . nub $ jjs
                            [y, z] = map (\rr -> if rr == 0 then 9 else rr - 1) qs
                         in if length qs /= 2 || ps == [0] || ps > [n]
                                then Nothing
                                else Just ((if c == 'D' then D else I) n y z (head ps))
                    | length ys /= g c = Nothing
                   | c `elem` "FVW" =
                        let [y, z] = map (\rr -> if rr == 0 then 9 else rr - 1) ys
                         in Just ((case c of 'F' -> F; 'V' -> V; _ -> W) n y z)
                    | c `elem` "HMT" =
                        let [y, z, u] = map (\rr -> if rr == 0 then 9 else rr - 1) ys
                         in Just ((case c of 'T' -> T; 'H' -> H; 'M' -> M; _ -> R) n y z u)
                    | c == 'G' =
                        let [y0, z0] = map (\rr -> if rr == 0 then 9 else rr - 1) ys
                            (y, z) 
                                 | y0 < z0 = (y0, z0)
                                 | otherwise = (z0, y0) 
                         in Just (G n y z)
                    | c == 'Q' =
                        let [y, z, u, w] = map (\rr -> if rr == 0 then 9 else rr - 1) ys
                         in Just (Q n y z u w)
                    | c == 'U' =
                        let [y, z, t, u, w] = map (\rr -> if rr == 0 then 9 else rr - 1) ys
                         in Just (U n y z t u w)
                    | otherwise = Nothing
             in res
        | c `elem` "AB" =
            let y = readMaybe (take 1 ts) :: Maybe Int
             in if isJust y
                    then
                        let y0 = fromJust y
                            zs =
                                map (\rr -> if rr == 0 then 9 else rr - 1)
                                    . filter (/= y0)
                                    . nub
                                    . catMaybes
                                    . map (\t -> readMaybe [t] :: Maybe Int)
                                    . drop 1 $
                                    ts
                         in case zs of
                                [] -> Nothing
                                ~x2 ->
                                    Just
                                        ( (if c == 'A' then SA else SB)
                                            n
                                            (if y0 == 0 then 9 else y0 - 1)
                                            (listArray (0, length x2 - 1) x2)
                                        )
                    else Nothing
        | c == 'P' =
            if null ts
                then Just E
                else
                    Just
                        . P n
                        . listArray (0, length ts - 1)
                        . map (\r -> case (fromJust (readMaybe [r] :: Maybe Int)) of 0 -> 9; q -> q - 1) $
                        ts
        | c == 'N' =
            if tl == 0
                then Just E
                else
                    Just
                        . N n
                        . listArray (0, tl - 1)
                        . map
                            ( (\[s, w] -> (w, s))
                                . map (\r -> case (fromJust (readMaybe [r] :: Maybe Int)) of 0 -> 9; q -> q - 1)
                            ) $
                        h3
        | otherwise = Nothing
      where
        h1 (b : d : ds) = [b, d] : h1 ds
        h1 _ = []
        h2 = h1 ts
        qqs = map head h2
        pps = map last h2
        h3
            | length (nub qqs) == length qqs && length (nub pps) == length pps = h2
            | otherwise = []
        tl = length h3
    g c
        | c `elem` "FGVW" = 2
        | c == 'Q' = 4
        | c == 'U' = 5
        | otherwise = 3

type EncodedCnstrs =
    EncodedContraints Int (Array Int Int) (Array Int (Int, Int))

{- | Must be applied to the correct array of permutation indeces. Otherwise, it gives runtime error (exception). All the integers inside the
'EncodedCnstrs' must be in the range [0..n-1] where @n@ corresponds to the maximum element in the permutation 'Array' 'Int' 'Int'.
-}
decodeConstraint1 ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    EncodedCnstrs ->
    t (Array Int Int) ->
    t (Array Int Int)
decodeConstraint1 E = id
decodeConstraint1 (P _ v) = fixedPointsS v
decodeConstraint1 (Q _ i j k l) = unsafeQuadruples i j k l
decodeConstraint1 (T _ i j k) = unsafeTriples i j k
decodeConstraint1 (SA _ i v) = unsafeSeveralA i v
decodeConstraint1 (SB _ i v) = unsafeSeveralB i v
decodeConstraint1 (F _ i j) = filterOrderIJ i j
decodeConstraint1 (V _ i j) = filterSignDistanceIJ i j (abs $ j - i)
decodeConstraint1 (W _ i j) = filterUnsignDistanceIJ i j (abs $ j - i)
decodeConstraint1 (H _ i j k) = filterSignDistanceIJK3 i j k (abs $ j - i) (abs $ k - j)
decodeConstraint1 (R _ i j k) = filterUnsignDistanceIJK3 i j k (abs $ j - i) (abs $ k - j)
decodeConstraint1 (M _ i j k) = filterMixedDistanceIJK3 i j k (abs $ j - i) (abs $ k - j)
decodeConstraint1 (N _ v) = fixedPointsG v
decodeConstraint1 (D _ i j d) = filterSignDistanceIJ i j (abs d)
decodeConstraint1 (I _ i j d) = filterUnsignDistanceIJ i j (abs d)
decodeConstraint1 (U _ i j k l m) = unsafeQuintuples i j k l m
decodeConstraint1 (G _ i j) = filterGrouppedTogether i j
{-# SPECIALIZE decodeConstraint1 ::
    EncodedCnstrs -> [Array Int Int] -> [Array Int Int]
    #-}

{- | Must be applied to the correct array of permutation indeces. Otherwise, it gives runtime error (exception). All the integers inside the
'EncodedCnstrs' must be in the range [0..n-1] where @n@ corresponds to the maximum element in the permutation 'Array' 'Int' 'Int'.
-}
decodeLConstraints ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    [EncodedCnstrs] ->
    t (Array Int Int) ->
    t (Array Int Int)
decodeLConstraints (x : xs) = decodeLConstraints' ys . decodeConstraint1 y
  where
    y = minimum (x : xs)
    ys = filter (/= y) . g $ (x : xs)
    g (E : zs) = g zs
    g (z : zs) = z : g zs
    g _ = []
    decodeLConstraints' (z : zs) = decodeLConstraints' zs . decodeConstraint1 z
    decodeLConstraints' _ = id
decodeLConstraints _ = id
{-# SPECIALIZE decodeLConstraints ::
    [EncodedCnstrs] -> [Array Int Int] -> [Array Int Int]
    #-}

isConstraint1 :: Bool -> Array Int Int -> EncodedCnstrs -> Bool
isConstraint1 bool _ E = bool
isConstraint1 True arr (F _ i j) = unsafeOrderIJ i j arr
isConstraint1 True arr (T _ i j k) = isTripleOrdered i j k arr
isConstraint1 True arr (Q _ i j k l) = isQuadrupleOrdered i j k l arr
isConstraint1 True arr (SA _ i arr2) = isSeveralAOrdered i arr2 arr
isConstraint1 True arr (SB _ i arr2) = isSeveralBOrdered i arr2 arr
isConstraint1 True arr (P _ arr2) = isFixedPoint arr2 arr
isConstraint1 True arr (H _ i j k) = isSignDistIJK3 i j k (abs $ j - i) (abs $ k - j) arr
isConstraint1 True arr (M _ i j k) = isMixedDistIJK3 i j k (abs $ j - i) (abs $ k - j) arr
isConstraint1 True arr (R _ i j k) = isUnsignDistIJK3 i j k (abs $ j - i) (abs $ k - j) arr
isConstraint1 True arr (V _ i j) = unsafeSignDistanceIJ i j (abs $ j - i) arr
isConstraint1 True arr (W _ i j) = unsafeUnsignDistanceIJ i j (abs $ j - i) arr
isConstraint1 True arr (N _ arr2) = isFixedPointTup arr2 arr
isConstraint1 True arr (D _ i j d) = unsafeSignDistanceIJ i j (abs d) arr
isConstraint1 True arr (I _ i j d) = unsafeUnsignDistanceIJ i j (abs d) arr
isConstraint1 True arr (U _ i j k l m) = isQuintupleOrdered i j k l m arr
isConstraint1 True arr (G _ i j) = isGrouppedTogether i j arr
isConstraint1 False arr (F _ i j) = unsafeOrderIJ j i arr
isConstraint1 False arr (T _ i j k) = notTripleOrdered i j k arr
isConstraint1 False arr (Q _ i j k l) = notQuadrupleOrdered i j k l arr
isConstraint1 False arr (SA _ i arr2) = notSeveralAOrdered i arr2 arr
isConstraint1 False arr (SB _ i arr2) = notSeveralBOrdered i arr2 arr
isConstraint1 False arr (P _ arr2) = notFixedPoint arr2 arr
isConstraint1 False arr (H _ i j k) = notSignDistIJK3 i j k (abs $ j - i) (abs $ k - j) arr
isConstraint1 False arr (M _ i j k) = notMixedDistIJK3 i j k (abs $ j - i) (abs $ k - j) arr
isConstraint1 False arr (R _ i j k) = notUnsignDistIJK3 i j k (abs $ j - i) (abs $ k - j) arr
isConstraint1 False arr (V _ i j) = unsafeSignDistanceIJ j i (abs $ j - i) arr
isConstraint1 False arr (W _ i j) = not . unsafeUnsignDistanceIJ i j (abs $ j - i) $ arr
isConstraint1 False arr (N _ arr2) = notFixedPointTup arr2 arr
isConstraint1 False arr (D _ i j d) = unsafeSignDistanceIJ j i (abs d) arr
isConstraint1 False arr (I _ i j d) = not . unsafeUnsignDistanceIJ i j (abs d) $ arr
isConstraint1 False arr (U _ i j k l m) = notQuintupleOrdered i j k l m arr
isConstraint1 False arr (G _ i j) = notGrouppedTogether i j arr

isE :: EncodedCnstrs -> Bool
isE E = True
isE _ = False

isP :: EncodedCnstrs -> Bool
isP (P _ _) = True
isP _ = False

isF :: EncodedCnstrs -> Bool
isF (F _ _ _) = True
isF _ = False

isT :: EncodedCnstrs -> Bool
isT (T _ _ _ _) = True
isT _ = False

isQ :: EncodedCnstrs -> Bool
isQ (Q _ _ _ _ _) = True
isQ _ = False

isSA :: EncodedCnstrs -> Bool
isSA (SA _ _ _) = True
isSA _ = False

isSB :: EncodedCnstrs -> Bool
isSB (SB _ _ _) = True
isSB _ = False

isV :: EncodedCnstrs -> Bool
isV (V _ _ _) = True
isV _ = False

isW :: EncodedCnstrs -> Bool
isW (W _ _ _) = True
isW _ = False

isH :: EncodedCnstrs -> Bool
isH (H _ _ _ _) = True
isH _ = False

isR :: EncodedCnstrs -> Bool
isR (R _ _ _ _) = True
isR _ = False

isM :: EncodedCnstrs -> Bool
isM (M _ _ _ _) = True
isM _ = False

isN :: EncodedCnstrs -> Bool
isN (N _ _) = True
isN _ = False

isD :: EncodedCnstrs -> Bool
isD (D _ _ _ _) = True
isD _ = False

isI :: EncodedCnstrs -> Bool
isI (I _ _ _ _) = True
isI _ = False

isU :: EncodedCnstrs -> Bool
isU (U _ _ _ _ _ _) = True
isU _ = False

isG :: EncodedCnstrs -> Bool
isG (G _ _ _) = True
isG _ = False

-- | Works only with the correctly defined argument though it is not checked. Use with this caution.
getIEl :: EncodedCnstrs -> Int
getIEl E = -1
getIEl (P _ arr) = unsafeAt arr 0
getIEl (Q _ i _ _ _) = i
getIEl (T _ i _ _) = i
getIEl (SA _ i _) = i
getIEl (SB _ i _) = i
getIEl (F _ i _) = i
getIEl (V _ i _) = i
getIEl (W _ i _) = i
getIEl (H _ i _ _) = i
getIEl (R _ i _ _) = i
getIEl (M _ i _ _) = i
getIEl (N _ arr) = fst . unsafeAt arr $ 0
getIEl (D _ i _ _) = i
getIEl (I _ i _ _) = i
getIEl (U _ i _ _ _ _) = i
getIEl (G _ i _) = i

-- | Works only with the correctly defined arguments though it is not checked. Use with this caution.
setIEl :: Int -> EncodedCnstrs -> EncodedCnstrs
setIEl _ E = E
setIEl i (P n arr) = P n (arr // [(0, i)])
setIEl i (Q n _ j k l) = Q n i j k l
setIEl i (T n _ j k) = T n i j k
setIEl i (SA n _ v) = SA n i v
setIEl i (SB n _ v) = SB n i v
setIEl i (F n _ j) = F n i j
setIEl i (V n _ j) = V n i j
setIEl i (W n _ j) = W n i j
setIEl i (H n _ j k) = H n i j k
setIEl i (R n _ j k) = R n i j k
setIEl i (M n _ j k) = M n i j k
setIEl _ (N n arr) = N n arr
setIEl i (D n _ j k) = D n i j k
setIEl i (I n _ j k) = I n i j k
setIEl i (U n _ j k l m) = U n i j k l m
setIEl i (G n _ j) = G n i j

