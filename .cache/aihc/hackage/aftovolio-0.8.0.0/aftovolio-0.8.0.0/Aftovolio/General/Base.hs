{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields -fobject-code #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.General.Base
Copyright   :  (c) Oleksandr Zhabenko 2021-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

This is a computational scheme for generalized usage of the phonetic languages and AFTOVolio approach.
It is intended to be exported qualified, so that the functions in every language
implementation have the same names and signatures as these ones and the data type used here.
It is may be not the most efficient implementation.
-}
module Aftovolio.General.Base (
    -- * Phonetics representation data type for the phonetic languages approach.
    PhoneticElement (..),
    PhoneticsRepresentationPL (..),
    PhoneticsRepresentationPLX (..),
    Generations,
    InterGenerationsString,
    WritingSystemPRPLX,
    GWritingSystemPRPLX,
    PhoneticRepresentationXInter,
    IGWritingSystemPRPLX,
    fromX2PRPL,
    fromPhoneticRX,

    -- * Functions to work with the one.

    -- ** Predicates
    isPRC,
    isPRAfterC,
    isPRBeforeC,
    isPREmptyC,

    -- ** Convert to the 'PhoneticsRepresentationPLX'.
    stringToXSG,
    stringToXG,
    stringToXS,
    string2X,

    -- ** Apply conversion from 'PhoneticsRepresentationPLX'.
    rulesX,

    -- * Auxiliary functions
    fHelp4,
    findSA,
    findSAI,

    -- * Some class extensions for 'Eq' and 'Ord' type classes
    (~=),
    compareG,
) where

import Data.Char (isLetter)
import Data.Either
import Data.List (find, groupBy, intercalate, nub, partition, sortBy, words, (\\))
import Data.Maybe (fromJust, isJust)
import Data.Tuple (fst, snd)
import GHC.Arr
import GHC.Base
import GHC.Exts
import GHC.Int (Int8 (..))
import GHC.List
import GHC.Num ((-))
import Text.Show (Show (..))

{- | The syllable after this is encoded with the representation with every 'Char' being some phonetic language phenomenon.
To see its usual written representation, use the defined 'showRepr' function (please, implement your own one).
-}
data PhoneticsRepresentationPL
    = PR {string :: String, afterString :: String, beforeString :: String}
    | PRAfter {string :: String, afterString :: String}
    | PRBefore {string :: String, beforeString :: String}
    | PREmpty {string :: String}
    deriving (Eq, Ord)

instance Show PhoneticsRepresentationPL where
    show (PR xs ys zs) = intercalate " " ["R", show zs, show xs, show ys]
    show (PRAfter xs ys) = intercalate " " ["A", show xs, show ys]
    show (PRBefore xs zs) = intercalate " " ["B", show zs, show xs]
    show (PREmpty xs) = "E " `mappend` show xs

class PhoneticElement a where
    readPEMaybe :: String -> Maybe a

instance PhoneticElement PhoneticsRepresentationPL where
    readPEMaybe rs
        | not . any isLetter $ rs = Nothing
        | otherwise = case ys of
            "R" -> case yss of
                [zs, xs, ts] -> Just (PR xs ts zs)
                _ -> Nothing
            "A" -> case yss of
                [xs, ts] -> Just (PRAfter xs ts)
                _ -> Nothing
            "B" -> case yss of
                [zs, xs] -> Just (PRBefore xs zs)
                _ -> Nothing
            "E" -> case yss of
                [xs] -> Just (PREmpty xs)
                _ -> Nothing
            _ -> Nothing
      where
        (ys : yss) = words rs

{- | Extended variant of the 'PhoneticsRepresentationPL' data type where the information for the 'Char' is encoded into the
data itself. Is easier to implement the rules in the separate file by just specifying the proper and complete list of
'PhoneticsRepresentationPLX' values. While the 'char' function can be used to group 'PhoneticRepresentationPLX'
that represents some phenomenae, for the phonetic languages approach the 'string1' is used in the most cases.
-}
data PhoneticsRepresentationPLX
    = PRC
        { stringX :: String
        , afterStringX :: String
        , beforeStringX :: String
        , char :: Char
        , string1 :: String
        }
    | PRAfterC
        {stringX :: String, afterStringX :: String, char :: Char, string1 :: String}
    | PRBeforeC
        {stringX :: String, beforeStringX :: String, char :: Char, string1 :: String}
    | PREmptyC {stringX :: String, char :: Char, string1 :: String}
    deriving (Eq, Ord)

instance Show PhoneticsRepresentationPLX where
    show (PRC xs ys zs c us) = intercalate " " ["RC", show zs, show xs, show ys, '\'' : c : "\'", us]
    show (PRAfterC xs ys c us) = intercalate " " ["AC", show xs, show ys, '\'' : c : "\'", us]
    show (PRBeforeC xs zs c us) = intercalate " " ["BC", show zs, show xs, '\'' : c : "\'", us]
    show (PREmptyC xs c us) = "EC " `mappend` show xs `mappend` (' ' : '\'' : c : "\'") `mappend` us

instance PhoneticElement PhoneticsRepresentationPLX where
    readPEMaybe rs
        | not . any isLetter $ rs = Nothing
        | otherwise = case ys of
            "RC" -> case yss of
                [zs, xs, ts, cs, us] -> case cs of
                    '\'' : c : "\'" -> Just (PRC xs ts zs c us)
                    _ -> Nothing
                _ -> Nothing
            "AC" -> case yss of
                [xs, ts, cs, us] -> case cs of
                    '\'' : c : "\'" -> Just (PRAfterC xs ts c us)
                    _ -> Nothing
                _ -> Nothing
            "BC" -> case yss of
                [zs, xs, cs, us] -> case cs of
                    '\'' : c : "\'" -> Just (PRBeforeC xs zs c us)
                    _ -> Nothing
                _ -> Nothing
            "EC" -> case yss of
                [xs, cs, us] -> case cs of
                    '\'' : c : "\'" -> Just (PREmptyC xs c us)
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
      where
        (ys : yss) = words rs

isPRC :: PhoneticsRepresentationPLX -> Bool
isPRC (PRC _ _ _ _ _) = True
isPRC _ = False

isPRAfterC :: PhoneticsRepresentationPLX -> Bool
isPRAfterC (PRAfterC _ _ _ _) = True
isPRAfterC _ = False

isPRBeforeC :: PhoneticsRepresentationPLX -> Bool
isPRBeforeC (PRBeforeC _ _ _ _) = True
isPRBeforeC _ = False

isPREmptyC :: PhoneticsRepresentationPLX -> Bool
isPREmptyC (PREmptyC _ _ _) = True
isPREmptyC _ = False

fromX2PRPL :: PhoneticsRepresentationPLX -> PhoneticsRepresentationPL
fromX2PRPL (PREmptyC xs _ _) = PREmpty xs
fromX2PRPL (PRAfterC xs ys _ _) = PRAfter xs ys
fromX2PRPL (PRBeforeC xs zs _ _) = PRBefore xs zs
fromX2PRPL (PRC xs ys zs _ _) = PR xs ys zs
{-# INLINE fromX2PRPL #-}

-- | An analogue of the 'rulesPR' function for 'PhoneticsRepresentationPLX'.
rulesX :: PhoneticsRepresentationPLX -> Char
rulesX = char
{-# INLINE rulesX #-}

stringToXS :: WritingSystemPRPLX -> String -> [String]
stringToXS xs ys = ks : stringToX' zss l ts
  where
    !zss = nub . map stringX $ xs
    !l = maximum . map length $ zss
    f ys l zss =
        splitAt
            ( (\xs -> if null xs then 1 else head xs) . filter (\n -> elem (take n ys) zss) $
                [l, l - 1 .. 1]
            )
            ys
    {-# INLINE f #-}
    (!ks, !ts) = f ys l zss
    stringToX' rss m vs = bs : stringToX' rss m us
      where
        (!bs, !us) = f vs m rss

{- | Uses the simplest variant of the 'GWritingSystemPRPLX' with just two generations where all the 'PREmptyC' elements in the
'WritingSystemPRPLX' are used in the last order. Can be suitable for simple languages (e. g. Esperanto).
-}
string2X :: WritingSystemPRPLX -> String -> [PhoneticsRepresentationPLX]
string2X xs = stringToXG [(zs, 1), (ys, 0)]
  where
    (ys, zs) = partition isPREmptyC xs
{-# INLINE string2X #-}

{- | Each generation represents a subset of rules for representation transformation. The 'PhoneticsRepresentationPLX'
are groupped by the generations so that in every group with the same generation number ('Int8' value, typically starting
from 1) the rules represented have no conflicts with each other (this guarantees that they can be applied simultaneously
without the danger of incorrect interference). Usage of 'Generations' is a design decision and is inspired by the
GHC RULES pragma and the GHC compilation multistage process.
-}
type Generations = Int8

-- | Each value represents temporary intermediate resulting 'String' data to be transformed further into the representation.
type InterGenerationsString = String

{- | If the list here is proper and complete, then it usually represents the whole writing system of the language. For proper usage,
the list must be sorted in the ascending order.
-}
type WritingSystemPRPLX = [PhoneticsRepresentationPLX]

{- | The \'dynamic\' representation of the general writing system that specifies what transformations are made simultaneously
during the conversion to the phonetic languages phonetics representation. During transformations those elements that have
greater 'Generations' are used earlier than others. The last ones are used those elements with the 'Generations' element
equal to 0 that must correspond to the 'PREmptyC' constructor-built records. For proper usage, the lists on the first
place of the tuples must be sorted in the ascending order.
-}
type GWritingSystemPRPLX = [([PhoneticsRepresentationPLX], Generations)]

-- | The intermediate representation of the phonetic languages data. Is used during conversions.
type PhoneticRepresentationXInter =
    Either PhoneticsRepresentationPLX InterGenerationsString

fromPhoneticRX ::
    [PhoneticsRepresentationPLX] ->
    [PhoneticRepresentationXInter] ->
    [PhoneticsRepresentationPLX]
fromPhoneticRX ts = concatMap (fromInter2X ts)
  where
    fromInter2X ::
        [PhoneticsRepresentationPLX] ->
        PhoneticRepresentationXInter ->
        [PhoneticsRepresentationPLX]
    fromInter2X _ (Left x) = [x]
    fromInter2X ys (Right z) = filter ((== z) . stringX) ys

{- | The \'dynamic\' representation of the process of transformation for the general writing system during the conversion.
Is not intended to be produced by hand, but automatically by programs.
-}
type IGWritingSystemPRPLX = [(PhoneticRepresentationXInter, Generations)]

{- | Splits the given list using 4 predicates into tuple of 4 lists of elements satisfying the predicates in the order
being preserved.
-}
fHelp4 ::
    (a -> Bool) ->
    (a -> Bool) ->
    (a -> Bool) ->
    (a -> Bool) ->
    [a] ->
    ([a], [a], [a], [a])
fHelp4 p1 p2 p3 p4 = foldr g v
  where
    v = ([], [], [], [])
    g x (xs1, xs2, xs3, xs4)
        | p1 x = (x : xs1, xs2, xs3, xs4)
        | p2 x = (xs1, x : xs2, xs3, xs4)
        | p3 x = (xs1, xs2, x : xs3, xs4)
        | p4 x = (xs1, xs2, xs3, x : xs4)
        | otherwise = (xs1, xs2, xs3, xs4)
{-# INLINE fHelp4 #-}

{- | Partial equivalence that is used to find the appropriate 'PhoneticsRepresentationPL' for the class of
'PhoneticsRepresentationPLX' values.
-}
(~=) :: PhoneticsRepresentationPL -> PhoneticsRepresentationPLX -> Bool
(PR xs ys zs) ~= (PRC xs1 ys1 zs1 _ _) = xs == xs1 && ys == ys1 && zs == zs1
(PRAfter xs ys) ~= (PRAfterC xs1 ys1 _ _) = xs == xs1 && ys == ys1
(PRBefore ys zs) ~= (PRBeforeC ys1 zs1 _ _) = ys == ys1 && zs == zs1
(PREmpty xs) ~= (PREmptyC xs1 _ _) = xs1 == xs1
_ ~= _ = False

{- | Partial equivalence that is used to find the appropriate 'PhoneticsRepresentationPL' for the class of
'PhoneticsRepresentationPLX' values.
-}
compareG :: PhoneticsRepresentationPL -> PhoneticsRepresentationPLX -> Ordering
compareG (PR xs ys zs) (PRC xs1 ys1 zs1 _ _)
    | xs /= xs1 = compare xs xs1
    | ys /= ys1 = compare ys ys1
    | zs /= zs1 = compare zs zs1
    | otherwise = EQ
compareG (PR _ _ _) _ = LT
compareG (PREmpty xs) (PREmptyC xs1 _ _)
    | xs /= xs1 = compare xs xs1
    | otherwise = EQ
compareG (PREmpty _) _ = GT
compareG (PRAfter xs ys) (PRAfterC xs1 ys1 _ _)
    | xs /= xs1 = compare xs xs1
    | ys /= ys1 = compare ys ys1
    | otherwise = EQ
compareG (PRAfter _ _) (PRC _ _ _ _ _) = GT
compareG (PRAfter _ _) _ = LT
compareG (PRBefore ys zs) (PRBeforeC ys1 zs1 _ _)
    | ys /= ys1 = compare ys ys1
    | zs /= zs1 = compare zs zs1
    | otherwise = EQ
compareG (PRBefore _ _) (PREmptyC _ _ _) = LT
compareG (PRBefore _ _) _ = GT

-- | Is somewhat rewritten from the 'CaseBi.Arr.gBF3' function (not exported) from the @mmsyn2-array@ package.
gBF3 ::
    (Ix i) =>
    (# Int#, PhoneticsRepresentationPLX #) ->
    (# Int#, PhoneticsRepresentationPLX #) ->
    PhoneticsRepresentationPL ->
    Array i PhoneticsRepresentationPLX ->
    Maybe PhoneticsRepresentationPLX
gBF3 (# !i#, k #) (# !j#, m #) repr arr
    | isTrue# ((j# -# i#) ># 1#) =
        case compareG repr p of
            GT -> gBF3 (# n#, p #) (# j#, m #) repr arr
            LT -> gBF3 (# i#, k #) (# n#, p #) repr arr
            _ -> Just p
    | repr ~= m = Just m
    | repr ~= k = Just k
    | otherwise = Nothing
  where
    !n# = (i# +# j#) `quotInt#` 2#
    !p = unsafeAt arr (I# n#)
{-# INLINEABLE gBF3 #-}

findSA ::
    PhoneticsRepresentationPL ->
    Array Int PhoneticsRepresentationPLX ->
    Maybe PhoneticsRepresentationPLX
findSA repr arr = gBF3 (# i#, k #) (# j#, m #) repr arr
  where
    !(I# i#, I# j#) = bounds arr
    !k = unsafeAt arr (I# i#)
    !m = unsafeAt arr (I# i#)

{- | Finds and element in the 'Array' that the corresponding 'PhoneticsRepresentationPLX' from the first argument is '~=' to the
it. The 'String' arguments inside the tuple pair are the 'beforeString' and the 'afterString' elements of it to be used in 'Right'
case.
-}
findSAI ::
    PhoneticRepresentationXInter ->
    (String, String) ->
    Array Int PhoneticsRepresentationPLX ->
    Maybe PhoneticsRepresentationPLX
findSAI repr (xs, ys) arr
    | isLeft repr =
        gBF3
            (# i#, k #)
            (# j#, m #)
            (fromX2PRPL . fromLeft (PREmptyC " " ' ' " ") $ repr)
            arr
    | otherwise =
        gBF3 (# i#, k #) (# j#, m #) (str2PRPL (fromRight [] repr) (xs, ys)) arr
  where
    !(I# i#, I# j#) = bounds arr
    !k = unsafeAt arr (I# i#)
    !m = unsafeAt arr (I# i#)
    str2PRPL :: String -> (String, String) -> PhoneticsRepresentationPL
    str2PRPL ts ([], []) = PREmpty ts
    str2PRPL ts (ys, []) = PRBefore ts ys
    str2PRPL ts ([], zs) = PRAfter ts zs
    str2PRPL ts (ys, zs) = PR ts zs ys

stringToXSG ::
    GWritingSystemPRPLX -> Generations -> String -> IGWritingSystemPRPLX
stringToXSG xs n ys
    | any ((== n) . snd) xs && n > 0 =
        stringToXSGI (xs \\ ts) (n - 1) . xsG zs n $ pss
    | otherwise =
        error
            "Aftovolio.General.Base.stringToXSG: Not defined for these first two arguments. "
  where
    !pss = stringToXS (concatMap fst xs) ys -- ps :: [String]
    !ts = filter ((== n) . snd) $ xs -- ts :: GWritingSystemPRPLX
    !zs = if null ts then [] else fst . head $ ts -- zs :: PhoneticRepresentationX
    xsG1 rs n (k1s : k2s : k3s : kss) (!r2s, !r3s, !r4s, !r5s) -- xsG1 :: [PhoneticRepresentationPLX] -> [String] -> Generations -> IGWritingSystemPRPLX
        | isJust x1 =
            (Right k1s, n - 1) : (Left . fromJust $ x1, n) : xsG1 rs n (k3s : kss) (r2s, r3s, r4s, r5s)
        | isJust x2 = (Left . fromJust $ x2, n) : xsG1 rs n (k2s : k3s : kss) (r2s, r3s, r4s, r5s)
        | isJust x3 =
            (Right k1s, n - 1) : (Left . fromJust $ x3, n) : xsG1 rs n (k3s : kss) (r2s, r3s, r4s, r5s)
        | isJust x4 = (Left . fromJust $ x4, n) : xsG1 rs n (k2s : k3s : kss) (r2s, r3s, r4s, r5s)
        | otherwise = (Right k1s, n - 1) : xsG1 rs n (k2s : k3s : kss) (r2s, r3s, r4s, r5s)
      where
        !x1 = findSA (PR k2s k3s k1s) r2s
        !x2 = findSA (PRAfter k1s k2s) r3s
        !x3 = findSA (PRBefore k2s k1s) r4s
        !x4 = findSA (PREmpty k1s) r5s
    xsG1 rs n (k1s : k2s : kss) (!r2s, !r3s, !r4s, !r5s)
        | isJust x2 = (Left . fromJust $ x2, n) : xsG1 rs n (k2s : kss) (r2s, r3s, r4s, r5s)
        | isJust x3 =
            (Right k1s, n - 1) : (Left . fromJust $ x3, n) : xsG1 rs n kss (r2s, r3s, r4s, r5s)
        | isJust x4 = (Left . fromJust $ x4, n) : xsG1 rs n (k2s : kss) (r2s, r3s, r4s, r5s)
        | otherwise = (Right k1s, n - 1) : xsG1 rs n (k2s : kss) (r2s, r3s, r4s, r5s)
      where
        !x2 = findSA (PRAfter k1s k2s) r3s
        !x3 = findSA (PRBefore k2s k1s) r4s
        !x4 = findSA (PREmpty k1s) r5s
    xsG1 rs n [k1s] (_, _, _, r5s)
        | isJust x4 = [(Left . fromJust $ x4, n)]
        | otherwise = [(Right k1s, n - 1)]
      where
        !x4 = findSA (PREmpty k1s) r5s
    xsG1 rs n [] (_, _, _, _) = []
    xsG rs n jss = xsG1 rs n jss (r2s, r3s, r4s, r5s)
      where
        (!r2ls, !r3ls, !r4ls, !r5ls) = fHelp4 isPRC isPRAfterC isPRBeforeC isPREmptyC rs
        !r2s = listArray (0, length r2ls - 1) r2ls
        !r3s = listArray (0, length r3ls - 1) r3ls
        !r4s = listArray (0, length r4ls - 1) r4ls
        !r5s = listArray (0, length r5ls - 1) r5ls

-- | Is used internally in the 'stringToXSG' and 'stringToXG' functions respectively.
stringToXSGI ::
    GWritingSystemPRPLX ->
    Generations ->
    IGWritingSystemPRPLX ->
    IGWritingSystemPRPLX
stringToXSGI xs n ys
    | n > 0 = stringToXSGI (xs \\ ts) (n - 1) . xsGI zs n $ ys
    | otherwise = ys
  where
    !ts = filter ((== n) . snd) xs -- ts :: GWritingSystemPRPLX
    !zs = concatMap fst ts -- zs :: PhoneticRepresentationX
    xsGI1 rs n (k1s : k2s : k3s : kss) (r2s, r3s, r4s, r5s) -- xsGI1 :: [PhoneticRepresentationPLX] -> Generations -> IGWritingSystemPRPLX -> IGWritingSystemPRPLX
        | snd k2s == n && isJust x1 =
            (fst k1s, n - 1)
                : (Left . fromJust $ x1, n)
                : xsGI1 rs n (k3s : kss) (r2s, r3s, r4s, r5s)
        | snd k1s == n && isJust x2 =
            (Left . fromJust $ x2, n) : xsGI1 rs n (k2s : k3s : kss) (r2s, r3s, r4s, r5s)
        | snd k2s == n && isJust x3 =
            (fst k1s, n - 1) : (Left . fromJust $ x3, n) : xsGI1 rs n (k3s : kss) (r2s, r3s, r4s, r5s)
        | snd k1s == n && isJust x4 =
            (Left . fromJust $ x4, n) : xsGI1 rs n (k2s : k3s : kss) (r2s, r3s, r4s, r5s)
        | otherwise = (fst k1s, n - 1) : xsGI1 rs n (k2s : k3s : kss) (r2s, r3s, r4s, r5s)
      where
        !x1 =
            findSAI
                (fst k2s)
                (either stringX id . fst $ k1s, either stringX id . fst $ k3s)
                r2s
        !x2 = findSAI (fst k1s) ([], either stringX id . fst $ k2s) r3s
        !x3 = findSAI (fst k2s) (either stringX id . fst $ k1s, []) r4s
        !x4 = findSAI (fst k1s) ([], []) r5s
    xsGI1 rs n (k1s : k2s : kss) (r2s, r3s, r4s, r5s)
        | snd k1s == n && isJust x2 =
            (Left . fromJust $ x2, n) : xsGI1 rs n (k2s : kss) (r2s, r3s, r4s, r5s)
        | snd k2s == n && isJust x3 =
            (fst k1s, n - 1) : (Left . fromJust $ x3, n) : xsGI1 rs n kss (r2s, r3s, r4s, r5s)
        | snd k1s == n && isJust x4 =
            (Left . fromJust $ x4, n) : xsGI1 rs n (k2s : kss) (r2s, r3s, r4s, r5s)
        | otherwise = (fst k1s, n - 1) : xsGI1 rs n (k2s : kss) (r2s, r3s, r4s, r5s)
      where
        !x2 = findSAI (fst k1s) ([], either stringX id . fst $ k2s) r3s
        !x3 = findSAI (fst k2s) (either stringX id . fst $ k1s, []) r4s
        !x4 = findSAI (fst k1s) ([], []) r5s
    xsGI1 rs n [k1s] (_, _, _, r5s)
        | snd k1s == n && isJust x4 = [(Left . fromJust $ x4, n)]
        | otherwise = [(fst k1s, n - 1)]
      where
        !x4 = findSAI (fst k1s) ([], []) r5s
    xsGI1 rs n [] (_, _, _, _) = []
    xsGI rs n jss = xsGI1 rs n jss (r2s, r3s, r4s, r5s)
      where
        (!r2ls, !r3ls, !r4ls, !r5ls) = fHelp4 isPRC isPRAfterC isPRBeforeC isPREmptyC rs
        !r2s = listArray (0, length r2ls - 1) r2ls
        !r3s = listArray (0, length r3ls - 1) r3ls
        !r4s = listArray (0, length r4ls - 1) r4ls
        !r5s = listArray (0, length r5ls - 1) r5ls

-- | The full conversion function. Applies conversion into representation using the 'GWritingSystemPRPLX' provided.
stringToXG :: GWritingSystemPRPLX -> String -> [PhoneticsRepresentationPLX]
stringToXG xs ys = fromPhoneticRX ts . map fst . stringToXSG xs n $ ys
  where
    n = maximum . map snd $ xs
    !ts = concatMap fst . filter ((== 0) . snd) $ xs
