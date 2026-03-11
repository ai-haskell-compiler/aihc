{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell utilities for generating double words declarations
module Data.DoubleWord.TH
  ( mkDoubleWord
  , mkUnpackedDoubleWord
  ) where

import GHC.Arr (Ix(..))
import Data.Ratio ((%))
import Data.Bits (Bits(..))
#if MIN_VERSION_base(4,7,0)
import Data.Bits (FiniteBits(..))
#endif
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
#if MIN_VERSION_hashable(1,2,0)
import Data.Hashable (Hashable(..), hashWithSalt)
#else
import Data.Hashable (Hashable(..), combine)
#endif
#if !MIN_VERSION_base(4,12,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Language.Haskell.TH hiding (unpacked, match, conP)
import Data.BinaryWord (BinaryWord(..))
import Data.DoubleWord.Base

tup ∷ [Exp] → Exp
#if MIN_VERSION_template_haskell(2,16,0)
tup = TupE . fmap Just
#else
tup = TupE
#endif

-- | Declare signed and unsigned binary word types built from
--   the specified low and high halves. The high halves /must/ have
--   less or equal bit-length than the lover half. For each data type
--   the following instances are declared: 'DoubleWord', 'Eq', 'Ord',
--   'Bounded', 'Enum', 'Num', 'Real', 'Integral', 'Show', 'Read',
--   'Hashable', 'Ix', 'Bits', 'BinaryWord'.
mkDoubleWord ∷ String -- ^ Unsigned variant type name
             → String -- ^ Unsigned variant constructor name
#if MIN_VERSION_template_haskell(2,11,0)
             → Bang   -- ^ Unsigned variant higher half strictness
#else
             → Strict -- ^ Unsigned variant higher half strictness
#endif
             → Name   -- ^ Unsigned variant higher half type
             → String -- ^ Signed variant type name
             → String -- ^ Signed variant constructor name
#if MIN_VERSION_template_haskell(2,11,0)
             → Bang   -- ^ Signed variant higher half strictness
#else
             → Strict -- ^ Signed variant higher half strictness
#endif
             → Name   -- ^ Signed variant higher half type
#if MIN_VERSION_template_haskell(2,11,0)
             → Bang   -- ^ Lower half strictness
#else
             → Strict -- ^ Lower half strictness
#endif
             → Name   -- ^ Lower half type
             → [Name] -- ^ List of instances for automatic derivation
             → Q [Dec]
mkDoubleWord un uc uhs uhn sn sc shs shn ls ln ad =
    (++) <$> mkDoubleWord' False un' uc' sn' sc' uhs (ConT uhn) ls lt ad
         <*> mkDoubleWord' True  sn' sc' un' uc' shs (ConT shn) ls lt ad
  where un' = mkName un
        uc' = mkName uc
        sn' = mkName sn
        sc' = mkName sc
        lt  = ConT ln

-- | @'mkUnpackedDoubleWord' u uh s sh l@ is an alias for
--   @'mkDoubleWord' u u 'Unpacked' uh s s 'Unpacked' sh 'Unpacked' l@
mkUnpackedDoubleWord ∷ String -- ^ Unsigned variant type name
                     → Name   -- ^ Unsigned variant higher half type
                     → String -- ^ Signed variant type name
                     → Name   -- ^ Signed variant higher half type
                     → Name   -- ^ Lower half type
                     → [Name] -- ^ List of instances for automatic derivation
                     → Q [Dec]
mkUnpackedDoubleWord un uhn sn shn ln ad =
    mkDoubleWord un un unpacked uhn sn sn unpacked shn unpacked ln ad
  where unpacked =
#if MIN_VERSION_template_haskell(2,11,0)
                   Bang SourceUnpack SourceStrict
#else
                   Unpacked
#endif

mkDoubleWord' ∷ Bool
              → Name → Name
              → Name → Name
#if MIN_VERSION_template_haskell(2,11,0)
              → Bang
#else
              → Strict
#endif
              → Type
#if MIN_VERSION_template_haskell(2,11,0)
              → Bang
#else
              → Strict
#endif
              → Type
              → [Name]
              → Q [Dec]
mkDoubleWord' signed tp cn otp ocn hiS hiT loS loT ad = (<$> mkRules) $ (++) $
    [ DataD [] tp []
#if MIN_VERSION_template_haskell(2,11,0)
            Nothing
#endif
            [NormalC cn [(hiS, hiT), (loS, loT)]]
#if MIN_VERSION_template_haskell(2,12,0)
            [DerivClause Nothing (map ConT ad)]
#elif MIN_VERSION_template_haskell(2,11,0)
            (ConT <$> ad)
#else
            ad
#endif
    , inst ''DoubleWord [tp]
        [ tySynInst ''LoWord [tpT] loT
        , tySynInst ''HiWord [tpT] hiT
        , funLo 'loWord (VarE lo)
        , inline 'loWord
        , funHi 'hiWord (VarE hi)
        , inline 'hiWord
        , fun 'fromHiAndLo (ConE cn)
        , inline 'fromHiAndLo
        {- extendLo x = W allZeroes x -}
        , funX 'extendLo $ appWN ['allZeroes, x]
        , inline 'extendLo
        {-
          signExtendLo x = W (if x < 0 then allOnes else allZeroes)
                             (unsignedWord x)
        -}
        , funX 'signExtendLo $
            appW [ CondE (appVN 'testMsb [x])
                         (VarE 'allOnes) (VarE 'allZeroes)
                 , appVN 'unsignedWord [x] ]
        , inlinable 'signExtendLo
        ]
    , inst ''Eq [tp] $
        {- (W hi lo) == (W hi' lo') = hi == hi' && lo == lo' -}
        [ funHiLo2 '(==) $
            appV '(&&) [appVN '(==) [hi, hi'], appVN '(==) [lo, lo']]
        , inline '(==) ]
    , inst ''Ord [tp]
        {-
          compare (W hi lo) (W hi' lo') = case hi `compare` hi' of
            EQ → lo `compare` lo'
            x  → x
        -}
        [ funHiLo2 'compare $
            CaseE (appVN 'compare [hi, hi'])
              [ Match (conP 'EQ []) (NormalB (appVN 'compare [lo, lo'])) []
              , Match (VarP x) (NormalB (VarE x)) [] ]
        , inlinable 'compare ]
    , inst ''Bounded [tp]
        {- minBound = W minBound minBound -}
        [ fun 'minBound $ appWN ['minBound, 'minBound]
        , inline 'minBound
        {- maxBound = W maxBound maxBound -}
        , fun 'maxBound $ appWN ['maxBound, 'maxBound]
        , inline 'maxBound ]
    , inst ''Enum [tp]
        {-
          succ (W hi lo) = if lo == maxBound then W (succ hi) minBound
                                             else W hi (succ lo)
        -}
        [ funHiLo 'succ $ CondE (appVN '(==) [lo, 'maxBound])
                                (appW [appVN 'succ [hi], VarE 'minBound])
                                (appW [VarE hi, appVN 'succ [lo]])
        , inlinable 'succ
        {-
          pred (W hi lo) = if lo == minBound then W (pred hi) maxBound
                                             else W hi (pred lo)
        -}
        , funHiLo 'pred $ CondE (appVN '(==) [lo, 'minBound])
                                (appW [appVN 'pred [hi], VarE 'maxBound])
                                (appW [VarE hi, appVN 'pred [lo]])
        , inlinable 'pred
        {-
          toEnum x
            | x < 0     = if signed
                          then W (-1) (negate $ 1 + toEnum (negate (x + 1)))
                          else ERROR
            | otherwise = W 0 (toEnum x)
        -}
        , funX 'toEnum $
            CondE (appV '(<) [VarE x, litI 0])
                  (if signed
                   then appW [ VarE 'allOnes
                             , appV 'negate
                                 [ appV '(+)
                                     [ oneE
                                     , appV 'toEnum
                                         [ appV 'negate
                                             [appV '(+) [VarE x, litI 1]] ]
                                     ]
                                 ]
                             ]
                   else appV 'error [litS "toEnum: nagative value"])
                  (appW [VarE 'allZeroes, appVN 'toEnum [x]])
        {-
          fromEnum (W 0 lo)    = fromEnum lo
          fromEnum (W (-1) lo) = if signed then negate $ fromEnum $ negate lo
                                           else ERROR
          fromEnum _           = ERROR
        -}
        , FunD 'fromEnum $
            Clause [conP cn [LitP $ IntegerL 0, VarP lo]]
                   (NormalB $ appVN 'fromEnum [lo]) [] :
            if signed
            then [ Clause [conP cn [LitP $ IntegerL (-1), VarP lo]]
                          (NormalB $
                             appV 'negate
                               [appV 'fromEnum [appV 'negate [VarE lo]]])
                          []
                 , Clause [WildP]
                          (NormalB $
                             appV 'error [litS "fromEnum: out of bounds"])
                          []
                 ]
            else [ Clause [WildP]
                          (NormalB $
                             appV 'error [litS "fromEnum: out of bounds"])
                          [] ]
        {- enumFrom x = enumFromTo x maxBound -}
        , funX 'enumFrom $ appVN 'enumFromTo [x, 'maxBound]
        , inline 'enumFrom
        {-
          enumFromThen x y =
            enumFromThenTo x y $ if y >= x then maxBound else minBound
        -}
        , funXY 'enumFromThen $
            appV 'enumFromThenTo
              [ VarE x
              , VarE y
              , CondE (appVN '(>=) [y, x]) (VarE 'maxBound) (VarE 'minBound)
              ]
        , inlinable 'enumFromThen
        {-
          enumFromTo x y = case y `compare` x of
              LT → []
              EQ → [x]
              GT → x : up y x
            where up to c = next : if next == to then [] else up to next
                    where next = c + 1
        -}
        , FunD 'enumFromTo $ return $
            Clause
              [VarP x, VarP y]
              (NormalB $
                 CaseE (appVN 'compare [y, x])
                   [ match (conP 'LT []) (ConE '[])
                   , match (conP 'EQ []) (singE $ VarE x)
                   , match (conP 'GT []) $ appC '(:) [VarE x, appVN up [y, x]]
                   ])
              [ FunD up $ return $
                  Clause [VarP to, VarP c]
                    (NormalB $
                       appC '(:)
                         [ VarE next
                         , CondE (appVN '(==) [next, to])
                                 (ConE '[]) (appVN up [to, next])
                         ])
                    [val next $ appVN '(+) [c, 'lsb]]
              ]
        {-
          enumFromThenTo x y z = case y `compare` x of
              LT → if z > y then (if z > x then [] else [x])
                            else x : down step (z + step) y
                where step = x - y
                      to = z + step
                      down c | c < to    = [c]
                             | otherwise = c : down (c - step)
              EQ → if z < x then [] else repeat x
              GT → if z < y then (if z < x then [] else [x])
                            else x : up step (z - step) y
                where step = y - x
                      to = z - step
                      up c | c > to    = [c]
                           | otherwise = c : up (c + step)
        -}
        , FunD 'enumFromThenTo $ return $
            Clause [VarP x, VarP y, VarP z]
              (NormalB $
                CaseE (appVN 'compare [y, x])
                  [ match'
                      (conP 'LT [])
                      (CondE (appVN '(>) [z, y])
                             (CondE (appVN '(>) [z, x])
                                    (ConE '[]) (singE $ VarE x))
                             (appC '(:) [VarE x, appVN down [y]]))
                      [ val step $ appVN '(-) [x, y]
                      , val to $ appVN '(+) [z, step]
                      , fun1 down c $
                          CondE (appVN '(<) [c, to])
                                (singE $ VarE c)
                                (appC '(:)
                                      [ VarE c
                                      , appV down [appVN '(-) [c, step]]
                                      ])
                      ]
                  , match
                      (conP 'EQ [])
                      (CondE (appVN '(<) [z, x])
                             (ConE '[]) (appVN 'repeat [x]))
                  , match'
                      (conP 'GT [])
                      (CondE (appVN '(<) [z, y])
                             (CondE (appVN '(<) [z, x])
                                    (ConE '[]) (singE $ VarE x))
                             (appC '(:) [VarE x, appVN up [y]]))
                      [ val step $ appVN '(-) [y, x]
                      , val to $ appVN '(-) [z, step]
                      , fun1 up c $
                          CondE (appVN '(>) [c, to])
                                (singE $ VarE c)
                                (appC '(:)
                                      [ VarE c
                                      , appV up [appVN '(+) [c, step]]
                                      ])
                      ]
                  ])
              []
        ]
    , inst ''Num [tp]
        {-
          negate (W hi lo) = if lo == 0 then W (negate hi) 0
                                        else W (negate $ hi + 1) (negate lo)
        -}
        [ funHiLo 'negate $
            CondE (appVN '(==) [lo, 'allZeroes])
                  (appW [appVN 'negate [hi], zeroE])
                  (appW [ appV 'negate [appVN '(+) ['lsb, hi]]
                        , appVN 'negate [lo] ])
        , inlinable 'negate
        {-
          abs x = if SIGNED
                  then if x < 0 then negate x else x
                  else x
        -}
        , funX 'abs $
            if signed
            then CondE (appVN '(<) [x, 'allZeroes])
                       (appVN 'negate [x]) (VarE x)
            else VarE x
        , if signed then inlinable 'abs else inline 'abs
        {-
          signum (W hi lo) = if SIGNED
                             then case hi `compare` 0 of
                               LT → W (-1) maxBound
                               EQ → if lo == 0 then 0 else 1
                               GT → W 0 1
                             else if hi == 0 && lo == 0 then 0 else 1
        -}
        , funHiLo 'signum $
            if signed
            then CaseE (appVN 'compare [hi, 'allZeroes])
                   [ Match (conP 'LT [])
                           (NormalB $ appWN ['allOnes, 'maxBound]) []
                   , Match (conP 'EQ [])
                           (NormalB $ CondE (appVN '(==) [lo, 'allZeroes])
                                            zeroE oneE)
                           []
                   , Match (conP 'GT []) (NormalB oneE) []
                   ]
            else CondE (appV '(&&) [ appVN '(==) [hi, 'allZeroes]
                                   , appVN '(==) [lo, 'allZeroes] ])
                       zeroE oneE
        , inlinable 'signum
        {-
          (W hi lo) + (W hi' lo') = W y x
            where x = lo + lo'
                  y = hi + hi' + if x < lo then 1 else 0
        -}
        , funHiLo2' '(+) (appWN [y, x])
            [ val x $ appVN '(+) [lo, lo']
            , val y $ appV '(+)
                        [ appVN '(+) [hi, hi']
                        , CondE (appVN '(<) [x, lo]) oneE zeroE ]
            ]
        , inlinable '(+)
        {-
          UNSIGNED:
            (W hi lo) * (W hi' lo') =
                W (hi * fromIntegral lo' + hi' * fromIntegral lo +
                   fromIntegral x) y
              where (x, y) = unwrappedMul lo lo'

          SIGNED:
            x * y = signedWord $ unsignedWord x * unsignedWord y
        -}
        , if signed
          then
            funXY '(*) $
              appV 'signedWord
                   [appV '(*) [ appVN 'unsignedWord [x]
                              , appVN 'unsignedWord [y] ]]
          else
            funHiLo2' '(*)
              (appW [ appV '(+)
                        [ appV '(+)
                            [ appV '(*) [VarE hi, appVN 'fromIntegral [lo']]
                            , appV '(*) [VarE hi', appVN 'fromIntegral [lo]] ]
                        , appVN 'fromIntegral [x] ]
                    , VarE y ])
              [vals [x, y] (appVN 'unwrappedMul [lo, lo'])]
        , inlinable '(*)
        {-
          fromInteger x = W (fromInteger y) (fromInteger z)
            where (y, z) = x `divMod` (toInteger (maxBound ∷ L) + 1)
        -}
        , funX' 'fromInteger
            (appW [appVN 'fromInteger [y], appVN 'fromInteger [z]])
            [vals [y, z]
               (appV 'divMod
                  [ VarE x
                  , appV '(+)
                      [appV 'toInteger [SigE (VarE 'maxBound) loT], litI 1]
                  ])]
        ]
    , inst ''Real [tp]
        {- toRational x = toInteger x % 1 -}
        [ funX 'toRational $ appV '(%) [appVN 'toInteger [x], litI 1]
        , inline 'toRational ]
    , inst ''Integral [tp] $
        {-
          toInteger (W hi lo) =
            toInteger hi * (toInteger (maxBound ∷ L) + 1) + toInteger lo
        -}
        [ funHiLo 'toInteger $
            appV '(+)
              [ appV '(*)
                  [ appVN 'toInteger [hi]
                  , appV '(+)
                      [appV 'toInteger [SigE (VarE 'maxBound) loT], litI 1] ]
              , appVN 'toInteger [lo] ]
        {-
          UNSIGNED:
            quotRem x@(W hi lo) y@(W hi' lo') =
                if hi' == 0 && lo' == 0
                then error "divide by zero"
                else case compare hi hi' of
                  LT → (0, x)
                  EQ → compare lo lo' of
                    LT → (0, x)
                    EQ → (1, 0)
                    GT | hi' == 0 → (W 0 t2, W 0 t1)
                      where (t2, t1) = quotRem lo lo'
                    GT → (1, lo - lo')
                  GT | lo' == 0 → (W 0 (fromIntegral t2),
                                   W (fromIntegral t1) lo)
                    where (t2, t1) = quotRem hi hi'
                  GT | hi' == 0 && lo' == maxBound →
                      if t2 == 0
                      then if t1 == maxBound
                           then (W 0 z + 1, 0)
                           else (W 0 z, t1)
                      else if t1 == maxBound
                           then (W 0 z + 2, 1)
                           else if t1 == xor maxBound 1
                                then (W 0 z + 2, 0)
                                else (W 0 z + 1, W 0 (t1 + 1))
                    where z = fromIntegral hi
                          (t2, t1) = unwrappedAdd z lo
                  GT | hi' == 0 → (t2, W 0 t1)
                    where (t2, t1) = div1 hi lo lo'
                  GT → if t1 == t2
                       then (1, x - y)
                       else (W 0 (fromIntegral q2), shiftR r2 t2)
                    where t1 = leadingZeroes hi
                          t2 = leadingZeroes hi'
                          z = shiftR hi (bitSize (undefined ∷ H) - t2)
                          W hhh hll = shiftL x t2
                          v@(W lhh lll) = shiftL y t2
                          -- z hhh hll / lhh lll
                          ((0, q1), r1) = div2 z hhh lhh
                          (t4, t3) = unwrappedMul (fromIntegral q1) lll
                          t5 = W (fromIntegral t4) t3
                          t6 = W r1 hll
                          (t8, t7) = unwrappedAdd t6 v
                          (t10, t9) = unwrappedAdd t7 v
                          (q2, r2) =
                            if t5 > t6
                            then
                              if loWord t8 == 0
                              then
                                if t7 >= t5
                                then (q1 - 1, t7 - t5)
                                else
                                  if loWord t10 == 0
                                  then (q1 - 2, t9 - t5)
                                  else (q1 - 2, (maxBound - t5) + t9 + 1)
                              else
                                (q1 - 1, (maxBound - t5) + t7 + 1)
                            else
                              (q1, t6 - t5)
            where div1 hhh hll by = go hhh hll 0
                    where (t2, t1) = quotRem maxBound by
                          go h l c =
                              if z == 0
                              then (c + W (fromIntegral t8) t7 + W 0 t10, t9)
                              else go (fromIntegral z) t5
                                      (c + (W (fromIntegral t8) t7))
                            where h1 = fromIntegral h
                                  (t4, t3) = unwrappedMul h1 (t1 + 1)
                                  (t6, t5) = unwrappedAdd t3 l
                                  z = t4 + t6
                                  (t8, t7) = unwrappedMul h1 t2
                                  (t10, t9) = quotRem t5 by
                  div2 hhh hll by = go hhh hll (0, 0)
                    where (t2, t1) = quotRem maxBound by
                          go h l c =
                              if z == 0
                              then (addT (addT c (t8, t7)) (0, t10), t9)
                              else go z t5 (addT c (t8, t7))
                            where (t4, t3) = unwrappedMul h (t1 + 1)
                                  (t6, t5) = unwrappedAdd t3 l
                                  z = t4 + t6
                                  (t8, t7) = unwrappedMul h t2
                                  (t10, t9) = quotRem t5 by
                          addT (lhh, lhl) (llh, lll) = (lhh + llh + t4, t3)
                            where (t4, t3) = unwrappedAdd lhl lll

          SIGNED:
            quotRem x y =
              if x < 0
              then
                if y < 0
                then let (q, r) = quotRem (negate $ unsignedWord x)
                                          (negate $ unsignedWord y) in
                       (signedWord q, signedWord $ negate r)
                else let (q, r) = quotRem (negate $ unsignedWord x)
                                          (unsignedWord y) in
                       (signedWord $ negate q, signedWord $ negate r)
              else
                if y < 0
                then let (q, r) = quotRem (unsignedWord x)
                                          (negate $ unsignedWord y) in
                       (signedWord $ negate q, signedWord r)
                else let (q, r) = quotRem (unsignedWord x)
                                          (unsignedWord y) in
                       (signedWord q, signedWord r)
        -}
        , if signed
          then
            funXY 'quotRem $
              CondE (appVN 'testMsb [x])
                (CondE (appVN 'testMsb [y])
                   (LetE [vals [q, r] $
                            appV 'quotRem
                              [ appV 'unsignedWord [appVN 'negate [x]]
                              , appV 'unsignedWord [appVN 'negate [y]] ]]
                      (tup [ appVN 'signedWord [q]
                            , appV 'signedWord [appVN 'negate [r]] ]))
                   (LetE [vals [q, r] $
                            appV 'quotRem
                              [ appV 'unsignedWord [appVN 'negate [x]]
                              , appVN 'unsignedWord [y] ]]
                      (tup [ appV 'signedWord [appVN 'negate [q]]
                            , appV 'signedWord [appVN 'negate [r]] ])))
                (CondE (appVN 'testMsb [y])
                   (LetE [vals [q, r] $
                            appV 'quotRem
                              [ appVN 'unsignedWord [x]
                              , appV 'unsignedWord [appVN 'negate [y]] ]]
                      (tup [ appV 'signedWord [appVN 'negate [q]]
                            , appVN 'signedWord [r] ]))
                   (LetE [vals [q, r] $
                            appV 'quotRem
                              [ appVN 'unsignedWord [x]
                              , appVN 'unsignedWord [y] ]]
                      (tup [ appVN 'signedWord [q]
                            , appVN 'signedWord [r] ])))
          else
            funHiLo2XY' 'quotRem
              (CondE (appV '(&&) [ appVN '(==) [hi', 'allZeroes]
                                 , appVN '(==) [lo', 'allZeroes] ])
                 (appV 'error [litS "divide by zero"])
                 (CaseE (appVN 'compare [hi, hi'])
                    [ match (conP 'LT []) (tup [zeroE, VarE x])
                    , match (conP 'EQ [])
                        (CaseE (appVN 'compare [lo, lo'])
                           [ match (conP 'LT []) (tup [zeroE, VarE x])
                           , match (conP 'EQ []) (tup [oneE, zeroE])
                           , Match (conP 'GT [])
                               (GuardedB $ return
                                  ( NormalG (appVN '(==) [hi', 'allZeroes])
                                  , tup [ appWN ['allZeroes, t2]
                                         , appWN ['allZeroes, t1] ]))
                               [vals [t2, t1] $ appVN 'quotRem [lo, lo']]
                           , match (conP 'GT []) $
                               tup [ oneE
                                    , appW [zeroE, appVN '(-) [lo, lo']] ]
                           ])
                    , Match (conP 'GT [])
                        (GuardedB $ return
                           ( NormalG (appVN '(==) [lo', 'allZeroes])
                           , tup
                               [ appW [zeroE, appVN 'fromIntegral [t2]]
                               , appW [appVN 'fromIntegral [t1], VarE lo]
                               ] ))
                        [vals [t2, t1] $ appVN 'quotRem [hi, hi']]
                    , Match (conP 'GT [])
                        (GuardedB $ return
                           ( NormalG (appV '(&&)
                                        [ appVN '(==) [hi', 'allZeroes]
                                        , appVN '(==) [lo', 'maxBound] ])
                           , CondE (appVN '(==) [t2, 'allZeroes])
                               (CondE (appVN '(==) [t1, 'maxBound])
                                  (tup
                                     [ appV '(+)
                                         [ appWN ['allZeroes, z]
                                         , oneE ]
                                     , zeroE ])
                                  (tup
                                     [ appWN ['allZeroes, z]
                                     , appWN ['allZeroes, t1] ]))
                               (CondE (appVN '(==) [t1, 'maxBound])
                                  (tup
                                     [ appV '(+)
                                         [appWN ['allZeroes, z], litI 2]
                                     , oneE ])
                                  (CondE
                                     (appV '(==)
                                        [ VarE t1
                                        , appVN 'xor ['maxBound, 'lsb]
                                        ])
                                     (tup
                                        [ appV '(+)
                                            [appWN ['allZeroes, z], litI 2]
                                        , zeroE ])
                                     (tup
                                        [ appV '(+)
                                            [appWN ['allZeroes, z], oneE]
                                        , appW [ zeroE
                                               , appVN '(+) [t1, 'lsb] ]
                                        ])))
                           ))
                        [ val z $ appVN 'fromIntegral [hi]
                        , vals [t2, t1] $ appVN 'unwrappedAdd [z, lo] ]
                    , Match (conP 'GT [])
                        (GuardedB $ return
                           ( NormalG (appVN '(==) [hi', 'allZeroes])
                           , tup [VarE t2, appWN ['allZeroes, t1]] ))
                        [vals [t2, t1] $ appVN div1 [hi, lo, lo']]
                    , match' (conP 'GT [])
                        (CondE (appVN '(==) [t1, t2])
                               (tup [oneE, appVN '(-) [x, y]])
                               (tup [ appW [zeroE, appVN 'fromIntegral [q2]]
                                     , appVN 'shiftR [r2, t2] ]))
                        [ val t1 $ appVN 'leadingZeroes [hi]
                        , val t2 $ appVN 'leadingZeroes [hi']
                        , val z $ appV 'shiftR
                                    [ VarE hi
                                    , appV '(-) [hiSizeE, VarE t2]
                                    ]
                        , ValD (conP cn [VarP hhh, VarP hll])
                            (NormalB $ appVN 'shiftL [x, t2]) []
                        , ValD (AsP v $ conP cn [VarP lhh, VarP lll])
                            (NormalB $ appVN 'shiftL [y, t2]) []
                        , ValD (TupP [ TupP [LitP (IntegerL 0), VarP q1]
                                     , VarP r1 ])
                            (NormalB $ appVN div2 [z, hhh, lhh]) []
                        , vals [t4, t3] $
                            appV 'unwrappedMul
                              [appVN 'fromIntegral [q1], VarE lll]
                        , val t5 $ appW [appVN 'fromIntegral [t4], VarE t3]
                        , val t6 $ appWN [r1, hll]
                        , vals [t8, t7] $ appVN 'unwrappedAdd [t6, v]
                        , vals [t10, t9] $ appVN 'unwrappedAdd [t7, v]
                        , vals [q2, r2] $
                            CondE (appVN '(>) [t5, t6])
                              (CondE (appV '(==) [appVN 'loWord [t8], zeroE])
                                 (CondE (appVN '(>=) [t7, t5])
                                    (tup [ appVN '(-) [q1, 'lsb]
                                          , appVN '(-) [t7, t5] ])
                                    (CondE (appV '(==) [ appVN 'loWord [t10]
                                                       , zeroE ])
                                       (tup [ appV '(-) [VarE q1, litI 2]
                                             , appVN '(-) [t9, t5] ])
                                       (tup [ appV '(-) [VarE q1, litI 2]
                                             , appV '(+)
                                                 [ appVN '(-) ['maxBound, t5]
                                                 , appVN '(+) [t9, 'lsb]
                                                 ]
                                             ])))
                                 (tup [ appVN '(-) [q1, 'lsb]
                                       , appV '(+)
                                           [ appVN '(-) ['maxBound, t5]
                                           , appVN '(+) [t7, 'lsb] ]
                                       ]))
                              (tup [VarE q1, appVN '(-) [t6, t5]])
                        ]
                    ]))
              [ FunD div1 $ return $
                  Clause [VarP hhh, VarP hll, VarP by]
                    (NormalB (appVN go [hhh, hll, 'allZeroes]))
                    [ vals [t2, t1] $ appVN 'quotRem ['maxBound, by]
                    , FunD go $ return $
                        Clause [VarP h, VarP l, VarP c]
                          (NormalB
                             (CondE (appVN '(==) [z, 'allZeroes])
                                (tup [ appV '(+)
                                          [ VarE c
                                          , appV '(+)
                                              [ appW [ appVN 'fromIntegral [t8]
                                                     , VarE t7 ]
                                              , appWN ['allZeroes, t10] ]
                                          ]
                                      , VarE t9 ])
                                (appV go
                                   [ appVN 'fromIntegral [z]
                                   , VarE t5
                                   , appV '(+)
                                       [ VarE c
                                       , appW [ appVN 'fromIntegral [t8]
                                              , VarE t7 ]
                                       ]
                                   ])))
                          [ val h1 $ appVN 'fromIntegral [h]
                          , vals [t4, t3] $
                              appV 'unwrappedMul
                                [VarE h1, appVN '(+) [t1, 'lsb]]
                          , vals [t6, t5] $ appVN 'unwrappedAdd [t3, l]
                          , val z $ appVN '(+) [t4, t6]
                          , vals [t8, t7] $ appVN 'unwrappedMul [h1, t2]
                          , vals [t10, t9] $ appVN 'quotRem [t5, by] ]
                    ]
              , FunD div2 $ return $
                  Clause [VarP hhh, VarP hll, VarP by]
                    (NormalB (appV go [ VarE hhh
                                      , VarE hll
                                      , tup [zeroE, zeroE]]))
                    [ vals [t2, t1] $ appVN 'quotRem ['maxBound, by]
                    , FunD go $ return $
                        Clause [VarP h, VarP l, VarP c]
                          (NormalB
                             (CondE (appVN '(==) [z, 'allZeroes])
                                (tup [ appV addT
                                          [ VarE c
                                          , appV addT
                                              [ tup [VarE t8 , VarE t7]
                                              , tup [zeroE, VarE t10] ]
                                          ]
                                      , VarE t9 ])
                                (appV go
                                   [ VarE z
                                   , VarE t5
                                   , appV addT
                                       [ VarE c
                                       , tup [VarE t8, VarE t7]
                                       ]
                                   ])))
                          [ vals [t4, t3] $
                              appV 'unwrappedMul
                                [VarE h, appVN '(+) [t1, 'lsb]]
                          , vals [t6, t5] $ appVN 'unwrappedAdd [t3, l]
                          , val z $ appVN '(+) [t4, t6]
                          , vals [t8, t7] $ appVN 'unwrappedMul [h, t2]
                          , vals [t10, t9] $ appVN 'quotRem [t5, by] ]
                    , FunD addT $ return $
                        Clause [ TupP [VarP lhh, VarP lhl]
                               , TupP [VarP llh, VarP lll]
                               ]
                          (NormalB (tup [ appV '(+)
                                             [ VarE t4
                                             , appVN '(+) [lhh, llh]
                                             ]
                                         , VarE t3
                                         ]))
                          [vals [t4, t3] $ appVN 'unwrappedAdd [lhl, lll]]
                    ]
              ]
        {-
          UNSIGNED:
            divMod = quotRem

          SIGNED:
            divMod x y =
              if x < 0
              then
                if y < 0
                then let (q, r) = quotRem (negate $ unsignedWord x)
                                          (negate $ unsignedWord y) in
                       (signedWord q, signedWord $ negate r)
                else let (q, r) = quotRem (negate $ unsignedWord x)
                                          (unsignedWord y)
                         q1 = signedWord (negate q)
                         r1 = signedWord (negate r) in
                       if r == 0
                       then (q1, r1)
                       else (q1 - 1, r1 + y)
              else
                if y < 0
                then let (q, r) = quotRem (unsignedWord x)
                                          (negate $ unsignedWord y)
                         q1 = signedWord (negate q)
                         r1 = signedWord r in
                       if r == 0
                       then (q1, r1)
                       else (q1 - 1, r1 + y)
                else let (q, r) = quotRem (unsignedWord x)
                                          (unsignedWord y) in
                       (signedWord q, signedWord r)
        -}
        , if signed
          then
            funXY 'divMod $
              CondE (appVN 'testMsb [x])
                (CondE (appVN 'testMsb [y])
                   (LetE [vals [q, r] $
                            appV 'quotRem
                              [ appV 'unsignedWord [appVN 'negate [x]]
                              , appV 'unsignedWord [appVN 'negate [y]] ]]
                      (tup [ appVN 'signedWord [q]
                            , appV 'signedWord [appVN 'negate [r]] ]))
                   (LetE [ vals [q, r] $
                             appV 'quotRem
                               [ appV 'unsignedWord [appVN 'negate [x]]
                               , appVN 'unsignedWord [y] ]
                         , val q1 $ appV 'signedWord [appVN 'negate [q]]
                         , val r1 $ appV 'signedWord [appVN 'negate [r]]
                         ]
                      (CondE (appVN '(==) [r, 'allZeroes])
                         (tup [VarE q1, VarE r1])
                         (tup [ appVN '(-) [q1, 'lsb]
                               , appVN '(+) [r1, y] ]))))
                (CondE (appVN 'testMsb [y])
                   (LetE [ vals [q, r] $
                             appV 'quotRem
                               [ appVN 'unsignedWord [x]
                               , appV 'unsignedWord [appVN 'negate [y]] ]
                         , val q1 $ appV 'signedWord [appVN 'negate [q]]
                         , val r1 $ appVN 'signedWord [r]
                         ]
                      (CondE (appVN '(==) [r, 'allZeroes])
                         (tup [VarE q1, VarE r1])
                         (tup [ appVN '(-) [q1, 'lsb]
                               , appVN '(+) [r1, y] ])))
                   (LetE [vals [q, r] $
                            appV 'quotRem
                              [ appVN 'unsignedWord [x]
                              , appVN 'unsignedWord [y] ]]
                      (tup [ appVN 'signedWord [q]
                            , appVN 'signedWord [r] ])))
          else
            fun 'divMod $ VarE 'quotRem
        ] ++
        if signed then [] else [inline 'divMod]
    , inst ''Show [tp]
        [ fun 'show $ appVN '(.) ['show, 'toInteger]
        , inline 'show ]
    , inst ''Read [tp]
        {-
          readsPrec x y = fmap (\(q, r) → (fromInteger q, r))
                        $ readsPrec x y
        -}
        [ funXY 'readsPrec $
            appV 'fmap [ LamE [TupP [VarP q, VarP r]]
                              (tup [appVN 'fromInteger [q], VarE r])
                       , appVN 'readsPrec [x, y] ]
        ]
    , inst ''Hashable [tp]
#if MIN_VERSION_hashable(1,2,0)
        {-
          hashWithSalt x (W hi lo) =
            x `hashWithSalt` hi `hashWithSalt` lo
        -}
        [ funXHiLo 'hashWithSalt $
            appV 'hashWithSalt [appVN 'hashWithSalt [x, hi], VarE lo]
#else
        {- hash (W hi lo) = hash hi `combine` hash lo -}
        [ funHiLo 'hash $ appV 'combine [appVN 'hash [hi], appVN 'hash [lo]]
        , inline 'hash
#endif
        , inline 'hashWithSalt ]
    , inst ''Ix [tp]
        {- range (x, y) = enumFromTo x y -}
        [ funTup 'range $ appVN 'enumFromTo [x, y]
        , inline 'range
        {- unsafeIndex (x, _) z = fromIntegral z - fromIntegral x -}
        , funTupLZ 'unsafeIndex $
            appV '(-) [appVN 'fromIntegral [z], appVN 'fromIntegral [x]]
        , inline 'unsafeIndex
        {- inRange (x, y) z = z >= x && z <= y -}
        , funTupZ 'inRange $
            appV '(&&) [appVN '(>=) [z, x], appVN '(<=) [z, y]]
        , inline 'inRange ]
    , inst ''Bits [tp] $
        {- bitSize _ = bitSize (undefined ∷ H) + bitSize (undefined ∷ L) -}
        [ fun_ 'bitSize $ appV '(+) [hiSizeE, loSizeE]
        , inline 'bitSize
#if MIN_VERSION_base(4,7,0)
        {- bitSizeMaybe = Just . finiteBitSize -}
        , fun 'bitSizeMaybe $ appV '(.) [ConE 'Just, VarE 'finiteBitSize]
        , inline 'bitSizeMaybe
#endif
        {- isSigned _ = SIGNED -}
        , fun_ 'isSigned $ ConE $ if signed then 'True else 'False
        , inline 'isSigned
        {- complement (W hi lo) = W (complement hi) (complement lo) -}
        , funHiLo 'complement $
            appW [appVN 'complement [hi], appVN 'complement [lo]]
        , inline 'complement
        {- xor (W hi lo) (W hi' lo') = W (xor hi hi') (xor lo lo') -}
        , funHiLo2 'xor $ appW [appVN 'xor [hi, hi'], appVN 'xor [lo, lo']]
        , inline 'xor
        {- (W hi lo) .&. (W hi' lo') = W (hi .&. hi') (lo .&. lo') -}
        , funHiLo2 '(.&.) $
            appW [appVN '(.&.) [hi, hi'], appVN '(.&.) [lo, lo']]
        , inline '(.&.)
        {- (W hi lo) .|. (W hi' lo') = W (hi .|. hi') (lo .|. lo') -}
        , funHiLo2 '(.|.) $
            appW [appVN '(.|.) [hi, hi'], appVN '(.|.) [lo, lo']]
        , inline '(.|.)
        {-
          shiftL (W hi lo) x =
              if y > 0
                then W (shiftL hi x .|. fromIntegral (shiftR lo y))
                       (shiftL lo x)
                else W (fromIntegral $ shiftL lo $ negate y) 0
            where y = bitSize (undefined ∷ L) - x
        -}
        , funHiLoX' 'shiftL
            (CondE (appV '(>) [VarE y, litI 0])
                   (appW
                      [ appV '(.|.)
                          [ appVN 'shiftL [hi, x]
                          , appV 'fromIntegral [appVN 'shiftR [lo, y]] ]
                      , appVN 'shiftL [lo, x] ])
                   (appW [ appV 'fromIntegral
                             [appV 'shiftL [VarE lo, appVN 'negate [y]]]
                         , zeroE ]))
            [val y $ appV '(-) [loSizeE, VarE x]]
        {-
          shiftR (W hi lo) x =
              W (shiftR hi x)
                (if y >= 0 then shiftL (fromIntegral hi) y .|. shiftR lo x
                           else z)
            where y = bitSize (undefined ∷ L) - x
                  z = if SIGNED
                      then fromIntegral $
                             shiftR (fromIntegral hi ∷ SignedWord L) $
                               negate y
                      else shiftR (fromIntegral hi) $ negate y
        -}
        , funHiLoX' 'shiftR
            (appW [ appVN 'shiftR [hi, x]
                  , CondE (appV '(>=) [VarE y, litI 0])
                          (appV '(.|.)
                             [ appV 'shiftL
                                 [appVN 'fromIntegral [hi], VarE y]
                             , appVN 'shiftR [lo, x] ])
                          (VarE z) ])
            [ val y $ appV '(-) [loSizeE, VarE x]
            , val z $
                if signed
                then appV 'fromIntegral
                       [appV 'shiftR
                          [ SigE (appVN 'fromIntegral [hi])
                                 (AppT (ConT ''SignedWord) loT)
                          , appVN 'negate [y] ]]
                else appV 'shiftR [ appVN 'fromIntegral [hi]
                                  , appVN 'negate [y] ]
            ]
        {-
          UNSIGNED:
            rotateL (W hi lo) x =
                if y >= 0
                then W (fromIntegral (shiftL lo y) .|. shiftR hi z)
                     W (shiftL (fromIntegral hi) (bitSize (undefined ∷ L) - z)
                        .|. shiftR lo z)
                else W (fromIntegral (shiftR lo $ negate y) .|. shiftL hi x)
                       (shift (fromIntegral hi) (bitSize (undefined ∷ L) - z)
                        .|. shiftL lo x
                        .|. shiftR lo z)
              where y = x - bitSize (undefined ∷ L)
                    z = bitSize (undefined ∷ W) - x
          SIGNED:
            rotateL x y = signedWord $ rotateL (unsignedWord x) y
        -}
        , if signed
          then
            funXY 'rotateL $
              appV 'signedWord
                   [appV 'rotateL [appVN 'unsignedWord [x], VarE y]]
          else
            funHiLoX' 'rotateL
              (CondE (appV '(>=) [VarE y, litI 0])
                 (appW
                    [ appV '(.|.)
                        [ appV 'fromIntegral [appVN 'shiftL [lo, y]]
                        , appVN 'shiftR [hi, z] ]
                    , appV '(.|.)
                        [ appV 'shiftL
                            [ appVN 'fromIntegral [hi]
                            , appV '(-) [loSizeE, VarE z]
                            ]
                        , appVN 'shiftR [lo, z] ]
                    ])
                 (appW
                    [ appV '(.|.)
                        [ appV 'fromIntegral
                            [appV 'shiftR [VarE lo, appVN 'negate [y]]]
                        , appVN 'shiftL [hi, x] ]
                    , appV '(.|.)
                        [ appV 'shift
                            [ appVN 'fromIntegral [hi]
                            , appV '(-) [loSizeE, VarE z] ]
                        , appV '(.|.)
                            [appVN 'shiftL [lo, x], appVN 'shiftR [lo, z]] ]
                    ]))
              [ val y $ appV '(-) [VarE x, loSizeE]
              , val z $ appV '(-) [sizeE, VarE x]
              ]
        {- rotateR x y = rotateL x $ bitSize (undefined ∷ W) - y -}
        , funXY 'rotateR $ appV 'rotateL [VarE x, appV '(-) [sizeE, VarE y]]
        , inline 'rotateR
        {-
          bit x = if y >= 0 then W (bit y) 0 else W 0 (bit x)
            where y = x - bitSize (undefined ∷ LoWord W)
        -}
        , funX' 'bit (CondE (appV '(>=) [VarE y, litI 0])
                            (appW [appVN 'bit [y], zeroE])
                            (appW [zeroE, appVN 'bit [x]]))
            [val y $ appV '(-) [VarE x, loSizeE]]
        , inlinable 'bit
        {-
          setBit (W hi lo) x =
              if y >= 0 then W (setBit hi y) lo else W hi (setBit lo x)
            where y = x - bitSize (undefined ∷ L)
        -}
        , funHiLoX' 'setBit
            (CondE (appV '(>=) [VarE y, litI 0])
                   (appW [appVN 'setBit [hi, y], VarE lo])
                   (appW [VarE hi, appVN 'setBit [lo, x]]))
            [val y $
               appV '(-) [ VarE x
                         , appV 'bitSize [SigE (VarE 'undefined) loT] ]]
        , inlinable 'setBit
        {-
          clearBit (W hi lo) x =
              if y >= 0 then W (clearBit hi y) lo
                        else W hi (clearBit lo x)
            where y = x - bitSize (undefined ∷ L)
        -}
        , funHiLoX' 'clearBit
            (CondE (appV '(>=) [VarE y, litI 0])
                   (appW [appVN 'clearBit [hi, y], VarE lo])
                   (appW [VarE hi, appVN 'clearBit [lo, x]]))
            [val y $ appV '(-) [VarE x, loSizeE]]
        , inlinable 'clearBit
        {-
          complementBit (W hi lo) x =
              if y >= 0 then W (complementBit hi y) lo
                        else W hi (complementBit lo x)
            where y = x - bitSize (undefined ∷ L)
        -}
        , funHiLoX' 'complementBit
            (CondE (appV '(>=) [VarE y, litI 0])
                   (appW [appVN 'complementBit [hi, y], VarE lo])
                   (appW [VarE hi, appVN 'complementBit [lo, x]]))
            [val y $ appV '(-) [VarE x, loSizeE]]
        , inlinable 'complementBit
        {-
          testBit (W hi lo) x =
              if y >= 0 then testBit hi y else testBit lo x
            where y = x - bitSize (undefined ∷ L)
        -}
        , funHiLoX' 'testBit
            (CondE (appV '(>=) [VarE y, litI 0])
                   (appVN 'testBit [hi, y])
                   (appVN 'testBit [lo, x]))
            [val y $ appV '(-) [VarE x, loSizeE]]
        , inlinable 'testBit
        {- popCount (W hi lo) = popCount hi + popCount lo -}
        , funHiLo 'popCount
            (appV '(+) [appVN 'popCount [hi], appVN 'popCount [lo]])
        , inline 'popCount
        ] ++
        if signed then [inline 'rotateL] else []
#if MIN_VERSION_base(4,7,0)
    , inst ''FiniteBits [tp]
        {-
           finiteBitSize = finiteBitSize (undefined ∷ H) +
                           finiteBitSize (undefined ∷ L)
        -}
        [ fun_ 'finiteBitSize $ appV '(+) [hiSizeE, loSizeE]
        , inline 'finiteBitSize
# if MIN_VERSION_base(4,8,0)
        {- countLeadingZeros = leadingZeroes -}
        , fun 'countLeadingZeros $ VarE 'leadingZeroes
        , inline 'countLeadingZeros
        {- countTrailingZeros = trailingZeroes -}
        , fun 'countTrailingZeros $ VarE 'trailingZeroes
        , inline 'countTrailingZeros
# endif
        ]
#endif
    , inst ''BinaryWord [tp]
        [ tySynInst ''UnsignedWord [tpT] $
            ConT $ if signed then otp else tp
        , tySynInst ''SignedWord [tpT] $
            ConT $ if signed then tp else otp
        {-
          UNSIGNED:
            unsignedWord = id

          SIGNED:
            unsignedWord (W hi lo) = U (unsignedWord hi) lo
        -}
        , if signed
          then
            funHiLo 'unsignedWord $
              appC ocn [appVN 'unsignedWord [hi], VarE lo]
          else
            fun 'unsignedWord $ VarE 'id
        , inline 'unsignedWord
        {-
          UNSIGNED:
            signedWord (W hi lo) = S (signedWord hi) lo

          SIGNED:
            signedWord = id
        -}
        , if signed
          then
            fun 'signedWord $ VarE 'id
          else
            funHiLo 'signedWord $
              appC ocn [appVN 'signedWord [hi], VarE lo]
        , inline 'signedWord
        {-
          UNSIGNED:
            unwrappedAdd (W hi lo) (W hi' lo') = (W 0 z, W y x)
              where (t1, x) = unwrappedAdd lo lo'
                    (t3, t2) = unwrappedAdd hi (fromIntegral t1)
                    (t4, y) = unwrappedAdd t2 hi'
                    z = fromIntegral $ t3 + t4
          SIGNED:
            unwrappedAdd x y = (z, t4)
              where t1 = if x < 0 then maxBound else minBound
                    t2 = if y < 0 then maxBound else minBound
                    (t3, t4) = unwrappedAdd (unsignedWord x) (unsignedWord y)
                    z = signedWord $ t1 + t2 + t3
        -}
        , if signed
          then
            funXY' 'unwrappedAdd (tup [VarE z, VarE t4])
              [ val t1 $ CondE (appVN 'testMsb [x])
                               (VarE 'maxBound) (VarE 'minBound)
              , val t2 $ CondE (appVN 'testMsb [y])
                               (VarE 'maxBound) (VarE 'minBound)
              , vals [t3, t4] $
                  appV 'unwrappedAdd [ appVN 'unsignedWord [x]
                                     , appVN 'unsignedWord [y] ]
              , val z $
                  appV 'signedWord [appV '(+) [VarE t1, appVN '(+) [t2, t3]]]
              ]
          else
            funHiLo2' 'unwrappedAdd
              (tup [appWN ['allZeroes, z], appWN [y, x]])
              [ vals [t1, x] $ appVN 'unwrappedAdd [lo, lo']
              , vals [t3, t2] $
                  appV 'unwrappedAdd [VarE hi, appVN 'fromIntegral [t1]]
              , vals [t4, y] $ appVN 'unwrappedAdd [t2, hi']
              , val z $ appV 'fromIntegral [appVN '(+) [t3, t4]]
              ]
        {-
          UNSIGNED:
            unwrappedMul (W hi lo) (W hi' lo') =
                (W (hhh + fromIntegral (shiftR t9 y) + shiftL x z)
                   (shiftL t9 z .|. shiftR t3 y),
                 W (fromIntegral t3) lll)
              where (llh, lll) = unwrappedMul lo lo'
                    (hlh, hll) = unwrappedMul (fromIntegral hi) lo'
                    (lhh, lhl) = unwrappedMul lo (fromIntegral hi')
                    (hhh, hhl) = unwrappedMul hi hi'
                    (t2, t1) = unwrappedAdd llh hll
                    (t4, t3) = unwrappedAdd t1 lhl
                    (t6, t5) = unwrappedAdd (fromIntegral hhl) (t2 + t4)
                    (t8, t7) = unwrappedAdd t5 lhh
                    (t10, t9) = unwrappedAdd t7 hlh
                    x = fromIntegral $ t6 + t8 + t10
                    y = bitSize (undefined ∷ H)
                    z = bitSize (undefined ∷ L) - y
          SIGNED:
            unwrappedMul (W hi lo) (W hi' lo') = (x, y)
              where t1 = W (complement hi') (complement lo') + 1
                    t2 = W (complement hi) (complement lo) + 1
                    (t3, y) = unwrappedMul (U (unsignedWord hi) lo)
                                           (U (unsignedWord hi') lo')
                    z = signedWord t3
                    x = if hi < 0
                        then if hi' < 0
                             then z + t1 + t2
                             else z + t1
                        else if hi' < 0
                             then z + t2
                             else z
        -}
        , if signed
          then
            funHiLo2' 'unwrappedMul (tup [VarE x, VarE y])
              [ val t1 $
                  appV '(+) [ appW [ appVN 'complement [hi']
                                   , appVN 'complement [lo'] ]
                            , oneE ]
              , val t2 $
                  appV '(+) [ appW [ appVN 'complement [hi]
                                   , appVN 'complement [lo] ]
                            , oneE ]
              , vals [t3, y] $
                  appV 'unwrappedMul
                    [ appC ocn [appVN 'unsignedWord [hi], VarE lo]
                    , appC ocn [appVN 'unsignedWord [hi'], VarE lo'] ]
              , val z $ appVN 'signedWord [t3]
              , val x $
                  CondE (appVN 'testMsb [hi])
                    (CondE (appVN 'testMsb [hi'])
                       (appV '(+) [VarE z, appVN '(+) [t1, t2]])
                       (appVN '(+) [z, t1]))
                    (CondE (appVN 'testMsb [hi'])
                       (appVN '(+) [z, t2]) (VarE z))
              ]
          else
            funHiLo2' 'unwrappedMul
              (tup [ appW
                        [ appV '(+)
                            [ VarE hhh
                            , appV '(+)
                                [ appV 'fromIntegral [appVN 'shiftR [t9, y]]
                                , appVN 'shiftL [x, z] ]
                            ]
                        , appV '(.|.) [ appVN 'shiftL [t9, z]
                                      , appVN 'shiftR [t3, y] ]
                        ]
                    , appW [appVN 'fromIntegral [t3], VarE lll]
                    ])
              [ vals [llh, lll] $ appVN 'unwrappedMul [lo, lo']
              , vals [hlh, hll] $
                  appV 'unwrappedMul [appVN 'fromIntegral [hi], VarE lo']
              , vals [lhh, lhl] $
                  appV 'unwrappedMul [VarE lo, appVN 'fromIntegral [hi']]
              , vals [hhh, hhl] $ appVN 'unwrappedMul [hi, hi']
              , vals [t2, t1] $ appVN 'unwrappedAdd [llh, hll]
              , vals [t4, t3] $ appVN 'unwrappedAdd [t1, lhl]
              , vals [t6, t5] $
                  appV 'unwrappedAdd [ appVN 'fromIntegral [hhl]
                                     , appVN '(+) [t2, t4] ]
              , vals [t8, t7] $ appVN 'unwrappedAdd [t5, lhh]
              , vals [t10, t9] $ appVN 'unwrappedAdd [t7, hlh]
              , val x $
                  appV 'fromIntegral
                    [appV '(+) [VarE t6, appVN '(+) [t8, t10]]]
              , val y $ hiSizeE
              , val z $ appV '(-) [loSizeE, VarE y]
              ]
        {-
          UNSIGNED:
            leadingZeroes (W hi lo) =
                if x == y then y + leadingZeroes lo else x
              where x = leadingZeroes hi
                    y = bitSize (undefined ∷ H)
          SIGNED:
            leadingZeroes = leadingZeroes . unsignedWord
        -}
        , if signed
          then
            fun 'leadingZeroes $ appVN '(.) ['leadingZeroes, 'unsignedWord]
          else
            funHiLo' 'leadingZeroes
              (CondE (appVN '(==) [x, y])
                     (appV '(+) [VarE y, appVN 'leadingZeroes [lo]])
                     (VarE x))
              [ val x $ appVN 'leadingZeroes [hi]
              , val y $ hiSizeE
              ]
        , if signed then inlinable 'leadingZeroes
                    else inline 'leadingZeroes
        {-
          UNSIGNED:
            trailingZeroes (W hi lo) =
                if x == y then y + trailingZeroes hi else x
              where x = trailingZeroes lo
                    y = bitSize (undefined ∷ L)
          SIGNED:
            trailingZeroes = trailingZeroes . unsignedWord
        -}
        , if signed
          then
            fun 'trailingZeroes $ appVN '(.) ['trailingZeroes, 'unsignedWord]
          else
            funHiLo' 'trailingZeroes
              (CondE (appVN '(==) [x, y])
                     (appV '(+) [VarE y, appVN 'trailingZeroes [hi]])
                     (VarE x))
              [ val x $ appVN 'trailingZeroes [lo]
              , val y $ loSizeE ]
        , if signed then inlinable 'trailingZeroes
                    else inline 'trailingZeroes
        {- allZeroes = W allZeroes allZeroes -}
        , fun 'allZeroes $ appWN ['allZeroes, 'allZeroes]
        , inline 'allZeroes
        {- allOnes = W allOnes allOnes -}
        , fun 'allOnes $ appWN ['allOnes, 'allOnes]
        , inline 'allOnes
        {- msb = W msb allZeroes -}
        , fun 'msb $ appWN ['msb, 'allZeroes]
        , inline 'msb
        {- lsb = W allZeroes lsb -}
        , fun 'lsb $ appWN ['allZeroes, 'lsb]
        , inline 'lsb
        {- testMsb (W hi _) = testMsb hi -}
        , funHi 'testMsb $ appVN 'testMsb [hi]
        , inline 'testMsb
        {- testLsb (W _ lo) = testLsb lo -}
        , funLo 'testLsb $ appVN 'testLsb [lo]
        , inline 'testLsb
        {- setMsb (W hi lo) = W (setMsb hi) lo -}
        , funHiLo 'setMsb $ appW [appVN 'setMsb [hi], VarE lo]
        , inline 'setMsb
        {- setLsb (W hi lo) = W hi (setLsb lo) -}
        , funHiLo 'setLsb $ appW [VarE hi, appVN 'setLsb [lo]]
        , inline 'setLsb
        {- clearMsb (W hi lo) = W (clearMsb hi) lo -}
        , funHiLo 'clearMsb $ appW [appVN 'clearMsb [hi], VarE lo]
        , inline 'clearMsb
        {- clearLsb (W hi lo) = W hi (clearLsb lo) -}
        , funHiLo 'clearLsb $ appW [VarE hi, appVN 'clearLsb [lo]]
        , inline 'clearLsb
        ]
    ]
  where
    x    = mkName "x"
    y    = mkName "y"
    z    = mkName "z"
    t1   = mkName "t1"
    t2   = mkName "t2"
    t3   = mkName "t3"
    t4   = mkName "t4"
    t5   = mkName "t5"
    t6   = mkName "t6"
    t7   = mkName "t7"
    t8   = mkName "t8"
    t9   = mkName "t9"
    t10  = mkName "t10"
    v    = mkName "v"
    q    = mkName "q"
    q1   = mkName "q1"
    q2   = mkName "q2"
    r    = mkName "r"
    r1   = mkName "r1"
    r2   = mkName "r2"
    lll  = mkName "lll"
    llh  = mkName "llh"
    lhl  = mkName "lhl"
    lhh  = mkName "lhh"
    hll  = mkName "hll"
    hlh  = mkName "hlh"
    hhl  = mkName "hhl"
    hhh  = mkName "hhh"
    h    = mkName "h"
    h1   = mkName "h1"
    l    = mkName "l"
    div1 = mkName "div1"
    div2 = mkName "div2"
    addT = mkName "addT"
    by   = mkName "by_"
    go   = mkName "go_"
    c    = mkName "c"
    next = mkName "next_"
    step = mkName "step_"
    to   = mkName "to_"
    down = mkName "down_"
    up   = mkName "up_"
    hi   = mkName "hi_"
    lo   = mkName "lo_"
    hi'  = mkName "hi'"
    lo'  = mkName "lo'"
    tpT  = ConT tp
    tySynInst n ps t =
#if MIN_VERSION_template_haskell(2,15,0)
      TySynInstD (TySynEqn Nothing (foldl AppT (ConT n) ps) t)
#elif MIN_VERSION_template_haskell(2,9,0)
      TySynInstD n (TySynEqn ps t)
#else
      TySynInstD n ps t
#endif
    inst cls params = InstanceD
#if MIN_VERSION_template_haskell(2,11,0)
                                Nothing
#endif
                                [] (foldl AppT (ConT cls) (ConT <$> params))
    fun n e       = FunD n [Clause [] (NormalB e) []]
    fun1 n a e    = FunD n [Clause [VarP a] (NormalB e) []]
    fun_ n e      = FunD n [Clause [WildP] (NormalB e) []]
    funX' n e ds  = FunD n [Clause [VarP x] (NormalB e) ds]
    funX n e      = funX' n e []
    funXY' n e ds = FunD n [Clause [VarP x, VarP y] (NormalB e) ds]
    funXY n e     = funXY' n e []
    funTup n e    = FunD n [Clause [TupP [VarP x, VarP y]] (NormalB e) []]
    funTupZ n e   =
      FunD n [Clause [TupP [VarP x, VarP y], VarP z] (NormalB e) []]
    funTupLZ n e  =
      FunD n [Clause [TupP [VarP x, WildP], VarP z] (NormalB e) []]
    funLo n e     = FunD n [Clause [conP cn [WildP, VarP lo]] (NormalB e) []]
    funHi n e     = FunD n [Clause [conP cn [VarP hi, WildP]] (NormalB e) []]
    funHiLo n e   = funHiLo' n e []
    funHiLo' n e ds  =
      FunD n [Clause [conP cn [VarP hi, VarP lo]] (NormalB e) ds]
    funHiLoX' n e ds =
      FunD n [Clause [conP cn [VarP hi, VarP lo], VarP x] (NormalB e) ds]
    funHiLo2 n e     = funHiLo2' n e []
    funHiLo2' n e ds =
      FunD n [Clause [ conP cn [VarP hi, VarP lo]
                     , conP cn [VarP hi', VarP lo'] ]
                     (NormalB e) ds]
    funHiLo2XY' n e ds =
      FunD n [Clause [ AsP x (conP cn [VarP hi, VarP lo])
                     , AsP y (conP cn [VarP hi', VarP lo']) ]
                     (NormalB e) ds]
    funXHiLo n e  = FunD n [Clause [VarP x, conP cn [VarP hi, VarP lo]]
                                   (NormalB e) []]
    match' p e ds = Match p (NormalB e) ds
    match p e     = match' p e []
    inline n = PragmaD $ InlineP n Inline FunLike AllPhases
    inlinable n = PragmaD $ InlineP n Inlinable FunLike AllPhases
    val n e   = ValD (VarP n) (NormalB e) []
    vals ns e = ValD (TupP (VarP <$> ns)) (NormalB e) []
    app f   = foldl AppE f
    appN f  = app f . fmap VarE
    appV f  = app (VarE f)
    appC f  = app (ConE f)
    appW    = appC cn
    appVN f = appN (VarE f)
    appCN f = appN (ConE f)
    appWN   = appCN cn
    litI = LitE . IntegerL
    litS = LitE . StringL
    zeroE = VarE 'allZeroes
    oneE  = VarE 'lsb
#if MIN_VERSION_base(4,7,0)
    loSizeE = appV 'finiteBitSize [SigE (VarE 'undefined) loT]
    hiSizeE = appV 'finiteBitSize [SigE (VarE 'undefined) hiT]
    sizeE   = appV 'finiteBitSize [SigE (VarE 'undefined) tpT]
#else
    loSizeE = appV 'bitSize [SigE (VarE 'undefined) loT]
    hiSizeE = appV 'bitSize [SigE (VarE 'undefined) hiT]
    sizeE   = appV 'bitSize [SigE (VarE 'undefined) tpT]
#endif
    singE e = appC '(:) [e, ConE '[]]
    conP name ps =
      ConP name
#if MIN_VERSION_template_haskell(2,18,0)
           [] ps
#else
           ps
#endif
    ruleP name lhs rhs phases =
      RuleP name
#if MIN_VERSION_template_haskell(2,15,0)
            Nothing
#endif
            [] lhs rhs phases
    mkRules = do
      let idRule = ruleP ("fromIntegral/" ++ show tp ++ "->" ++ show tp)
                         (VarE 'fromIntegral)
                         (SigE (VarE 'id) (AppT (AppT ArrowT tpT) tpT))
                         AllPhases
          signRule = ruleP ("fromIntegral/" ++ show tp ++ "->" ++ show otp)
                           (VarE 'fromIntegral)
                           (SigE (VarE (if signed then 'unsignedWord
                                                  else 'signedWord))
                                 (AppT (AppT ArrowT tpT) (ConT otp)))
                           AllPhases
      mkRules' [idRule, signRule] loT
               (VarE 'loWord)
               (VarE 'extendLo)
               (VarE 'signExtendLo)
    mkRules' rules t narrowE extE signExtE = do
      let narrowRule = ruleP ("fromIntegral/" ++ show tp ++ "->" ++ showT t)
                             (VarE 'fromIntegral)
                             (SigE narrowE (AppT (AppT ArrowT tpT) t))
                             AllPhases
          extRule = ruleP ("fromIntegral/" ++ showT t ++ "->" ++ show tp)
                          (VarE 'fromIntegral)
                          (SigE extE (AppT (AppT ArrowT t) tpT))
                          AllPhases
      signedRules ← do
        insts ← reifyInstances ''SignedWord [t]
        case insts of
#if MIN_VERSION_template_haskell(2,15,0)
          [TySynInstD (TySynEqn _ _ signT)] → return $
#elif MIN_VERSION_template_haskell(2,9,0)
          [TySynInstD _ (TySynEqn _ signT)] → return $
#else
          [TySynInstD _ _ signT] → return $
#endif
            [ ruleP ("fromIntegral/" ++ show tp ++ "->" ++ showT signT)
                    (VarE 'fromIntegral)
                    (SigE (AppE (appVN '(.) ['signedWord]) narrowE)
                          (AppT (AppT ArrowT tpT) signT))
                    AllPhases
            , ruleP ("fromIntegral/" ++ showT signT ++ "->" ++ show tp)
                    (VarE 'fromIntegral)
                    (SigE signExtE (AppT (AppT ArrowT signT) tpT))
                    AllPhases ]
          _ → return []
      let rules' = narrowRule : extRule : signedRules ++ rules
      case smallerStdTypes t of
        Just ts → do
          let smallRules = ts >>= \(uSmallName, sSmallName) →
                let uSmallT = ConT uSmallName
                    sSmallT = ConT sSmallName in
                [ ruleP ("fromIntegral/" ++
                         show tp ++ "->" ++ show uSmallName)
                        (VarE 'fromIntegral)
                        (SigE (appV '(.) [VarE 'fromIntegral, narrowE])
                              (AppT (AppT ArrowT tpT) uSmallT))
                        AllPhases
                , ruleP ("fromIntegral/" ++
                         show uSmallName ++ "->" ++ show tp)
                        (VarE 'fromIntegral)
                        (SigE (appV '(.) [extE, VarE 'fromIntegral])
                              (AppT (AppT ArrowT uSmallT) tpT))
                        AllPhases
                , ruleP ("fromIntegral/" ++
                         show tp ++ "->" ++ show sSmallName)
                        (VarE 'fromIntegral)
                        (SigE (appV '(.) [VarE 'fromIntegral, narrowE])
                              (AppT (AppT ArrowT tpT) sSmallT))
                        AllPhases
                , ruleP ("fromIntegral/" ++
                         show sSmallName ++ "->" ++ show tp)
                        (VarE 'fromIntegral)
                        (SigE (appV '(.) [signExtE, VarE 'fromIntegral])
                              (AppT (AppT ArrowT sSmallT) tpT))
                        AllPhases
                ]
          return $ PragmaD <$> rules' ++ smallRules
        _ → do
          insts ← reifyInstances ''LoWord [t]
          case insts of
#if MIN_VERSION_template_haskell(2,15,0)
            [TySynInstD (TySynEqn _ _ t')] →
#elif MIN_VERSION_template_haskell(2,9,0)
            [TySynInstD _ (TySynEqn _ t')] →
#else
            [TySynInstD _ _ t'] →
#endif
              mkRules' rules' t'
                       (appV '(.) [VarE 'loWord, narrowE])
                       (appV '(.) [VarE 'extendLo, extE])
                       (appV '(.) [VarE 'signExtendLo, signExtE])
            _ → return $ PragmaD <$> rules'
    showT (ConT n) = show n
    showT t = show t
    stdTypes = [(''Word64, ''Int64), (''Word32, ''Int32),
                (''Word16, ''Int16), (''Word8, ''Int8)]
    smallerStdTypes t = smallerStdTypes' t stdTypes
    smallerStdTypes' _ [] = Nothing
    smallerStdTypes' t ((ut, _) : ts)
      | ConT ut == t = Just ts
      | otherwise    = smallerStdTypes' t ts

