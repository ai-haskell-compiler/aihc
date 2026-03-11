-- |
-- Module      : Crypto.OpenSSL.ECC
-- License     : BSD-style
-- Stability   : experimental
-- Portability : Unix
--
module Crypto.OpenSSL.ECC
    ( EcPoint
    , EcGroup
    , EcKey
    -- * Curve group
    , ecGroupFromCurveOID
    , ecGroupGFp
    , ecGroupGF2m
    , ecGroupGetDegree
    , ecGroupGetOrder
    , ecGroupGetCoFactor
    , ecGroupGetGenerator
    , ecGroupGetCurveGFp
    , ecGroupGetCurveGF2m
    -- * EcPoint arithmetic
    , ecPointAdd
    , ecPointsSum
    , ecPointDbl
    , ecPointMul
    , ecPointMulWithGenerator
    , ecPointsMulAndSum
    , ecPointsMulOfPowerAndSum
    , ecPointGeneratorMul
    , ecPointInvert
    , ecPointInfinity
    , ecPointIsAtInfinity
    , ecPointIsOnCurve
    , ecPointEq
    -- * EcPoint serialization
    , PointConversionForm(..)
    , ecPointToOct
    , ecPointFromOct
    , ecPointFromJProjectiveGFp
    , ecPointToJProjectiveGFp
    , ecPointFromAffineGFp
    , ecPointToAffineGFp
    , ecPointFromAffineGF2m
    , ecPointToAffineGF2m
    -- * Key
    , ecKeyGenerateNew
    , ecKeyFromPair
    , ecKeyToPair
    ) where

import           Control.Monad (void, forM_)
import           Control.Applicative
import           Control.Exception (bracket)
import           Crypto.OpenSSL.ECC.Foreign
import           Crypto.OpenSSL.ASN1
import           Crypto.OpenSSL.BN
import           Crypto.OpenSSL.BN.Foreign
import           Crypto.OpenSSL.Misc
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Data.ByteArray as B

-- | An ellitic curve group
newtype EcGroup = EcGroup (ForeignPtr EC_GROUP)

-- | An elliptic curve point
newtype EcPoint = EcPoint (ForeignPtr EC_POINT)

-- | An elliptic curve key
newtype EcKey = EcKey (ForeignPtr EC_KEY)

data PointConversionForm =
      PointConversion_Compressed
    | PointConversion_Uncompressed
    | PointConversion_Hybrid
    deriving (Show,Eq)

ecPointConversionToC :: PointConversionForm -> PointConversionFormT
ecPointConversionToC PointConversion_Compressed   = 2
ecPointConversionToC PointConversion_Uncompressed = 4
ecPointConversionToC PointConversion_Hybrid       = 6

withPointNew :: Ptr EC_GROUP -> (Ptr EC_POINT -> IO ()) -> IO EcPoint
withPointNew grp f = do
    ptr <- ssl_point_new grp
    f ptr
    EcPoint <$> newForeignPtr ssl_point_free_funptr ptr

withPointNewWithReturn :: Ptr EC_GROUP -> (Ptr EC_POINT -> IO r) -> IO (r, EcPoint)
withPointNewWithReturn grp f = do
    ptr   <- ssl_point_new grp
    r     <- f ptr
    point <- EcPoint <$> newForeignPtr ssl_point_free_funptr ptr
    return (r, point)

withPointDup :: Ptr EC_GROUP -> Ptr EC_POINT -> (Ptr EC_POINT -> IO ()) -> IO EcPoint
withPointDup grp p f = do
    ptr <- ssl_point_dup p grp
    f ptr
    EcPoint <$> newForeignPtr ssl_point_free_funptr ptr

withPointTemp :: Ptr EC_GROUP -> (Ptr EC_POINT -> IO a) -> IO a
withPointTemp grp f = bracket (ssl_point_new grp) (ssl_point_free) f


-- | try to get a curve group from an ASN1 description string (OID)
--
-- e.g.
--
-- * "1.3.132.0.35" == SEC_P521_R1
--
-- * "1.2.840.10045.3.1.7" == SEC_P256_R1
--
ecGroupFromCurveOID :: String -> Maybe EcGroup
ecGroupFromCurveOID s = asn1Description s >>= grabCurve
  where
    grabCurve (Nid i) = doIO $ do
        g <- ssl_group_new_by_curve_name (fromIntegral i)
        if g == nullPtr
            then return Nothing
            else Just . EcGroup <$> newForeignPtr ssl_group_free g
    {-# NOINLINE grabCurve #-}

-- | Create a new GFp group with explicit (p,a,b,(x,y),order,h)
--
-- Generally, this interface should not be used, and user should
-- really not stray away from already defined curves.
--
-- Use at your own risks.
ecGroupGFp :: Integer -- ^ p
           -> Integer -- ^ a
           -> Integer -- ^ b
           -> (Integer,Integer) -- ^ generator
           -> Integer -- ^ order
           -> Integer -- ^ cofactor
           -> EcGroup
ecGroupGFp p a b (genX, genY) order cofactor = doIO $
    withIntegerAsBN p        $ \bnp        ->
    withIntegerAsBN a        $ \bna        ->
    withIntegerAsBN b        $ \bnb        ->
    withIntegerAsBN genX     $ \bnGX       ->
    withIntegerAsBN genY     $ \bnGY       ->
    withIntegerAsBN order    $ \bnOrder    ->
    withIntegerAsBN cofactor $ \bnCofactor ->
    withBnCtxNew             $ \bnCtx      -> do
        group <- ssl_group_new_curve_GFp bnp bna bnb bnCtx
        point <- ssl_point_new group
        check $ ssl_point_set_affine_coordinates_GFp group point bnGX bnGY bnCtx
        check $ ssl_group_set_generator group point bnOrder bnCofactor
        ssl_point_free point
        EcGroup <$> newForeignPtr ssl_group_free group
{-# NOINLINE ecGroupGFp #-}

-- | Create a new GF2m group with explicit (p,a,b,(x,y),order,h)
--
-- same warning as `ecGroupGFp`
ecGroupGF2m :: Integer -- ^ p
            -> Integer -- ^ a
            -> Integer -- ^ b
            -> (Integer,Integer) -- ^ generator
            -> Integer -- ^ order
            -> Integer -- ^ cofactor
            -> EcGroup
ecGroupGF2m p a b (genX, genY) order cofactor = doIO $
    withIntegerAsBN p        $ \bnp        ->
    withIntegerAsBN a        $ \bna        ->
    withIntegerAsBN b        $ \bnb        ->
    withIntegerAsBN genX     $ \bnGX       ->
    withIntegerAsBN genY     $ \bnGY       ->
    withIntegerAsBN order    $ \bnOrder    ->
    withIntegerAsBN cofactor $ \bnCofactor ->
    withBnCtxNew             $ \bnCtx      -> do
        group <- ssl_group_new_curve_GF2m bnp bna bnb bnCtx
        point <- ssl_point_new group
        check $ ssl_point_set_affine_coordinates_GF2m group point bnGX bnGY bnCtx
        check $ ssl_group_set_generator group point bnOrder bnCofactor
        ssl_point_free point
        EcGroup <$> newForeignPtr ssl_group_free group
{-# NOINLINE ecGroupGF2m #-}

-- | get the group degree (number of bytes)
ecGroupGetDegree :: EcGroup -> Int
ecGroupGetDegree (EcGroup g) = doIO $
    withForeignPtr g  $ \gptr ->
        fromIntegral <$> ssl_group_get_degree gptr
{-# NOINLINE ecGroupGetDegree #-}

-- | get the order of the subgroup generated by the generator
ecGroupGetOrder :: EcGroup -> Integer
ecGroupGetOrder (EcGroup g) = doIO $
    withForeignPtr g  $ \gptr  ->
    withBnCtxNew      $ \bnCtx ->
    withBnNew         $ \bn    -> do
        check $ ssl_group_get_order gptr bn bnCtx
        bnToInt bn
{-# NOINLINE ecGroupGetOrder #-}

--- | get the cofactor of the curve.
--
-- usually a small number h that:
-- h = #E(Fp) / n
ecGroupGetCoFactor :: EcGroup -> Integer
ecGroupGetCoFactor (EcGroup g) = doIO $
    withForeignPtr g  $ \gptr  ->
    withBnCtxNew      $ \bnCtx ->
    withBnNew         $ \bn    -> do
        check $ ssl_group_get_cofactor gptr bn bnCtx
        bnToInt bn
{-# NOINLINE ecGroupGetCoFactor #-}

-- | Get the group generator
ecGroupGetGenerator :: EcGroup -> EcPoint
ecGroupGetGenerator (EcGroup g) = doIO $
    withForeignPtr g  $ \gptr ->
    withPointNew gptr $ \r    -> do
        p <- ssl_group_get0_generator gptr
        check $ ssl_point_copy r p
{-# NOINLINE ecGroupGetGenerator #-}

-- | get curve's (prime,a,b)
ecGroupGetCurveGFp :: EcGroup -> (Integer, Integer, Integer)
ecGroupGetCurveGFp (EcGroup g) = doIO $
    withForeignPtr g  $ \gptr  ->
    withBnCtxNew      $ \bnCtx ->
    withBnNew         $ \pPtr  ->
    withBnNew         $ \aPtr  ->
    withBnNew         $ \bPtr  -> do
        check $ ssl_group_get_curve_gfp gptr pPtr aPtr bPtr bnCtx
        (,,) <$> bnToInt pPtr <*> bnToInt aPtr <*> bnToInt bPtr
{-# NOINLINE ecGroupGetCurveGFp #-}

-- | get curve's (polynomial,a,b)
ecGroupGetCurveGF2m :: EcGroup -> (Integer, Integer, Integer)
ecGroupGetCurveGF2m (EcGroup g) = doIO $
    withForeignPtr g  $ \gptr  ->
    withBnCtxNew      $ \bnCtx ->
    withBnNew         $ \pPtr  ->
    withBnNew         $ \aPtr  ->
    withBnNew         $ \bPtr  -> do
        check $ ssl_group_get_curve_gf2m gptr pPtr aPtr bPtr bnCtx
        (,,) <$> bnToInt pPtr <*> bnToInt aPtr <*> bnToInt bPtr
{-# NOINLINE ecGroupGetCurveGF2m #-}

{-
ecPointNew :: EcGroup -> IO EcPoint
ecPointNew (EcGroup fptr) = withForeignPtr fptr $ \gptr ->
    withPointNew gptr (\_ -> return ())
-}

-- | add 2 points together, r = p1 + p2
ecPointAdd :: EcGroup -> EcPoint -> EcPoint -> EcPoint
ecPointAdd (EcGroup g) (EcPoint p1) (EcPoint p2) = doIO $
    withForeignPtr g  $ \gptr  ->
    withForeignPtr p1 $ \p1ptr ->
    withForeignPtr p2 $ \p2ptr ->
    withBnCtxNew      $ \bnCtx ->
    withPointNew gptr $ \r -> check $ ssl_point_add gptr r p1ptr p2ptr bnCtx
{-# NOINLINE ecPointAdd #-}

-- | Add many points together
ecPointsSum :: EcGroup -> [EcPoint] -> EcPoint
ecPointsSum g []               = ecPointInfinity g
ecPointsSum (EcGroup g) ((EcPoint x):xs) = doIO $
    withForeignPtr g       $ \gptr ->
    withForeignPtr x       $ \xptr ->
    withBnCtxNew           $ \bnCtx ->
    withPointDup gptr xptr $ \rptr ->
        forM_ xs $ \(EcPoint p) -> withForeignPtr p $ \pptr -> do
            check $ ssl_point_add gptr rptr rptr pptr bnCtx

-- | compute the doubling of the point p, r = p^2
ecPointDbl :: EcGroup -> EcPoint -> EcPoint
ecPointDbl (EcGroup g) (EcPoint p) = doIO $
    withForeignPtr g  $ \gptr ->
    withForeignPtr p  $ \pptr ->
    withBnCtxNew      $ \bnCtx ->
    withPointNew gptr $ \r -> check $ ssl_point_dbl gptr r pptr bnCtx
{-# NOINLINE ecPointDbl #-}

-- | compute q * m
ecPointMul :: EcGroup
           -> EcPoint -- ^ q
           -> Integer -- ^ m
           -> EcPoint
ecPointMul (EcGroup g) (EcPoint q) m = doIO $
    withForeignPtr g  $ \gptr ->
    withForeignPtr q  $ \qptr ->
    withBnCtxNew      $ \bnCtx ->
    withIntegerAsBN m $ \bnM   ->
    withPointNew gptr $ \r -> check $ ssl_point_mul gptr r nullPtr qptr bnM bnCtx
{-# NOINLINE ecPointMul #-}

-- | compute sum (\(q,m) -> q * m) l
ecPointsMulAndSum :: EcGroup -> [(EcPoint, Integer)] -> EcPoint
ecPointsMulAndSum g []          = ecPointInfinity g
ecPointsMulAndSum (EcGroup g) l = doIO $
    withForeignPtr g  $ \gptr  ->
    withBnCtxNew      $ \bnCtx ->
    withPointNew gptr $ \rptr  ->
    withPointTemp gptr $ \tptr -> do
        check $ ssl_point_set_to_infinity gptr rptr
        forM_ l $ \(EcPoint p,m) -> do
            withForeignPtr p $ \pptr -> withIntegerAsBN m $ \bnM -> check $ ssl_point_mul gptr tptr nullPtr pptr bnM bnCtx
            check $ ssl_point_add gptr rptr rptr tptr bnCtx

-- | Compute the sum of the point to the nth power
--
-- > f [p1,p2,..,pi] n = p1 * (n ^ 0) + p2 * (n ^ 1) + .. + pi * (n ^ i-1)
ecPointsMulOfPowerAndSum :: EcGroup -> [EcPoint] -> Integer -> EcPoint
ecPointsMulOfPowerAndSum g [] _               = ecPointInfinity g
ecPointsMulOfPowerAndSum (EcGroup g) l startn = doIO $
    withForeignPtr g       $ \gptr  ->
    withBnCtxNew           $ \bnCtx ->
    withIntegerAsBN startn $ \n     ->
    withBnNew              $ \nIter ->
    withBnNew              $ \gMod  ->
    withPointNew gptr      $ \rptr  ->
    withPointTemp gptr     $ \tptr  -> do
        check $ ssl_group_get_order gptr gMod bnCtx
        check $ ssl_bn_one nIter
        start gptr gMod bnCtx n nIter rptr tptr
  where
    start gptr gMod bnCtx n nIter rptr tptr = loop l
      where
        loop []     = return ()
        loop (EcPoint x:xs) = do
            -- r += x * current-n
            withForeignPtr x $ \xptr -> check $ ssl_point_mul gptr tptr nullPtr xptr nIter bnCtx
            check $ ssl_point_add gptr rptr rptr tptr bnCtx
            -- nIter = nIter * n
            check $ ssl_bn_mod_mul nIter nIter n gMod bnCtx
            loop xs

-- | compute generator * n + q * m
ecPointMulWithGenerator :: EcGroup
                        -> Integer -- ^ n
                        -> EcPoint -- ^ q
                        -> Integer -- ^ m
                        -> EcPoint
ecPointMulWithGenerator (EcGroup g) n (EcPoint q) m = doIO $
    withForeignPtr g  $ \gptr ->
    withForeignPtr q  $ \qptr ->
    withBnCtxNew      $ \bnCtx ->
    withIntegerAsBN n $ \bnN   ->
    withIntegerAsBN m $ \bnM   ->
    withPointNew gptr $ \r -> check $ ssl_point_mul gptr r bnN qptr bnM bnCtx
{-# NOINLINE ecPointMulWithGenerator #-}

-- | compute generator * n
ecPointGeneratorMul :: EcGroup -> Integer -> EcPoint
ecPointGeneratorMul (EcGroup g) n = doIO $
    withForeignPtr g  $ \gptr  ->
    withBnCtxNew      $ \bnCtx ->
    withIntegerAsBN n $ \bnN   ->
    withPointNew gptr $ \r     -> check $ ssl_point_mul gptr r bnN nullPtr nullPtr bnCtx
{-# NOINLINE ecPointGeneratorMul #-}

-- | compute the inverse on the curve on the point p, r = p^(-1)
ecPointInvert :: EcGroup -> EcPoint -> EcPoint
ecPointInvert (EcGroup g) (EcPoint p) = doIO $
    withForeignPtr g       $ \gptr ->
    withForeignPtr p       $ \pptr ->
    withBnCtxNew           $ \bnCtx ->
    withPointDup gptr pptr $ \dupptr  ->
        check $ ssl_point_invert gptr dupptr bnCtx
{-# NOINLINE ecPointInvert #-}

ecPointInfinity :: EcGroup -> EcPoint
ecPointInfinity (EcGroup g) = doIO $
    withForeignPtr g  $ \gptr  ->
    withPointNew gptr $ \r     ->
        check $ ssl_point_set_to_infinity gptr r
{-# NOINLINE ecPointInfinity #-}

-- | get if the point is at infinity
ecPointIsAtInfinity :: EcGroup -> EcPoint -> Bool
ecPointIsAtInfinity (EcGroup g) (EcPoint p) = doIO $
    withForeignPtr g $ \gptr ->
    withForeignPtr p $ \pptr ->
    ((==) 1 <$> ssl_point_is_at_infinity gptr pptr)
{-# NOINLINE ecPointIsAtInfinity #-}

-- | get if the point is on the curve
ecPointIsOnCurve :: EcGroup -> EcPoint -> Bool
ecPointIsOnCurve (EcGroup g) (EcPoint p) = doIO $
    withForeignPtr g $ \gptr ->
    withForeignPtr p $ \pptr ->
    withBnCtxNew     $ \bnCtx ->
    ((==) 1 <$> ssl_point_is_on_curve gptr pptr bnCtx)
{-# NOINLINE ecPointIsOnCurve #-}

-- | Create a binary represention of a point using the specific format
ecPointToOct :: B.ByteArray outBytes => EcGroup -> EcPoint -> PointConversionForm -> outBytes
ecPointToOct (EcGroup g) (EcPoint p) pconv = doIO $
    withForeignPtr g $ \gptr  ->
    withForeignPtr p $ \pptr  ->
    withBnCtxNew     $ \bnCtx -> do
        lenRequired <- ssl_point_2oct gptr pptr form nullPtr 0 bnCtx
        B.alloc (fromIntegral lenRequired) $ \buf -> do
            void $ ssl_point_2oct gptr pptr form (castPtr buf) lenRequired bnCtx
  where form = ecPointConversionToC pconv
{-# NOINLINE ecPointToOct #-}

-- | Try to parse a binary representation to a point
ecPointFromOct :: B.ByteArrayAccess inBytes => EcGroup -> inBytes -> Either String EcPoint
ecPointFromOct (EcGroup g) bs = doIO $ do
    (opensslRet,point) <- withForeignPtr g            $ \gptr ->
                          B.withByteArray bs          $ \bsPtr ->
                          withBnCtxNew                $ \bnCtx ->
                          withPointNewWithReturn gptr $ \r ->
                            ssl_point_oct2 gptr r bsPtr (fromIntegral $ B.length bs) bnCtx
    if opensslRet == 1 then return (Right point) else return (Left "invalid point")
{-# NOINLINE ecPointFromOct #-}

ecPointFromJProjectiveGFp :: EcGroup -> (Integer,Integer,Integer) -> EcPoint
ecPointFromJProjectiveGFp (EcGroup g) (x,y,z) = doIO $
    withForeignPtr g    $ \gptr  ->
    withBnCtxNew        $ \bnCtx ->
    withIntegerAsBN x   $ \bnX   ->
    withIntegerAsBN y   $ \bnY   ->
    withIntegerAsBN z   $ \bnZ   ->
    withPointNew gptr   $ \r ->
        check $ ssl_point_set_Jprojective_coordinates_GFp gptr r bnX bnY bnZ bnCtx
{-# NOINLINE ecPointFromJProjectiveGFp #-}

ecPointToJProjectiveGFp :: EcGroup -> EcPoint -> (Integer,Integer,Integer)
ecPointToJProjectiveGFp (EcGroup g) (EcPoint p) = doIO $
    withForeignPtr g  $ \gptr  ->
    withForeignPtr p  $ \pptr  ->
    withBnCtxNew      $ \bnCtx ->
    withBnNew         $ \bnX   ->
    withBnNew         $ \bnY   ->
    withBnNew         $ \bnZ   -> do
        check $ ssl_point_get_Jprojective_coordinates_GFp gptr pptr bnX bnY bnZ bnCtx
        (,,) <$> bnToInt bnX <*> bnToInt bnY <*> bnToInt bnZ
{-# NOINLINE ecPointToJProjectiveGFp #-}

-- | Convert a (x,y) to a point representation on a prime curve.
ecPointFromAffineGFp :: EcGroup -> (Integer, Integer) -> EcPoint
ecPointFromAffineGFp (EcGroup g) (x,y) = doIO $
    withForeignPtr g    $ \gptr  ->
    withBnCtxNew        $ \bnCtx ->
    withIntegerAsBN x   $ \bnX   ->
    withIntegerAsBN y   $ \bnY   ->
    withPointNew gptr   $ \r ->
        check $ ssl_point_set_affine_coordinates_GFp gptr r bnX bnY bnCtx
{-# NOINLINE ecPointFromAffineGFp #-}

-- | Convert a point of a prime curve to affine representation (x,y)
ecPointToAffineGFp :: EcGroup -> EcPoint -> (Integer, Integer)
ecPointToAffineGFp (EcGroup g) (EcPoint p) = doIO $
    withForeignPtr g  $ \gptr  ->
    withForeignPtr p  $ \pptr  ->
    withBnCtxNew      $ \bnCtx ->
    withBnNew         $ \bnX   ->
    withBnNew         $ \bnY   -> do
        check $ ssl_point_get_affine_coordinates_GFp gptr pptr bnX bnY bnCtx
        (,) <$> bnToInt bnX <*> bnToInt bnY
{-# NOINLINE ecPointToAffineGFp #-}

ecPointFromAffineGF2m :: EcGroup -> (Integer, Integer) -> EcPoint
ecPointFromAffineGF2m (EcGroup g) (x,y) = doIO $
    withForeignPtr g    $ \gptr  ->
    withBnCtxNew        $ \bnCtx ->
    withIntegerAsBN x   $ \bnX   ->
    withIntegerAsBN y   $ \bnY   ->
    withPointNew gptr   $ \r ->
        check $ ssl_point_set_affine_coordinates_GF2m gptr r bnX bnY bnCtx
{-# NOINLINE ecPointFromAffineGF2m #-}

ecPointToAffineGF2m :: EcGroup -> EcPoint -> (Integer, Integer)
ecPointToAffineGF2m (EcGroup g) (EcPoint p) = doIO $
    withForeignPtr g  $ \gptr  ->
    withForeignPtr p  $ \pptr  ->
    withBnCtxNew      $ \bnCtx ->
    withBnNew         $ \bnX   ->
    withBnNew         $ \bnY   -> do
        check $ ssl_point_get_affine_coordinates_GF2m gptr pptr bnX bnY bnCtx
        (,) <$> bnToInt bnX <*> bnToInt bnY
{-# NOINLINE ecPointToAffineGF2m #-}

-- | return if a point eq another point
ecPointEq :: EcGroup -> EcPoint -> EcPoint -> Bool
ecPointEq (EcGroup g) (EcPoint p1) (EcPoint p2) = doIO $
    withForeignPtr g  $ \gptr ->
    withForeignPtr p1 $ \ptr1 ->
    withForeignPtr p2 $ \ptr2 ->
    withBnCtxNew      $ \bnCtx ->
        (== 0) <$> ssl_point_cmp gptr ptr1 ptr2 bnCtx
{-# NOINLINE ecPointEq #-}

-- | generate a new key in a specific group
ecKeyGenerateNew :: EcGroup -> IO EcKey
ecKeyGenerateNew (EcGroup g) =
    withForeignPtr g  $ \gptr -> do
        key <- ssl_key_new
        check $ ssl_key_set_group key gptr
        check $ ssl_key_generate_key key
        EcKey <$> newForeignPtr ssl_key_free key

-- | create a key from a group and a private integer and public point keypair
ecKeyFromPair :: EcGroup -> (Integer, EcPoint) -> EcKey
ecKeyFromPair (EcGroup g) (i, (EcPoint p)) = doIO $
    withForeignPtr g  $ \gptr ->
    withForeignPtr p  $ \pptr ->
    withIntegerAsBN i $ \bnI  -> do
        key <- ssl_key_new
        check $ ssl_key_set_group key gptr
        check $ ssl_key_set_private_key key bnI
        check $ ssl_key_set_public_key key pptr
        EcKey <$> newForeignPtr ssl_key_free key

-- | return the private integer and public point of a key
ecKeyToPair :: EcKey -> (Integer, EcPoint)
ecKeyToPair (EcKey k) = doIO $
    withForeignPtr k $ \kptr -> do
        gptr  <- ssl_key_get0_group kptr
        point <- withPointNew gptr $ \r -> do
                    p <- ssl_key_get0_public_key kptr
                    check $ ssl_point_copy r p
        priv <- ssl_key_get0_private_key kptr >>= bnToInt
        return (priv, point)
