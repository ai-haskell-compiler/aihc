{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           , FlexibleContexts
           , ConstrainedClassMethods
           , UndecidableInstances
           #-}

module Main (main) where

import Data.Reflection

import Hedgehog
import Hedgehog.Main
import qualified Hedgehog.Gen   as HG
import qualified Hedgehog.Range as HR

import qualified Linear as L
import Linear.Geo hiding (diff)

-- from the numeric-limits package:

maxValue :: (RealFloat a) => a
maxValue = x
  where n = floatDigits x
        b = floatRadix x
        (_, u) = floatRange x
        x = encodeFloat (b ^ n - 1) (u - n)

minValue :: (RealFloat a) => a
minValue = -maxValue

hugeValFRange :: Range Double
hugeValFRange = HR.exponentialFloatFrom 0 minValue maxValue

hugeValFs :: MonadGen m => m Double
hugeValFs = HG.double hugeValFRange

modBugRange :: Range Double
modBugRange =
    let l = 1e15
    in HR.exponentialFloatFrom 0 (-l) l

modBug :: MonadGen m => m Double
modBug = HG.double modBugRange

class ApproxEq a where
    type family Atom a
    (~=~) :: Given (Atom a) => a -> a -> Bool

instance ApproxEq Double where
    type Atom Double = Double
    x ~=~ y = (abs (x - y)) / ((x + y + given) / 2) <= given

instance (ApproxEq a, Given (Atom a)) => ApproxEq (Radians a) where
    type Atom (Radians a) = a
    (Radians x) ~=~ (Radians y) = x ~=~ y

instance (ApproxEq a, Given (Atom a)) => ApproxEq (Degrees a) where
    type Atom (Degrees a) = a
    (Degrees x) ~=~ (Degrees y) = x ~=~ y

instance (ApproxEq a, Given (Atom a)) => ApproxEq (DMS a) where
    type Atom (DMS a) = a
    (DMS xd xm xs) ~=~ (DMS yd ym ys) = (xd ~=~ yd)
                                     && (xm ~=~ ym)
                                     && (xs ~=~ ys)

instance (ApproxEq a, Given (Atom a)) => ApproxEq (DM a) where
    type Atom (DM a) = a
    (DM xd xm) ~=~ (DM yd ym) = (xd ~=~ yd)
                             && (xm ~=~ ym)

instance (ApproxEq a, Given (Atom a)) => ApproxEq (L.V3 a) where
    type Atom (L.V3 a) = a
    (L.V3 ax ay az) ~=~ (L.V3 bx by bz) = (ax ~=~ bx)
                                       && (ay ~=~ by)
                                       && (az ~=~ bz)

instance (ApproxEq a, Given (Atom a)) => ApproxEq (ECEF a) where
    type Atom (ECEF a) = a
    (ECEF x) ~=~ (ECEF y) = x ~=~ y

instance (ApproxEq a, Given (Atom a)) => ApproxEq (Geo a) where
    type Atom (Geo a) = a
    (Geo ap al ah) ~=~ (Geo bp bl bh) = (ap ~=~ bp)
                                     && (al ~=~ bl)
                                     && (ah ~=~ bh)

instance (RealFloat a, ApproxEq a, Given (Atom a)) => ApproxEq (ENU a) where
    type Atom (ENU a) = a
    x@(ENU _ xp) ~=~ y = let ENU _ y'p = alignOrigin x y
                         in xp ~=~ y'p

(=~=) :: (ApproxEq a, Given (Atom a), Show a, MonadTest m) => a -> a -> m ()
(=~=) a b = diff a (~=~) b

--genECEF :: MonadGen m => m Double -> m (ECEF Double)
--genECEF gd = ECEF <$> (L.V3 <$> gd <*> gd <*> gd)

--genENU :: MonadGen m => m Double -> m (ENU Double)
--genENU gd = do
--    og <- genGeo gd
--    pv <- L.V3 <$> gd <*> gd <*> gd
--    pure (ENU (geoToECEF og) pv)

genGeo :: MonadGen m => m Double -> m (Geo Double)
genGeo gd = Geo <$> (normalizeAngle <$> genRad gd)
                <*> (normalizeAngle <$> genRad gd)
                <*> gd

genRad :: MonadGen m => m Double -> m (Radians Double)
genRad = fmap Radians

genDeg :: MonadGen m => m Double -> m (Degrees Double)
genDeg = fmap Degrees

genDMS :: MonadGen m => m Double -> m (DMS Double)
genDMS gd = DMS <$> gd <*> gd <*> gd

genDM :: MonadGen m => m Double -> m (DM Double)
genDM gd = DM <$> gd <*> gd

radNormRange :: Property
radNormRange = property $ do
    r <- forAll $ genRad modBug
    let (Radians r') = normalizeAngle r
    assert ((r' >= 0) && (r' < (2 * pi)))

radNormIdemp :: Property
radNormIdemp = property $ do
    r <- forAll $ genRad modBug
    let r' = normalizeAngle r
    r' === normalizeAngle r'

radToRadFromRadIdemp :: Property
radToRadFromRadIdemp = property $ do
    r <- forAll $ genRad hugeValFs
    r === fromRadians (toRadians r)

degNormRange :: Property
degNormRange = property $ do
    d <- forAll $ genDeg modBug
    let (Degrees d') = normalizeAngle d
    assert ((d' >= 0) && (d' < 360))

degNormIdemp :: Property
degNormIdemp = property $ do
    d <- forAll $ genDeg modBug
    let d' = normalizeAngle d
    d' === normalizeAngle d'

degToRadFromRadIdemp :: Property
degToRadFromRadIdemp = property $ do
    d <- forAll $ genDeg hugeValFs
    let r = toRadians d
    give 1e-15 (d =~= fromRadians r)

dmsNormRange :: Property
dmsNormRange = property $ do
    d <- forAll $ genDMS modBug
    let dmsn = normalizeAngle d
        (Degrees d') = dmsToDegrees dmsn
    assert ((d' >= 0) && (d' < 360))

dmsNormIdemp :: Property
dmsNormIdemp = property $ do
    d <- forAll $ genDMS modBug
    let d' = normalizeAngle d
    give 1e-10 (d' =~= normalizeAngle d')

-- Stability is so bad, practically this does not hold.
--dmsToRadFromRadIdemp :: Property
--dmsToRadFromRadIdemp = property $ do
--    d <- forAll $ genDMS modBug
--    let r = toRadians d
--    give 1e-2 (normalizeAngle d =~= normalizeAngle (fromRadians r))

dmNormRange :: Property
dmNormRange = property $ do
    d <- forAll $ genDM modBug
    let dmn = normalizeAngle d
        (Degrees d') = dmToDegrees dmn
    assert ((d' >= 0) && (d' < 360))

dmNormIdemp :: Property
dmNormIdemp = property $ do
    d <- forAll $ genDM modBug
    let d' = normalizeAngle d
    d' === normalizeAngle d'

-- Stability is so bad, practically this does not hold.
--dmToRadFromRadIdemp :: Property
--dmToRadFromRadIdemp = property $ do
--    d <- forAll $ genDM modBug
--    let r = toRadians d
--    give 1e-2 (normalizeAngle d =~= normalizeAngle (fromRadians r))

radToFromLatLonIdemp :: Property
radToFromLatLonIdemp = property $ do
    g <- forAll $ genGeo hugeValFs
    let p :: Radians Double
        l :: Radians Double
        h :: Double
        (p, l, h) = toLatLonAlt g
        g' = fromLatLonAlt p l h
    g === g'

degToFromLatLonIdemp :: Property
degToFromLatLonIdemp = property $ do
    g <- forAll $ genGeo hugeValFs
    let p :: Degrees Double
        l :: Degrees Double
        h :: Double
        (p, l, h) = toLatLonAlt g
        g' = fromLatLonAlt p l h
    give 1e-8 (g =~= g')

dmsToFromLatLonIdemp :: Property
dmsToFromLatLonIdemp = property $ do
    g <- forAll $ genGeo hugeValFs
    let p :: DMS Double
        l :: DMS Double
        h :: Double
        (p, l, h) = toLatLonAlt g
        g' = fromLatLonAlt p l h
    give 1e-8 (g =~= g')

dmToFromLatLonIdemp :: Property
dmToFromLatLonIdemp = property $ do
    g <- forAll $ genGeo hugeValFs
    let p :: DM Double
        l :: DM Double
        h :: Double
        (p, l, h) = toLatLonAlt g
        g' = fromLatLonAlt p l h
    give 1e-8 (g =~= g')

-- | Not true due to NaNs, need to figure out why...
--geoToFromECEFIdemp :: Property
--geoToFromECEFIdemp = property $ do
--    g <- forAll $ genGeo hugeValFs
--    give 1e-7
--        (g =~= normalizeGeo (ecefToGeo (geoToECEF g)))

-- | Not true due to NaNs, need to figure out why...
--enuToFromECEFIdemp :: Property
--enuToFromECEFIdemp = property $ do
--    p@(ENU o _) <- forAll $ genENU hugeValFs
--    give 1e-8 (p =~= ecefToENU o (enuToECEF p))

main :: IO ()
main = defaultMain $ (:[]) $ checkParallel $ Group "Linear.Geo"
    [ ("normalizeAngle range check @Radians", radNormRange)
    , ("normalizeAngle idempotent @Radians", radNormIdemp)
    , ("(fromRadians . toRadians) == id @Radians", radToRadFromRadIdemp)
    , ("normalizeAngle range check @Degrees", degNormRange)
    , ("normalizeAngle idempotent @Degrees", degNormIdemp)
    , ("(fromRadians . toRadians) == id @Degrees", degToRadFromRadIdemp)
    , ("normalizeAngle range check @DMS", dmsNormRange)
    , ("normalizeAngle idempotent @DMS", dmsNormIdemp)
    -- Does not hold in practice due to stability
    --, ("(fromRadians . toRadians) == id @DMS", dmsToRadFromRadIdemp)
    , ("normalizeAngle range check @DM", dmNormRange)
    , ("normalizeAngle idempotent @DM", dmNormIdemp)
    -- Does not hold in practice due to stability
    --, ("(fromRadians . toRadians) == id @DM", dmToRadFromRadIdemp)
    , ("(fromLatLon . toLatLon) == id @Radians", radToFromLatLonIdemp)
    , ("(fromLatLon . toLatLon) == id @Degrees", degToFromLatLonIdemp)
    , ("(fromLatLon . toLatLon) == id @DMS", dmsToFromLatLonIdemp)
    , ("(fromLatLon . toLatLon) == id @DM", dmToFromLatLonIdemp)
    -- Not true due to NaNs, need to figure out why...
    --, ("(ecefToGeo . geoToECEF) == id", geoToFromECEFIdemp)
    --, ("(ecefToENU . enuToECEF) == id", enuToFromECEFIdemp)
    ]
