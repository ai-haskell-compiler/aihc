{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE RecordWildCards                          #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeInType                               #-}
{-# LANGUAGE TypeOperators                            #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}


module Tests.Util (
    groupTree
  , CloseEnough(..)
  , generateData
  , withSize
  , dot
  ) where

import           Data.Complex
import           Data.Proxy
import           Data.Type.Equality
import           GHC.TypeLits.Compare
import           GHC.TypeNats
import           Hedgehog
import           Hedgehog.Internal.Property
import           Numeric.Natural
import           Statistics.Transform
import           Test.Tasty
import           Test.Tasty.Hedgehog
import qualified Data.Vector.Sized          as V
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

groupTree :: Group -> TestTree
groupTree Group{..} = testGroup (unGroupName groupName)
                                (map (uncurry go) groupProperties)
  where
    go :: PropertyName -> Property -> TestTree
    go n = testProperty (mkName (unPropertyName n))
    mkName = map deUnderscore . drop (length @[] @Char "prop_")
    deUnderscore '_' = ' '
    deUnderscore c   = c

newtype CloseEnough n = CE { getCE :: V.Vector n Double }
  deriving Show

instance KnownNat n => Eq (CloseEnough n) where
    CE x == CE y = ((d `dot` d) / sqrt ((x `dot` x) * (y `dot` y))) < 0.0001
      where
        d = V.zipWith (-) x y

dot :: Num a => V.Vector n a -> V.Vector n a -> a
dot a b = sum $ V.zipWith (*) a b

withSize
    :: Monad m
    => Range Natural
    -> (forall n. (KnownNat n, 1 <= 2^n) => Proxy n -> PropertyT m a)
    -> PropertyT m a
withSize r f = do
    n <- forAll $ Gen.integral r
    case someNatVal n of
      SomeNat (p :: Proxy n) -> do
        LE Refl <- pure $ Proxy @1 %<=? Proxy @(2^n)
        f p

generateData
    :: KnownNat n
    => Gen (V.Vector (2^n) Double)
generateData = fmap (fmap realPart . ifftSized) . V.generateM $ \i ->
    let i' = recip . (+ 1) . fromIntegral $ i
    in  mkPolar <$> Gen.double (Range.exponentialFloat (i' / 10) i')
                <*> Gen.double (Range.constant (-pi) pi)

ifftSized
    :: V.Vector (2^n) (Complex Double)
    -> V.Vector (2^n) (Complex Double)
ifftSized = V.withVectorUnsafe ifft

