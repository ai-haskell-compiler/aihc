-- Do not edit! Automatically created with doctest-extract from src/Data/NonEmpty/Map.hs
{-# LINE 53 "src/Data/NonEmpty/Map.hs" #-}

module Test.Data.NonEmpty.Map where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 54 "src/Data/NonEmpty/Map.hs" #-}
import     qualified Data.NonEmpty.Map as NonEmptyMap
import     qualified Data.NonEmpty as NonEmpty
import     qualified Data.Map as Map
import     qualified Test.QuickCheck as QC

forAllMap     :: (QC.Testable test) => (Map.Map Int String -> test) -> QC.Property
forAllMap     = QC.forAll (fmap Map.fromList QC.arbitrary)

forAllNonEmptyMap     :: (QC.Testable test) => (NonEmptyMap.T Int String -> test) -> QC.Property
forAllNonEmptyMap     = QC.forAll (fmap NonEmptyMap.fromList QC.arbitrary)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.NonEmpty.Map:106: "
{-# LINE 106 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 106 "src/Data/NonEmpty/Map.hs" #-}
          (\k a -> forAllMap $ \m -> Map.insert k a m == NonEmptyMap.flatten (NonEmptyMap.insert k a m))
 DocTest.printPrefix "Data.NonEmpty.Map:110: "
{-# LINE 110 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 110 "src/Data/NonEmpty/Map.hs" #-}
          (\k a -> forAllMap $ \m -> Map.insertWith (++) k a m == NonEmptyMap.flatten (NonEmptyMap.insertWith (++) k a m))
 DocTest.printPrefix "Data.NonEmpty.Map:164: "
{-# LINE 164 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 164 "src/Data/NonEmpty/Map.hs" #-}
          (\k -> forAllNonEmptyMap $ \m -> Map.delete k (NonEmptyMap.flatten m) == NonEmptyMap.delete k m)
 DocTest.printPrefix "Data.NonEmpty.Map:180: "
{-# LINE 180 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 180 "src/Data/NonEmpty/Map.hs" #-}
          (\xs -> Map.fromList (NonEmpty.flatten xs) == NonEmptyMap.flatten (NonEmptyMap.fromList (xs::NonEmpty.T [] (Int,Char))))
 DocTest.printPrefix "Data.NonEmpty.Map:184: "
{-# LINE 184 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 184 "src/Data/NonEmpty/Map.hs" #-}
          (\xs -> Map.fromListWith (++) (NonEmpty.flatten xs) == NonEmptyMap.flatten (NonEmptyMap.fromListWith (++) (xs::NonEmpty.T [] (Int,String))))
 DocTest.printPrefix "Data.NonEmpty.Map:189: "
{-# LINE 189 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 189 "src/Data/NonEmpty/Map.hs" #-}
          (forAllNonEmptyMap $ \m -> NonEmptyMap.fromAscList (NonEmptyMap.toAscList m) == m)
 DocTest.printPrefix "Data.NonEmpty.Map:193: "
{-# LINE 193 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 193 "src/Data/NonEmpty/Map.hs" #-}
          (forAllNonEmptyMap $ \m -> NonEmpty.flatten (NonEmptyMap.toAscList m) == Map.toAscList (NonEmptyMap.flatten m))
 DocTest.printPrefix "Data.NonEmpty.Map:209: "
{-# LINE 209 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 209 "src/Data/NonEmpty/Map.hs" #-}
          (forAllNonEmptyMap $ \xs -> forAllNonEmptyMap $ \ys -> Map.union (NonEmptyMap.flatten xs) (NonEmptyMap.flatten ys) == NonEmptyMap.flatten (NonEmptyMap.union xs ys))
 DocTest.printPrefix "Data.NonEmpty.Map:221: "
{-# LINE 221 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 221 "src/Data/NonEmpty/Map.hs" #-}
          (forAllMap $ \xm -> forAllNonEmptyMap $ \ym -> Map.union xm (NonEmptyMap.flatten ym) == NonEmptyMap.flatten (NonEmptyMap.unionLeft xm ym))
 DocTest.printPrefix "Data.NonEmpty.Map:226: "
{-# LINE 226 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 226 "src/Data/NonEmpty/Map.hs" #-}
          (forAllNonEmptyMap $ \xm -> forAllMap $ \ym -> Map.union (NonEmptyMap.flatten xm) ym == NonEmptyMap.flatten (NonEmptyMap.unionRight xm ym))
 DocTest.printPrefix "Data.NonEmpty.Map:231: "
{-# LINE 231 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 231 "src/Data/NonEmpty/Map.hs" #-}
          (forAllNonEmptyMap $ \xs -> forAllNonEmptyMap $ \ys -> Map.unionWith (++) (NonEmptyMap.flatten xs) (NonEmptyMap.flatten ys) == NonEmptyMap.flatten (NonEmptyMap.unionWith (++) xs ys))
 DocTest.printPrefix "Data.NonEmpty.Map:242: "
{-# LINE 242 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 242 "src/Data/NonEmpty/Map.hs" #-}
          (forAllMap $ \xm -> forAllNonEmptyMap $ \ym -> Map.unionWith (++) xm (NonEmptyMap.flatten ym) == NonEmptyMap.flatten (NonEmptyMap.unionLeftWith (++) xm ym))
 DocTest.printPrefix "Data.NonEmpty.Map:247: "
{-# LINE 247 "src/Data/NonEmpty/Map.hs" #-}
 DocTest.property
{-# LINE 247 "src/Data/NonEmpty/Map.hs" #-}
          (forAllNonEmptyMap $ \xm -> forAllMap $ \ym -> Map.unionWith (++) (NonEmptyMap.flatten xm) ym == NonEmptyMap.flatten (NonEmptyMap.unionRightWith (++) xm ym))
