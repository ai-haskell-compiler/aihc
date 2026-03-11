{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module TypesSpec (spec) where

import           Data.Aeson
import           Data.Aeson.Key
import           Data.Aeson.KeyMap
import           Test.Hspec
import           Test.QuickCheck
import           WLED.Types

instance Arbitrary StatePatch where
  arbitrary = State <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary StateComplete where
  arbitrary = State <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SegmentPatch where
  arbitrary = Segment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SegmentComplete where
  arbitrary = Segment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary NightlightPatch where
  arbitrary = Nightlight <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary NightlightComplete where
  arbitrary = Nightlight <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_associativity :: StatePatch -> StatePatch -> StatePatch -> Bool
prop_associativity a b c = (a <> b) <> c == a <> (b <> c)

prop_identity :: StatePatch -> Bool
prop_identity a = mempty <> a == a && a <> mempty == a

prop :: StateComplete -> StatePatch -> Bool
prop c p = c == append c (diff (append c p) c)

prop_appendIdentity :: StateComplete -> Bool
prop_appendIdentity c = append c mempty == c

prop_appendAssociativity :: StateComplete -> StatePatch -> StatePatch -> Bool
prop_appendAssociativity c p1 p2 = append c (p2 <> p1) == append (append c p1) p2

spec :: Spec
spec = do
  describe "Types" $ do
    describe "StatePatch" $ do
      it "Omit nothing fields" $ do
        let patch :: StatePatch = (mempty :: StatePatch) { stateBri = Just 1 }
        let json :: Either String Value = eitherDecode $ encode patch
        json `shouldBe` Right (Object (fromList [(fromString "bri",Number 1)]))
      it "should satisfy the associativity law" $
        property prop_associativity
      it "should have an identity" $
        property prop_identity
      it "what" $ do
        let p1 :: StatePatch = State {stateOn = Just True, stateBri = Just (-4), stateTransition = Just (-3), statePs = Just (-3), statePl = Just (-3), stateNl = Just (Nightlight {nightlightOn = Just True, nightlightDur = Just (-5), nightlightMode = Just 8, nightlightTbri = Just 4, nightlightRem = Just (-1)}), stateLor = Just 8, stateMainseg = Just 6, stateSeg = Just [Segment {segmentId = Nothing, segmentStart = Just (-1), segmentStop = Nothing, segmentLen = Just (-4), segmentGrp = Nothing, segmentSpc = Nothing, segmentOf = Just (-4), segmentOn = Nothing, segmentFrz = Just True, segmentBri = Just 7, segmentCct = Just (-1), segmentSet = Nothing, segmentCol = Nothing, segmentFx = Just 4, segmentSx = Just 4, segmentIx = Just 7, segmentPal = Just (-7), segmentC1 = Just 2, segmentC2 = Just 5, segmentC3 = Just (-8), segmentSel = Just False, segmentRev = Just False, segmentMi = Just False, segmentO1 = Just True, segmentO2 = Just True, segmentO3 = Nothing, segmentSi = Just 6, segmentM12 = Nothing},Segment {segmentId = Just (-7), segmentStart = Just 5, segmentStop = Just 4, segmentLen = Just (-3), segmentGrp = Just (-4), segmentSpc = Just (-6), segmentOf = Just 4, segmentOn = Just False, segmentFrz = Just False, segmentBri = Just (-5), segmentCct = Just (-3), segmentSet = Just (-6), segmentCol = Nothing, segmentFx = Just (-2), segmentSx = Just (-3), segmentIx = Just 0, segmentPal = Nothing, segmentC1 = Just 4, segmentC2 = Just 0, segmentC3 = Just (-3), segmentSel = Nothing, segmentRev = Just False, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Just True, segmentO3 = Just False, segmentSi = Just 8, segmentM12 = Nothing},Segment {segmentId = Just 5, segmentStart = Just 3, segmentStop = Nothing, segmentLen = Just 0, segmentGrp = Just 5, segmentSpc = Nothing, segmentOf = Just 2, segmentOn = Nothing, segmentFrz = Just True, segmentBri = Nothing, segmentCct = Just 6, segmentSet = Just 2, segmentCol = Nothing, segmentFx = Just 8, segmentSx = Just 4, segmentIx = Just 7, segmentPal = Just 6, segmentC1 = Just 6, segmentC2 = Just 7, segmentC3 = Just 1, segmentSel = Just False, segmentRev = Just True, segmentMi = Just False, segmentO1 = Just False, segmentO2 = Just False, segmentO3 = Just True, segmentSi = Just (-8), segmentM12 = Just (-8)},Segment {segmentId = Just 0, segmentStart = Nothing, segmentStop = Nothing, segmentLen = Just (-3), segmentGrp = Nothing, segmentSpc = Just (-1), segmentOf = Just 8, segmentOn = Just False, segmentFrz = Just False, segmentBri = Nothing, segmentCct = Just (-8), segmentSet = Just (-6), segmentCol = Nothing, segmentFx = Just 7, segmentSx = Just (-5), segmentIx = Just 7, segmentPal = Just (-7), segmentC1 = Nothing, segmentC2 = Just (-1), segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Just False, segmentO1 = Just True, segmentO2 = Just True, segmentO3 = Just True, segmentSi = Just 4, segmentM12 = Nothing},Segment {segmentId = Nothing, segmentStart = Just 3, segmentStop = Just (-1), segmentLen = Just 2, segmentGrp = Nothing, segmentSpc = Just (-8), segmentOf = Just 5, segmentOn = Just True, segmentFrz = Just False, segmentBri = Just 3, segmentCct = Just (-2), segmentSet = Just (-6), segmentCol = Nothing, segmentFx = Just (-3), segmentSx = Just (-6), segmentIx = Nothing, segmentPal = Just (-3), segmentC1 = Just (-3), segmentC2 = Just 8, segmentC3 = Just (-1), segmentSel = Just False, segmentRev = Just True, segmentMi = Nothing, segmentO1 = Just False, segmentO2 = Nothing, segmentO3 = Just False, segmentSi = Nothing, segmentM12 = Just 7},Segment {segmentId = Just (-2), segmentStart = Just 3, segmentStop = Just 5, segmentLen = Just 7, segmentGrp = Just (-2), segmentSpc = Just 4, segmentOf = Just 3, segmentOn = Just True, segmentFrz = Nothing, segmentBri = Just (-6), segmentCct = Just 3, segmentSet = Nothing, segmentCol = Just [[3,7,6,-5,1,4]], segmentFx = Just (-3), segmentSx = Nothing, segmentIx = Just (-2), segmentPal = Nothing, segmentC1 = Just 8, segmentC2 = Nothing, segmentC3 = Just (-1), segmentSel = Just True, segmentRev = Just False, segmentMi = Just False, segmentO1 = Just True, segmentO2 = Just False, segmentO3 = Nothing, segmentSi = Just (-3), segmentM12 = Just 6}]}
        let p2 :: StatePatch = State {stateOn = Just True, stateBri = Just 0, stateTransition = Just 5, statePs = Just (-5), statePl = Just 0, stateNl = Nothing, stateLor = Nothing, stateMainseg = Nothing, stateSeg = Just [Segment {segmentId = Just 8, segmentStart = Just (-6), segmentStop = Just 8, segmentLen = Just 4, segmentGrp = Just 8, segmentSpc = Just (-2), segmentOf = Nothing, segmentOn = Nothing, segmentFrz = Just False, segmentBri = Just (-4), segmentCct = Just 4, segmentSet = Nothing, segmentCol = Just [[-2,0,-1,0,-3,1,-8],[1,5,-2,6,7,-8,6],[-1,-4,-2,-4,0,6,4],[-1,1,-3,-2,-6,-8],[1,7,3,7,6,-2,8],[-2,3,-2],[0,5,-4]], segmentFx = Nothing, segmentSx = Just 4, segmentIx = Just (-4), segmentPal = Just 7, segmentC1 = Just (-2), segmentC2 = Nothing, segmentC3 = Nothing, segmentSel = Just False, segmentRev = Just True, segmentMi = Nothing, segmentO1 = Just False, segmentO2 = Just True, segmentO3 = Just True, segmentSi = Just 2, segmentM12 = Nothing},Segment {segmentId = Just (-1), segmentStart = Just (-2), segmentStop = Just 2, segmentLen = Just 0, segmentGrp = Just 1, segmentSpc = Just 5, segmentOf = Just 1, segmentOn = Just False, segmentFrz = Just True, segmentBri = Just 1, segmentCct = Just 6, segmentSet = Just (-5), segmentCol = Just [[4,3,-6,-8,-6,4,8,-2]], segmentFx = Nothing, segmentSx = Just 2, segmentIx = Just 5, segmentPal = Just (-5), segmentC1 = Just 8, segmentC2 = Just 8, segmentC3 = Just 2, segmentSel = Just True, segmentRev = Nothing, segmentMi = Just False, segmentO1 = Just False, segmentO2 = Just False, segmentO3 = Nothing, segmentSi = Just (-1), segmentM12 = Just (-6)},Segment {segmentId = Just 5, segmentStart = Nothing, segmentStop = Just 4, segmentLen = Just (-4), segmentGrp = Just (-6), segmentSpc = Nothing, segmentOf = Just (-2), segmentOn = Just False, segmentFrz = Just True, segmentBri = Just 8, segmentCct = Just (-4), segmentSet = Just (-4), segmentCol = Just [], segmentFx = Just 0, segmentSx = Just (-5), segmentIx = Nothing, segmentPal = Just (-4), segmentC1 = Just 1, segmentC2 = Just 8, segmentC3 = Just (-1), segmentSel = Just False, segmentRev = Just True, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Nothing, segmentO3 = Just True, segmentSi = Nothing, segmentM12 = Just (-3)}]}
        let p3 :: StatePatch = State {stateOn = Just True, stateBri = Just (-3), stateTransition = Nothing, statePs = Just (-8), statePl = Just (-1), stateNl = Just (Nightlight {nightlightOn = Just True, nightlightDur = Just 2, nightlightMode = Just 0, nightlightTbri = Just 1, nightlightRem = Just 1}), stateLor = Just (-8), stateMainseg = Just 0, stateSeg = Just [Segment {segmentId = Just 5, segmentStart = Just (-8), segmentStop = Just 3, segmentLen = Nothing, segmentGrp = Just 3, segmentSpc = Just (-6), segmentOf = Just (-1), segmentOn = Nothing, segmentFrz = Nothing, segmentBri = Just 5, segmentCct = Nothing, segmentSet = Just 3, segmentCol = Nothing, segmentFx = Just 3, segmentSx = Just (-4), segmentIx = Nothing, segmentPal = Nothing, segmentC1 = Just 3, segmentC2 = Just (-3), segmentC3 = Just (-5), segmentSel = Nothing, segmentRev = Just True, segmentMi = Nothing, segmentO1 = Just True, segmentO2 = Just False, segmentO3 = Nothing, segmentSi = Just (-5), segmentM12 = Nothing},Segment {segmentId = Just (-3), segmentStart = Just 3, segmentStop = Just 7, segmentLen = Just 7, segmentGrp = Just (-8), segmentSpc = Just (-2), segmentOf = Just 3, segmentOn = Just False, segmentFrz = Nothing, segmentBri = Just 2, segmentCct = Nothing, segmentSet = Nothing, segmentCol = Just [[4,4],[7,1,1]], segmentFx = Just 8, segmentSx = Just (-7), segmentIx = Just (-8), segmentPal = Just 7, segmentC1 = Just (-2), segmentC2 = Just (-8), segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Just True, segmentO1 = Just True, segmentO2 = Nothing, segmentO3 = Just True, segmentSi = Just (-2), segmentM12 = Just (-1)},Segment {segmentId = Just (-8), segmentStart = Nothing, segmentStop = Nothing, segmentLen = Nothing, segmentGrp = Just 3, segmentSpc = Just (-7), segmentOf = Nothing, segmentOn = Just False, segmentFrz = Just False, segmentBri = Just 5, segmentCct = Nothing, segmentSet = Just 3, segmentCol = Just [], segmentFx = Just 5, segmentSx = Just 3, segmentIx = Just 3, segmentPal = Nothing, segmentC1 = Nothing, segmentC2 = Just 4, segmentC3 = Just 1, segmentSel = Nothing, segmentRev = Just True, segmentMi = Just True, segmentO1 = Just True, segmentO2 = Just False, segmentO3 = Just False, segmentSi = Just (-5), segmentM12 = Just 7},Segment {segmentId = Just (-6), segmentStart = Just 1, segmentStop = Nothing, segmentLen = Just (-3), segmentGrp = Just 2, segmentSpc = Just 0, segmentOf = Just (-7), segmentOn = Just True, segmentFrz = Just False, segmentBri = Just 7, segmentCct = Just 7, segmentSet = Just 6, segmentCol = Just [[],[1,6,8],[-6,-8,-7,-8,3,1],[-1,2,-2,8,-6]], segmentFx = Just 3, segmentSx = Just 2, segmentIx = Just 6, segmentPal = Nothing, segmentC1 = Just (-1), segmentC2 = Nothing, segmentC3 = Just 0, segmentSel = Nothing, segmentRev = Just False, segmentMi = Nothing, segmentO1 = Just True, segmentO2 = Just True, segmentO3 = Just True, segmentSi = Just 4, segmentM12 = Just (-3)},Segment {segmentId = Nothing, segmentStart = Just 4, segmentStop = Nothing, segmentLen = Just 6, segmentGrp = Just 6, segmentSpc = Just (-7), segmentOf = Just (-6), segmentOn = Just True, segmentFrz = Nothing, segmentBri = Nothing, segmentCct = Just (-5), segmentSet = Just 1, segmentCol = Nothing, segmentFx = Just 1, segmentSx = Just 0, segmentIx = Just (-1), segmentPal = Nothing, segmentC1 = Just (-5), segmentC2 = Just (-7), segmentC3 = Just (-8), segmentSel = Nothing, segmentRev = Just False, segmentMi = Just False, segmentO1 = Just False, segmentO2 = Just True, segmentO3 = Just False, segmentSi = Just 2, segmentM12 = Nothing},Segment {segmentId = Nothing, segmentStart = Just 0, segmentStop = Just 5, segmentLen = Just 5, segmentGrp = Just (-7), segmentSpc = Just (-3), segmentOf = Just 2, segmentOn = Just False, segmentFrz = Just False, segmentBri = Just 8, segmentCct = Just (-7), segmentSet = Just (-8), segmentCol = Just [], segmentFx = Just 8, segmentSx = Nothing, segmentIx = Just 3, segmentPal = Just 0, segmentC1 = Just 0, segmentC2 = Just (-5), segmentC3 = Just 0, segmentSel = Just True, segmentRev = Just False, segmentMi = Just True, segmentO1 = Just True, segmentO2 = Just True, segmentO3 = Just True, segmentSi = Just 4, segmentM12 = Just (-6)},Segment {segmentId = Nothing, segmentStart = Just (-8), segmentStop = Nothing, segmentLen = Just 3, segmentGrp = Just 1, segmentSpc = Just 3, segmentOf = Just 5, segmentOn = Just True, segmentFrz = Just True, segmentBri = Nothing, segmentCct = Just (-4), segmentSet = Just 1, segmentCol = Just [[6,-1,3,-4,3],[-5,-6,8,4,4,-2],[-2,-1,6,-1,0],[4,-6,6,2,-2],[4],[1,-6,-2,2,4,2]], segmentFx = Nothing, segmentSx = Nothing, segmentIx = Just (-2), segmentPal = Just (-5), segmentC1 = Just 6, segmentC2 = Just 5, segmentC3 = Just (-1), segmentSel = Just True, segmentRev = Just True, segmentMi = Just False, segmentO1 = Just True, segmentO2 = Just True, segmentO3 = Just True, segmentSi = Just (-6), segmentM12 = Just 1},Segment {segmentId = Just 6, segmentStart = Just 2, segmentStop = Just 5, segmentLen = Nothing, segmentGrp = Just (-1), segmentSpc = Nothing, segmentOf = Just (-4), segmentOn = Just False, segmentFrz = Just True, segmentBri = Just 2, segmentCct = Nothing, segmentSet = Just (-7), segmentCol = Just [[-6,-8,-3,-4,-6,0,-6],[-7,2,-3,8],[1,1,-4,-8,4],[0,3,-5],[8,-3,2,-7,1,-5,-2,2],[-8,-2,-4],[3,3,-3,-3,-3,-1,8,4]], segmentFx = Just 3, segmentSx = Nothing, segmentIx = Just (-7), segmentPal = Just (-5), segmentC1 = Just 2, segmentC2 = Just (-5), segmentC3 = Just 0, segmentSel = Nothing, segmentRev = Just True, segmentMi = Just True, segmentO1 = Just False, segmentO2 = Just True, segmentO3 = Just True, segmentSi = Just (-6), segmentM12 = Nothing}]}
        (p1 <> p2) <> p3 `shouldBe` p1 <> (p2 <> p3)
    describe "StateComplete" $ do
      it "appending patches should satisfy the associativity law" $
        property prop_appendAssociativity
      it "appending patches should have an identity" $
        property prop_appendIdentity
      it "do not append lists" $ do
        let complete :: StateComplete = State True 255 0 0 0 (Nightlight False 0 0 0 0) 0 0 []
        let patch :: StatePatch = (mempty :: StatePatch) { stateSeg = Just [(mempty :: SegmentPatch) { segmentBri = Just 255 }] }
        append complete patch `shouldBe` complete
      it "append/diff" $
        property prop
      it "delete_segments" $ do
        let c1 :: StateComplete = State {stateOn = False, stateBri = 1, stateTransition = 1, statePs = 1, statePl = 0, stateNl = Nightlight {nightlightOn = True, nightlightDur = 0, nightlightMode = -1, nightlightTbri = -1, nightlightRem = -1}, stateLor = 0, stateMainseg = -1, stateSeg = [Segment {segmentId = 0, segmentStart = 1, segmentStop = -1, segmentLen = -1, segmentGrp = 0, segmentSpc = 1, segmentOf = 0, segmentOn = True, segmentFrz = True, segmentBri = -1, segmentCct = 1, segmentSet = 0, segmentCol = [], segmentFx = 0, segmentSx = 1, segmentIx = -1, segmentPal = 0, segmentC1 = 0, segmentC2 = -1, segmentC3 = 0, segmentSel = False, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = True, segmentO3 = False, segmentSi = -1, segmentM12 = -1}]}
        let c2 :: StateComplete = State {stateOn = False, stateBri = 1, stateTransition = 1, statePs = 1, statePl = 0, stateNl = Nightlight {nightlightOn = True, nightlightDur = 0, nightlightMode = -1, nightlightTbri = -1, nightlightRem = -1}, stateLor = 0, stateMainseg = -1, stateSeg = []}
        let clearSegment = fmap (fmap segmentStop) <$> stateSeg $ diff c1 c2
        clearSegment `shouldBe` Just [Just 0]
