module Bindings.YogaSpec where

import Test.Hspec
import Control.Monad.IO.Class
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Control.Exception (bracket)

import Bindings.Yoga
import Bindings.Yoga.Enums

withNode :: MonadIO m => (Ptr C'YGNode -> IO a) -> m a 
withNode f = liftIO $ bracket c'YGNodeNew c'YGNodeFree f

withValue :: MonadIO m => IO (Ptr C'YGValue) -> (Ptr C'YGValue -> IO a) -> m a
withValue m f = liftIO $ bracket m c'YGValueFree f

spec :: Spec
spec = do
    describe "YGNodeStyleGetFlexBasisWrapper" $
        it "wraps YGNodeStyleGetFlexBasis as intended" $ do
            flexBasis <- withNode $ \node -> do
                c'YGNodeStyleSetFlexBasis node 100.0
                withValue (c'YGNodeStyleGetFlexBasisWrapper node) peek
            c'YGValue'value flexBasis `shouldBe` 100.0

    describe "YGNodeStyleGetPositionWrapper" $
        it "wraps YGNodeStyleGetPosition as intended" $ do
            position <- withNode $ \node -> do
                c'YGNodeStyleSetPosition node c'YGEdgeLeft 150.0
                withValue (c'YGNodeStyleGetPositionWrapper node c'YGEdgeLeft) peek
            c'YGValue'value position `shouldBe` 150.0

    describe "YGNodeStyleGetMarginWrapper" $
        it "wraps YGNodeStyleGetMargin as intended" $ do
            margin <- withNode $ \node -> do
                c'YGNodeStyleSetMargin node c'YGEdgeLeft 250.0
                withValue (c'YGNodeStyleGetMarginWrapper node c'YGEdgeLeft) peek
            c'YGValue'value margin `shouldBe` 250.0

    describe "YGNodeStyleGetPaddingWrapper" $
        it "wraps YGNodeStyleGetPadding as intended" $ do
            padding <- withNode $ \node -> do
                c'YGNodeStyleSetPadding node c'YGEdgeLeft 125.0
                withValue (c'YGNodeStyleGetPaddingWrapper node c'YGEdgeLeft) peek
            c'YGValue'value padding `shouldBe` 125.0

    describe "YGNodeStyleGetWidthWrapper" $
        it "wraps YGNodeStyleGetWidth as intended" $ do
            width <- withNode $ \node -> do
                c'YGNodeStyleSetWidth node 125.0
                withValue (c'YGNodeStyleGetWidthWrapper node) peek
            c'YGValue'value width `shouldBe` 125.0

    describe "YGNodeStyleGetHeightWrapper" $
        it "wraps YGNodeStyleGetHeight as intended" $ do
            height <- withNode $ \node -> do
                c'YGNodeStyleSetHeight node 125.0
                withValue (c'YGNodeStyleGetHeightWrapper node) peek
            c'YGValue'value height `shouldBe` 125.0

    describe "YGNodeStyleGetMinWidthWrapper" $
        it "wraps YGNodeStyleGetMinWidth as intended" $ do
            minWidth <- withNode $ \node -> do
                c'YGNodeStyleSetMinWidth node 125.0
                withValue (c'YGNodeStyleGetMinWidthWrapper node) peek
            c'YGValue'value minWidth `shouldBe` 125.0

    describe "YGNodeStyleGetMinHeightWrapper" $
        it "wraps YGNodeStyleGetMinHeight as intended" $ do
            minHeight <- withNode $ \node -> do
                c'YGNodeStyleSetMinHeight node 125.0
                withValue (c'YGNodeStyleGetMinHeightWrapper node) peek
            c'YGValue'value minHeight `shouldBe` 125.0

    describe "YGNodeStyleGetMaxWidthWrapper" $
        it "wraps YGNodeStyleGetMaxWidth as intended" $ do
            maxWidth <- withNode $ \node -> do
                c'YGNodeStyleSetMaxWidth node 125.0
                withValue (c'YGNodeStyleGetMaxWidthWrapper node) peek
            c'YGValue'value maxWidth `shouldBe` 125.0

    describe "YGNodeStyleGetMaxHeightWrapper" $
        it "wraps YGNodeStyleGetMaxHeight as intended" $ do
            maxHeight <- withNode $ \node -> do
                c'YGNodeStyleSetMaxHeight node 125.0
                withValue (c'YGNodeStyleGetMaxHeightWrapper node) peek
            c'YGValue'value maxHeight `shouldBe` 125.0

main :: IO ()
main = hspec spec
