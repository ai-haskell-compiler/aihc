{-|
Module:      Spec.Text.PrettyPrintSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @pretty@ library.
-}
module Spec.Text.PrettyPrintSpec (main, spec) where

import           Data.Proxy (Proxy(..))

import           Instances.Text.PrettyPrint ()

import           Spec.Utils (genericTextShowSpec, matchesTextShowSpec)

import           Test.Hspec (Spec, describe, hspec, parallel)
-- import           Test.Hspec.QuickCheck (prop)

import           Text.PrettyPrint.HughesPJ (Doc, Mode, Style, TextDetails {-, renderStyle -})
import           Text.PrettyPrint.HughesPJClass (PrettyLevel)
import qualified Text.PrettyPrint.Annotated.HughesPJ as Annot (Doc)
import           Text.PrettyPrint.Annotated.HughesPJ (AnnotDetails, Span)
import qualified Text.PrettyPrint.Annotated.HughesPJClass as Annot (PrettyLevel)
-- import           TextShow (fromString)
import           TextShow.Text.PrettyPrint () -- (renderStyleB)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Doc" $
        matchesTextShowSpec (Proxy :: Proxy Doc)
    -- TODO: Figure out why this randomly stalls forever
--     describe "renderStyleB" $ do
--         prop "has the same output as renderStyle" prop_renderStyle
    describe "Mode" $ do
        let p :: Proxy Mode
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "Style" $ do
        let p :: Proxy Style
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "TextDetails" $ do
        let p :: Proxy TextDetails
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "PrettyLevel" $
        matchesTextShowSpec (Proxy :: Proxy PrettyLevel)
    describe "AnnotDetails Int" $
        matchesTextShowSpec (Proxy :: Proxy (AnnotDetails Int))
    describe "Doc Int (annotated)" $
        matchesTextShowSpec (Proxy :: Proxy (Annot.Doc Int))
    describe "PrettyLevel (annotated)" $
        matchesTextShowSpec (Proxy :: Proxy Annot.PrettyLevel)
    describe "Span Int" $
        matchesTextShowSpec (Proxy :: Proxy (Span Int))

-- | Verifies that the output of 'renderStyle' and 'renderStyleB' coincides.
-- prop_renderStyle :: Style -> Doc -> Expectation
-- prop_renderStyle sty doc = fromString (renderStyle sty doc) `shouldBe` renderStyleB sty doc
