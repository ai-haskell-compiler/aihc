{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Tree.Export.NewickSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed Feb 13 11:01:53 2019.
module ELynx.Tree.Export.NewickSpec
  ( spec,
  )
where

import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as BL
import ELynx.Tools.InputOutput
import ELynx.Tree
import ELynx.Tree.Arbitrary ()
import Test.Hspec
import Test.QuickCheck hiding (label)

samplePhyloByteStringTree :: Tree Phylo Name
samplePhyloByteStringTree = parseByteStringWith (newick Standard) sampleNewickByteString1

sampleNewickByteString1 :: BL.ByteString
sampleNewickByteString1 = "(Aeropyrum0:0.5478645225,(((((((((Arabidopsi:0.0701001024,Oryza_sati:0.0765988261):0.0309636193,Gymnosperm:0.0520325624):0.0338982245,Physcomitr:0.0768008916):0.0895714685,(Chlamydomo:0.1136227755,Dunaliella:0.1406347323):0.1117340620):0.0818876186,Rhodophyta:0.3405656487):0.0363527066,((((((Babesia_bo:0.1646969208,Theileria0:0.1519889486):0.1908081096,Plasmodium:0.3250696762):0.0637865908,(Toxoplasma:0.1153570425,Eimeria000:0.1671916078):0.0980136930):0.0518956330,Cryptospor:0.3175062809):0.1607708388,Ciliophora:0.5687502950):0.0624078848,(Phytophtho:0.2016424948,((Thalassios:0.1202730781,Phaeodacty:0.1290341329):0.1772775509,Phaeophyce:0.1989260715):0.0312359673):0.1154768302):0.0311952864):0.0149160316,(((((((((Candida_al:0.1027755272,Saccharomy:0.1190206560):0.1333487870,Neurospora:0.1977309079):0.0522926266,Schizosacc:0.2019603227):0.0567441011,(Cryptococc:0.1948614959,Ustilago_m:0.1564451295):0.0775729694):0.0323959951,Glomus_int:0.1573670796):0.0194701292,Chytridiom:0.2228415254):0.0384370601,Encephalit:1.4622174644):0.0416231688,(((Drosophila:0.2160627753,(Mammalians:0.1080484094,Tunicates0:0.1739253014):0.0289624371):0.0346633757,Hydrozoa00:0.2058137032):0.0480963050,Monosiga_b:0.3020637584):0.0654894239):0.0380915725,(Dictyostel:0.3453588998,Mastigamoe:0.3844779231):0.0478795653):0.0129578395):1.7592083381,((Archaeoglo:0.5402784445,Methanococ:0.4088567459):0.0993669265,Pyrococcus:0.4058713829):0.1734405968):0.2193511807,Pyrobaculu:0.7507718047):0.1646616482,Sulfolobus:0.5404967897);"

prop_invariant :: Tree Phylo Int -> Bool
prop_invariant t =
  parseByteStringWith (newick Standard) (toNewick t)
    ==
    -- We have to convert Int to ByteString, but we cannot use
    -- arbitrary instances with ByteStrings because many
    -- characters are disallowed and break the Newick string.
    second getName t

spec :: Spec
spec = describe "parseByteStringWith newick $ toNewickPhyloByteString" $ do
  it "should be an invariant for a sample tree" $ do
    let bs = toNewick samplePhyloByteStringTree
    parseByteStringWith
      (newick Standard)
      bs
      `shouldBe` samplePhyloByteStringTree
  it "should be an invariant for a sample tree" $ property prop_invariant
