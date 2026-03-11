{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Tree.Import.NewickSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Jan 18 10:14:04 2019.
module ELynx.Tree.Import.NewickSpec
  ( spec,
  )
where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Either
import ELynx.Tools.InputOutput
import ELynx.Tree
import Test.Hspec

-- sampleLabelByteString :: ByteString
-- sampleLabelByteString = "name:0.3"

-- sampleLeaf :: Tree Phylo ByteString
-- sampleLeaf = Node (Phylo (Just 0.3) Nothing) "name" []

-- sampleForestByteString :: ByteString
-- sampleForestByteString = "(l,l,(a,b))"

noPL :: Phylo
noPL = Phylo Nothing Nothing

-- sampleForest :: Forest Phylo ByteString
-- sampleForest =
--   [ Node noPL "l" [],
--     Node noPL "l" [],
--     Node
--       noPL
--       ""
--       [ Node noPL "a" [],
--         Node noPL "b" []
--       ]
--   ]

sampleNewickByteString1 :: BS.ByteString
sampleNewickByteString1 = "(Aeropyrum0:0.5478645225,(((((((((Arabidopsi:0.0701001024,Oryza_sati:0.0765988261):0.0309636193,Gymnosperm:0.0520325624):0.0338982245,Physcomitr:0.0768008916):0.0895714685,(Chlamydomo:0.1136227755,Dunaliella:0.1406347323):0.1117340620):0.0818876186,Rhodophyta:0.3405656487):0.0363527066,((((((Babesia_bo:0.1646969208,Theileria0:0.1519889486):0.1908081096,Plasmodium:0.3250696762):0.0637865908,(Toxoplasma:0.1153570425,Eimeria000:0.1671916078):0.0980136930):0.0518956330,Cryptospor:0.3175062809):0.1607708388,Ciliophora:0.5687502950):0.0624078848,(Phytophtho:0.2016424948,((Thalassios:0.1202730781,Phaeodacty:0.1290341329):0.1772775509,Phaeophyce:0.1989260715):0.0312359673):0.1154768302):0.0311952864):0.0149160316,(((((((((Candida_al:0.1027755272,Saccharomy:0.1190206560):0.1333487870,Neurospora:0.1977309079):0.0522926266,Schizosacc:0.2019603227):0.0567441011,(Cryptococc:0.1948614959,Ustilago_m:0.1564451295):0.0775729694):0.0323959951,Glomus_int:0.1573670796):0.0194701292,Chytridiom:0.2228415254):0.0384370601,Encephalit:1.4622174644):0.0416231688,(((Drosophila:0.2160627753,(Mammalians:0.1080484094,Tunicates0:0.1739253014):0.0289624371):0.0346633757,Hydrozoa00:0.2058137032):0.0480963050,Monosiga_b:0.3020637584):0.0654894239):0.0380915725,(Dictyostel:0.3453588998,Mastigamoe:0.3844779231):0.0478795653):0.0129578395):1.7592083381,((Archaeoglo:0.5402784445,Methanococ:0.4088567459):0.0993669265,Pyrococcus:0.4058713829):0.1734405968):0.2193511807,Pyrobaculu:0.7507718047):0.1646616482,Sulfolobus:0.5404967897);"

sampleNewickByteString2 :: BS.ByteString
sampleNewickByteString2 =
  "(Caenorhabd:0.0176707431,C0briggsae:0.0142817073,(Ancylostom:0.0711440844,(Pristionch:0.1301309005,((Brugia_mal:0.0757534325,Ascaris0su:0.0482660407)1:0.0563924634,(((Meloidogyn:0.1239621893,Heteroderi:0.0987968800)1:0.1136879428,Strongyloi:0.2483437292)1:0.0252467381,(Trichoceph:0.2985037612,((((((Coleoptera:0.0907850846,(Apis0melli:0.0754058285,Hemiptera0:0.1675359618)0.93:0.0085703192)1:0.0146980945,(Siphonapte:0.0556805916,Bombyx0mor:0.0968983509)1:0.0127867903)1:0.0167360185,((Drosophila:0.0492149086,Glossina0m:0.0534390467)1:0.0583462602,Anopheles0:0.0968919941)1:0.0431343553)1:0.0535616453,Crustacea0:0.2247268999)1:0.0252755187,Chelicerat:0.1537491558)1:0.0212497286,((Echinoderm:0.1803896615,(Cephalocho:0.1492264574,(Urochordat:0.2194747834,(Mammalia00:0.0393008407,Actinopter:0.0491700096):0.0858550024)1:0.0157515969)1:0.0132516777)1:0.0203423736,((((((((Neurospora:0.0721607581,Magnaporth:0.0814182810)1:0.0198940548,Gibberella:0.0858192964)1:0.0533872590,Eurotiomyc:0.1058840539)1:0.1266302603,(Candida0al:0.1349957509,Saccharomy:0.1553464572)1:0.1791344287)1:0.0529664967,Schizosacc:0.2550087905)1:0.0723650615,(Ustilago0m:0.2031812772,(Homobasidi:0.1473391802,Cryptococc:0.2070743149)1:0.0347868586)1:0.0790327507)1:0.0727415175,Glomales00:0.1779430068)1:0.0169066667,Chytridiom:0.3028920870)1:0.3311420273)1:0.0278566156)1:0.1049569161)1:0.1366217350)1:0.0171168289)1:0.0345725378)1:0.0542036935)1:0.0879337167)1;"

sampleNewickEmptyByteString :: BS.ByteString
sampleNewickEmptyByteString = "(,(,,),);"

sampleNewickEmpty :: Tree Phylo Name
sampleNewickEmpty =
  Node
    noPL
    ""
    [ Node noPL "" [],
      Node
        noPL
        ""
        [ Node noPL "" [],
          Node noPL "" [],
          Node noPL "" []
        ],
      Node noPL "" []
    ]

sampleTreeNewickRevbayes :: BS.ByteString
sampleTreeNewickRevbayes = "[&R](l[IDL]:0.3[KEYVALPAIRS],r[IDR]:0.4[KEYVALPARIS])[ID]:0.3;"

sampleNewickRevBayesFile :: String
sampleNewickRevBayesFile = "data/NewickRevBayes.tree"

spec :: Spec
spec = do
  -- describe "branchLength" $ do
  --   it "parses a colon and a branch length" $
  --     parse branchLength "" ":13.2"
  --       `shouldParse` Just 13.2

  --   it "returns Nothing if no branch length is given" $
  --     parse branchLength "" ""
  --       `shouldParse` Nothing

  -- describe "name" $ do
  --   it "parses a string of printable characters" $
  --     parse name "" "aName"
  --       `shouldParse` "aName"

  --   it "parses blanks, colons, semicolons, parentheses, and sequare brackets" $
  --     parse name "" "aName bla"
  --       `shouldParse` "aName"

  --   it "allows empty names" $ parse name "" "" `shouldParse` BL.pack ""

  -- describe "node" $ do
  --   it "parses a tree node" $
  --     parse node "" sampleLabelByteString
  --       `shouldParse` sampleLabel

  --   it "parses tree nodes with empty names and branch lengths" $
  --     parse node "" ""
  --       `shouldParse` PhyloLabelSoft "" Nothing Nothing

  -- describe "leaf" $
  --   it "parses a leaf of a tree" $
  --     parse leaf "" sampleLabelByteString
  --       `shouldParse` sampleLeaf

  -- describe "forestP" $
  --   it "parses a set of trees within brackets" $
  --     parse forestP "" sampleForestByteString
  --       `shouldParse` sampleForest

  describe "newick" $ do
    it "parses newick trees" $ do
      parseOnly (newick Standard) sampleNewickByteString1 `shouldSatisfy` isRight
      parseOnly (newick Standard) sampleNewickByteString2 `shouldSatisfy` isRight
    it "parses a weird newick tree without node labels nor branch lengths" $
      parseOnly (newick Standard) sampleNewickEmptyByteString `shouldBe` Right sampleNewickEmpty
  describe "newickRevBayes" $
    it "parses newick trees in RevBayes format" $
      do
        parseOnly (newick RevBayes) sampleTreeNewickRevbayes `shouldSatisfy` isRight
        t2 <- parseFileWith (newick RevBayes) sampleNewickRevBayesFile
        length (leaves t2) `shouldBe` 102
