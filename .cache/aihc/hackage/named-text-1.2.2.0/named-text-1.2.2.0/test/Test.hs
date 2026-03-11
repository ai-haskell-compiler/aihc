{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Data.HashMap.Strict as Map
import           Data.Name
import           Data.Parameterized.Context ( pattern Empty, pattern (:>) )
import           Data.Proxy ( Proxy(Proxy) )
import           Data.String ( IsString(fromString) )
import           Data.Text ( Text )
import qualified Data.Text as T
import           GHC.Exts ( proxy#, IsList(fromList, toList) )
import           GHC.Generics ( Generic )
import qualified Prettyprinter as PP
import           Text.Sayable

import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Checklist
import           Test.Tasty.Hspec
import           Test.Tasty.Runners.AntXML

#ifdef VERSION_aeson
import           Data.Aeson
import           Data.ByteString.Lazy ( ByteString )
import           Data.Name.JSON
#endif


main :: IO ()
main = tests >>= defaultMainWithIngredients (antXMLRunner : defaultIngredients)

tests :: IO TestTree
tests = testGroup "Named" <$> sequence
        [
          testCreate
        , testRender
        , testSemigroup
        , testIsList
        , testConversions
        , testValidNames
        , testSomeNames
        , testHasName
        , testUtilities
#ifdef VERSION_aeson
        , testJSON
#endif
        ]


instance TestShow Text
instance Sayable "test" (Named s n)
         => TestShow (Named s n) where testShow = sez @"test"
instance TestShow (Proxy UTF8) where testShow _ = "Proxy :: \"UTF8\""
instance TestShow (Proxy CaseInsensitive) where testShow _ = "Proxy :: !Case"
instance TestShow (Proxy Secure) where testShow _ = "Proxy :: Secure"
instance TestShow [SomeName] where testShow = testShowList
instance TestShow SomeName where testShow = viewSomeName testShow


testCreate :: IO TestTree
testCreate = testSpec "Named Creation" $
  describe "creation, properties, and extraction of Named" $ do

    it "CR1 creates UTF8 from IsText" $
      withChecklist "CR1" $
      fromText @(Named UTF8 "CR1") ("test text" :: Text)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "test text"
       :> Got "case sensitive text value" (/= "Test Text")
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR1"
       :> Val "style proxy" styleProxy (Proxy :: Proxy "UTF8")
       :> Val "extracted text" nameText ("test text" :: Text)
      )

    it "CR2 creates UTF8 from IsString" $
      withChecklist "CR2" $
      fromString @(Named UTF8 "CR2") ("test string" :: String)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent string value" id "test string"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR2"
       :> Val "style proxy" styleProxy (Proxy :: Proxy "UTF8")
       :> Val "extracted text" nameText ("test string" :: Text)
      )

    it "CR3 creates CaseInsensitive from IsText" $
      withChecklist "CR3" $
      fromText @(Named CaseInsensitive "CR3") ("Test teXT" :: Text)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "tesT tExt"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR3"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitive)
       :> Val "extracted text" nameText ("test text" :: Text)
      )

    it "CR4 creates CaseInsensitive from IsString" $
      withChecklist "CR4" $
      fromString @(Named CaseInsensitive "CR4") ("TEst STring" :: String)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent string value" id "teST sTring"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR4"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitive)
       :> Val "extracted text" nameText ("test string" :: Text)
      )

    it "CR5 creates Secure from IsText" $
      withChecklist "CR5" $
      fromText @(Named Secure "CR5") ("test text" :: Text)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "test text"
       :> Got "case sensitive secure value" (/= "Test Text")
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR5"
       :> Val "style proxy" styleProxy (Proxy :: Proxy Secure)
       :> Val "extracted text" nameText ("te#####xt" :: Text)
       :> Val "security bypass text" secureNameBypass ("test text" :: Text)
      )

    it "CR6 creates Secure from IsString" $
      withChecklist "CR6" $
      fromString @(Named Secure "CR6") ("TEst STring" :: String)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent string value" id "TEst STring"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR6"
       :> Val "style proxy" styleProxy (Proxy :: Proxy Secure)
       :> Val "extracted text" nameText ("TE#######ng" :: Text)
       :> Val "security bypass text" secureNameBypass ("TEst STring" :: Text)
      )

    it "CR7 creates short Secure" $
      withChecklist "CR7" $
      ("z" :: Named Secure "CR7")
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "z"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR7"
       :> Val "style proxy" styleProxy (Proxy :: Proxy Secure)
       :> Val "extracted text" nameText ("########" :: Text)
       :> Val "security bypass text" secureNameBypass ("z" :: Text)
      )


testRender :: IO TestTree
testRender = testSpec "Named Rendering" $
  describe "rendering of Named" $ do

  it "CR10 render UTF8 via Sayable" $
    withChecklist "CR10" $
    ("test text" :: Name "CR10")
    `checkValues`
    (Empty
     :> Val "as sayable" (sez @"test") "CR10 'test text'"
     :> Val "as sayable info" (sez @"info") "test text"
    )

  it "CR11 render UTF8 via Prettyprinter" $
    withChecklist "CR11" $
    fromText @(Named "UTF8" "CR11") ("test of text" :: Text)
    `checkValues`
    (Empty
     :> Val "as pretty" (show . PP.pretty) "CR11 'test of text'"
    )

  it "CR12 render UTF8 via Show" $
    withChecklist "CR12" $
    fromText @(Named "UTF8" "CR12") ("test of text" :: Text)
    `checkValues`
    (Empty
     :> Val "as show" show "CR12 'test of text'"
    )

  it "CR13 render CaseInsensitive via Sayable" $
    withChecklist "CR13" $
    ("tEST TeXt" :: Named CaseInsensitive "CR13")
    `checkValues`
    (Empty
     :> Val "as sayable" (sez @"test") "CR13 «test text»"
     :> Val "as sayable info" (sez @"info") "test text"
    )

  it "CR14 render CaseInsensitive via Prettyprinter" $
    withChecklist "CR14" $
    fromText @(Named CaseInsensitive "CR14") ("test OF text" :: Text)
    `checkValues`
    (Empty
     :> Val "as pretty" (show . PP.pretty) "CR14 «test of text»"
    )

  it "CR15 render CaseInsensitive via Show" $
    withChecklist "CR15" $
    fromText @(Named CaseInsensitive "CR15") ("TEST OF TEXT" :: Text)
    `checkValues`
    (Empty
     :> Val "as show" show "CR15 «test of text»"
    )

  it "CR16 render Secure via Sayable" $
    withChecklist "CR16" $
    ("tEST TeXt" :: Named Secure "CR16")
    `checkValues`
    (Empty
     :> Val "as sayable" (sez @"test") "CR16 'tE#####Xt'"
     :> Val "as sayable info" (sez @"info") "tE#####Xt"
    )

  it "CR17 render Secure via Prettyprinter" $
    withChecklist "CR17" $
    fromText @(Named Secure "CR17") ("test OF text" :: Text)
    `checkValues`
    (Empty
     :> Val "as pretty" (show . PP.pretty) "CR17 'te########xt'"
    )

  it "CR18 render Secure via Show" $
    withChecklist "CR18" $
    fromText @(Named Secure "CR18") ("TEST OF TEXT" :: Text)
    `checkValues`
    (Empty
     :> Val "as show" show "CR18 'TE########XT'"
    )

  it "CR19 render short Secure via Sayable" $
    withChecklist "CR19" $
    ("X" :: Named Secure "CR19")
    `checkValues`
    (Empty
     :> Val "as sayable" (sez @"test") "CR19 '########'"
     :> Val "as sayable info" (sez @"info") "########"
    )


testSemigroup :: IO TestTree
testSemigroup = testSpec "Named Semigroup" $
  describe "semigroup of Named" $ do

  it "CR20 UTF8 semigroup" $
    withChecklist "CR20" $
    (fromText @(Named UTF8 "CR20") ("more test text" :: Text)
     <> " and still more")
    `checkValues`
    (Empty
     :> Val "raw value" id "more test text and still more"
     :> Val "as sayable" (sez @"test") "CR20 'more test text and still more'"
    )

  it "CR21 CaseInsensitive semigroup" $
    withChecklist "CR21" $
    (fromText @(Named CaseInsensitive "CR21") ("mORE teST TexT" :: Text)
     <> " and STILL more")
    `checkValues`
    (Empty
     :> Val "implicitly constructed full form" id "more test text and still more"
     :> Val "as sayable" (sez @"test") "CR21 «more test text and still more»"
    )

  it "CR22 Secure semigroup" $
    withChecklist "CR22" $
    (fromText @(Named Secure "CR22") ("more test text" :: Text)
     <> " and still more")
    `checkValues`
    (Empty
     :> Val "implicitly constructed full form" id "more test text and still more"
     :> Val "as sayable" (sez @"test") "CR22 'mo#########################re'"
    )

testIsList :: IO TestTree
testIsList = testSpec "Named IsList" $
  describe "IsList of Named" $ do

  it "CR30 UTF8 IsList" $
    withChecklist "CR30" $
    (fromList "list of text" :: Name "CR30")
    `checkValues`
    (Empty
     :> Val "matches implicit construction" id "list of text"
     :> Val "as sayable" (sez @"test") "CR30 'list of text'"
     :> Val "as extracted text" nameText "list of text"
     :> Val "as list" toList ['l','i','s','t',' ','o','f',' ','t','e','x','t']
    )

  -- Note: no IsList instance for CaseInsensitive or Secure

testConversions :: IO TestTree
testConversions = testSpec "Named Conversions" $ do

  describe "Named nameOf conversions" $ do
    -- n.b. these tests use the "instance ConvertName" below.

    it "CR40 UTF8 default conversion" $
      withChecklist "CR40" $
      (convertName (fromText "list of text" :: Name "CR40") :: Name "CR40-2")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "list of text"
       :> Val "as sayable" (sez @"test") "CR40-2 'list of text'"
       :> Val "as extracted text" nameText "list of text"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR40-2"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

    it "CR41 UTF8 explicit conversion" $
      withChecklist "CR41" $
      (convertName (fromText "list of text" :: Name "CR41") :: Name "CR41-3")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "mjtu!pg!ufyu"
       :> Val "as sayable" (sez @"test") "CR41-3 'mjtu!pg!ufyu'"
       :> Val "as extracted text" nameText "mjtu!pg!ufyu"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR41-3"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

    it "CR42 CaseInsensitive conversion" $
      withChecklist "CR42" $
      (convertName (fromText "biT OF teXt" :: Named CaseInsensitive "CR42")
       :: Named CaseInsensitive "CR42 new")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "bit of text"
       :> Val "as sayable" (sez @"test") "CR42 new «bit of text»"
       :> Val "as extracted text" nameText "bit of text"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR42 new"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitive)
      )

    it "CR43 Secure conversion" $
      withChecklist "CR43" $
      (convertName (fromText "hidden text" :: Named Secure "CR43")
       :: Named Secure "CR43 again")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "hidden text"
       :> Val "as sayable" (sez @"test") "CR43 again 'hi#######xt'"
       :> Val "as extracted text" nameText "hi#######xt"
       :> Val "security bypass extraction" secureNameBypass ("hidden text" :: Text)
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR43 again"
       :> Val "style proxy" styleProxy (Proxy :: Proxy Secure)
      )

  describe "Named style conversions" $ do
    -- n.b. these tests use the "instance ConvertNameStyle" below.

    it "CR44 UTF8->CaseInsensitive default conversion" $
      withChecklist "CR44" $
      (convertStyle (fromText "Some TEXT" :: Name "CR44")
        :: Named CaseInsensitive "CR44")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "some text"
       :> Val "as sayable" (sez @"test") "CR44 «some text»"
       :> Val "as extracted text" nameText "some text"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR44"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitive)
      )

    it "CR45 UTF8->Secure default conversion" $
      withChecklist "CR45" $
      (convertStyle (fromText "Some TEXT" :: Name "CR45") :: Named Secure "CR45")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "Some TEXT"
       :> Val "as sayable" (sez @"test") "CR45 'So#####XT'"
       :> Val "as extracted text" nameText "So#####XT"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR45"
       :> Val "style proxy" styleProxy (Proxy :: Proxy Secure)
      )

    it "CR46 CaseInsensitive->UTF8 default conversion" $
      withChecklist "CR46" $
      (convertStyle (fromText "Some TEXT" :: Named CaseInsensitive "CR46")
        :: Named UTF8 "CR46")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "some text"
       :> Val "as sayable" (sez @"test") "CR46 'some text'"
       :> Val "as extracted text" nameText "some text"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR46"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

    it "CR47 Secure->UTF8 default conversion" $
      withChecklist "CR47" $
      (convertStyle (fromText "Some TEXT" :: Named Secure "CR47") :: Name "CR47")
      `checkValues`
      (Empty
       -- note here that the default only gets the masked form!
       :> Val "matches implicit construction" id "So#####XT"
       :> Val "as sayable" (sez @"test") "CR47 'So#####XT'"
       :> Val "as extracted text" nameText "So#####XT"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR47"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

    it "CR48 Secure->UTF8 implicit bypass conversion" $
      withChecklist "CR48" $
      (convertStyle (fromText "Some TEXT" :: Named Secure "CR48") :: Name "CR48")
      `checkValues`
      (Empty
       -- note here that the default only gets the masked form!
       :> Val "matches implicit construction" id "Some TEXT"
       :> Val "as sayable" (sez @"test") "CR48 'Some TEXT'"
       :> Val "as extracted text" nameText "Some TEXT"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR48"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

instance ConvertName UTF8 "CR40" "CR40-2"
instance ConvertName UTF8 "CR41" "CR41-3" where
  convertName = fromText . T.map succ . nameText
instance ConvertName CaseInsensitive "CR42" "CR42 new"
instance ConvertName Secure "CR43" "CR43 again" where
  convertName = fromText . secureNameBypass

instance ConvertNameStyle UTF8 Secure "CR45"
instance ConvertNameStyle CaseInsensitive UTF8 "CR46"
instance ConvertNameStyle Secure UTF8 "CR47"
instance ConvertNameStyle Secure UTF8 "CR48" where
  convertStyle = fromText . secureNameBypass


testValidNames :: IO TestTree
testValidNames = testSpec "Named ValidNames" $
  describe "ValidNames" $ do
    -- n.b. Uses the validateName function below.

  it "CR50 Valid Names" $
    withChecklist "CR50" $
    (fromText "valid text" :: Name "CR50")
    `checkValues`
    (Empty
     :> Got "valid name for validateName call" validateName
     :> Val "validated text extraction"
      (validName (Proxy :: Proxy '[ "Foo", "CR50", "Other"])) "valid text"
    )

    -- It would be nice to test the failure case for ValidNames, but that's a
    -- compilation failure.

validateName :: ValidNames n '[ "CR50", "CR50.2" ] => Name n -> Bool
validateName = const True


testSomeNames :: IO TestTree
testSomeNames = testSpec "Named SomeName and SomeStyle" $ do
  describe "SomeName" $ do

    it "CR60 SomeName collection and extraction" $
      withChecklist "CR60" $
      ([ SomeName (fromText "text one" :: Name "CR60.1")
       , SomeName (fromText "text two" :: Name "CR60 2")
       , SomeName (fromText "text 3" :: Name "CR60-three")
       ])
      `checkValues`
      (Empty
       :> Val "list length" length 3
       :> Val "@ 0" (viewSomeName (sez @"test") . (!!0)) "CR60.1 'text one'"
       :> Val "@ 1" (viewSomeName (sez @"test") . (!!1)) "CR60 2 'text two'"
       :> Val "@ 2" (viewSomeName (sez @"test") . (!!2)) "CR60-three 'text 3'"
      )

  describe "SomeStyle" $ do

    it "CR61 SomeStyle collection and extraction" $
      withChecklist "CR61" $
      ([ SomeNameStyle ("Regular text." :: Name "CR61")
       , SomeNameStyle ("cASE inSENsitIVE tEXt" :: Named CaseInsensitive "CR61")
       , SomeNameStyle ("secret text" :: Named Secure "CR61")
       ]
      )
      `checkValues`
      (Empty
       :> Val "list length" length 3
       :> Val "val @ 0" (viewSomeNameStyle nameText . (!!0)) "Regular text."
       :> Val "val @ 1" (viewSomeNameStyle nameText . (!!1)) "case insensitive text"
       :> Val "val @ 2" (viewSomeNameStyle nameText . (!!2)) "se#######xt"
       :> Val "nameOf @ 0" (viewSomeNameStyle (\n -> nameOf n proxy#) . (!!0)) "CR61"
       :> Val "nameOf @ 1" (viewSomeNameStyle (\n -> nameOf n proxy#) . (!!1)) "CR61"
       :> Val "nameOf @ 2" (viewSomeNameStyle (\n -> nameOf n proxy#) . (!!2)) "CR61"
      )

instance TestShow [SomeNameStyle "CR61"] where testShow = testShowList
instance TestShow (SomeNameStyle "CR61") where
  testShow = viewSomeNameStyle (\n -> nameOf n proxy# <> ": " <> show (nameText n))


testHasName :: IO TestTree
testHasName = testSpec "HasName" $ do
  describe "HasName Foo" $ do

    it "CR70 can extract UTF8 myName from Foo" $
      myName (Foo "bar" "baz" (Map.fromList [("one","quux"), ("two","brox")]))
      `shouldBe` ("baz" :: Name "principle")

    it "CR71 can extract secure myName from Bar" $
      myName (Bar "baz" "quux") `shouldBe` ("quux" :: Named Secure "second")

data Foo = Foo (Name "prefix") (Name "principle")
               (Map.HashMap (Name "alt-key") (Name "alt"))
         deriving (Eq, Generic, Show)
instance HasName Foo UTF8 "principle" where myName (Foo _ p _) = p

data Bar = Bar (Name "first") (Named Secure "second")
instance HasName Bar Secure "second" where myName (Bar _ s) = s

instance TestShow Foo

testUtilities :: IO TestTree
testUtilities = testSpec "Named utilities" $ do
  describe "Named length" $ do

    it "CR80 can get a UTF8 length" $
      nameLength ("Length of TEXT" :: Name "CR80") `shouldBe` 14

    it "CR81 can get a CaseInsensitive length" $
      nameLength ("Length of TEXT" :: Named CaseInsensitive "CR81") `shouldBe` 14

    it "CR82 can get a Secure length" $
      nameLength ("Length of secure TEXT" :: Named Secure "CR82") `shouldBe` 21

  describe "Named null check" $ do

    it "CR83 can check a null UTF8" $
      nullName ("" :: Name "CR83") `shouldBe` True

    it "CR84 can check a non-null UTF8" $
      nullName ("Not empty" :: Name "CR84") `shouldBe` False

    it "CR85 can check a null CaseInsensitive named" $
      nullName ("" :: Named CaseInsensitive "CR85") `shouldBe` True

    it "CR86 can check a non-null CaseInsensitive named" $
      nullName ("Not empty" :: Named CaseInsensitive "CR86") `shouldBe` False

    it "CR87 can check a null Secure named" $
      nullName ("" :: Name "CR87") `shouldBe` True

    it "CR88 can check a non-null Secure named" $
      nullName ("Not empty" :: Named Secure "CR88") `shouldBe` False

----------------------------------------------------------------------
-- Data.Name.JSON

#ifdef VERSION_aeson

instance TestShow (Proxy JSONStyle) where testShow _ = "Proxy :: \"JSONStyle\""

testJSON :: IO TestTree
testJSON = testSpec "Named JSON style" $ do
  describe "Named JSON" $ do

    it "CR90 can create a JSON Named from Text" $
      withChecklist "CR90" $
      fromText @(Named JSONStyle "CR90") ("test JSON text" :: Text)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "test JSON text"
       :> Got "case sensitive text value" (/= "Test json Text")
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR90"
       :> Val "style proxy" styleProxy (Proxy :: Proxy JSONStyle)
       :> Val "extracted text" nameText ("test JSON text" :: Text)
      )

    it "CR91 render UTF8 via Sayable" $
      withChecklist "CR91" $
      ("test JSON thing" :: Named JSONStyle "CR91")
      `checkValues`
      (Empty
       :> Val "as sayable" (sez @"test") "CR91 'test JSON thing'"
       :> Val "as sayable info" (sez @"info") "test JSON thing"
      )

    it "CR92 render UTF8 via Prettyprinter" $
      withChecklist "CR92" $
      fromText @(Named JSONStyle "CR92") ("test of JSON text" :: Text)
      `checkValues`
      (Empty
       :> Val "as pretty" (show . PP.pretty) "CR92 'test of JSON text'"
      )

    it "CR93 render UTF8 via Show" $
      withChecklist "CR93" $
      fromText @(Named JSONStyle "CR93") ("test of text" :: Text)
      `checkValues`
      (Empty
       :> Val "as show" show "CR93 'test of text'"
      )

    it "CR94 JSON semigroup" $
      withChecklist "CR94" $
      (fromText @(Named JSONStyle "CR94") ("more test JSON text" :: Text) <> "!!")
      `checkValues`
      (Empty
       :> Val "raw value" id "more test JSON text!!"
       :> Val "as sayable" (sez @"test") "CR94 'more test JSON text!!'"
      )

    it "CR95 JSON default conversion" $
      withChecklist "CR95" $
      (convertName (fromText "some JSON text" :: Named JSONStyle "CR95")
       :: Named JSONStyle "CR95 2")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "some JSON text"
       :> Val "as sayable" (sez @"test") "CR95 2 'some JSON text'"
       :> Val "as extracted text" nameText "some JSON text"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR95 2"
       :> Val "style proxy" styleProxy (Proxy :: Proxy JSONStyle)
      )

    it "CR96 UTF8->JSON default conversion" $
      withChecklist "CR96" $
      (convertStyle (fromText "Some JSON-able TEXT" :: Name "CR96")
        :: Named JSONStyle "CR96")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "Some JSON-able TEXT"
       :> Val "as sayable" (sez @"test") "CR96 'Some JSON-able TEXT'"
       :> Val "as extracted text" nameText "Some JSON-able TEXT"
      )

    it "CR97 JSON->UTF8 default conversion" $
      withChecklist "CR97" $
      (convertStyle (fromText "Some un-JSON-able TEXT" :: Named JSONStyle "CR97")
        :: Name "CR97")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "Some un-JSON-able TEXT"
       :> Val "as sayable" (sez @"test") "CR97 'Some un-JSON-able TEXT'"
       :> Val "as extracted text" nameText "Some un-JSON-able TEXT"
      )

    it "CR98 can get a JSON length" $
      nameLength ("Length of JSON TEXT" :: Named JSONStyle "CR98") `shouldBe` 19

    it "CR99 can check a null JSON Named" $
      nullName ("" :: Named JSONStyle "CR99") `shouldBe` True

    it "CR100 can check a non-null JSON Named" $
      nullName ("*" :: Named JSONStyle "CR99") `shouldBe` False

    it "CR101 Named toJSON is valid" $
      withChecklist "CR101" $
      let obj = convertStyle ("Regular TEXT!" :: Name "CR101")
                :: Named JSONStyle "CR101"
      in encode (toJSON obj)
         `checkValues`
         (Empty
          :> Val "encoded" id "\"Regular TEXT!\""
          :> Val "decoded" decode (Just obj)
         )

    it "CR102 toJSON of structure containing Named is valid" $
      withChecklist "CR102" $
      let obj = Foo "start" "your engines"
                (Map.fromList [ ("one","ready")
                              , ("two", "set")
                              , ("three", "go")
                              ])
      in encode (toJSON obj)
         `checkValues`
         (Empty
          :> Val "encoded" id
          "[\"start\",\"your engines\",{\"one\":\"ready\",\"three\":\"go\",\"two\":\"set\"}]"
          :> Val "decoded" decode (Just obj)
         )

    it "CR103 toJSON of record containing Named is valid" $
      withChecklist "CR103" $
      let obj = Info "John Henry" "railroad worker" "hammer master"
      in encode (toJSON obj)
         `checkValues`
         (Empty
          :> Val "encoded" id
          "{\"desc\":\"hammer master\",\"name\":\"John Henry\",\"title\":\"railroad worker\"}"
          :> Val "decoded" decode (Just obj)
         )

    it "CR104 JSON of record containing CaseInsensitive Named is not necessarily round-robin" $
      withChecklist "CR104" $
      let obj = Info "John Henry" "Railroad Worker" "Hammer Master"
      in encode (toJSON obj)
         `checkValues`
         (Empty
          :> Val "encoded" id
          "{\"desc\":\"Hammer Master\",\"name\":\"John Henry\",\"title\":\"railroad worker\"}"
          :> Val "decoded" decode (Just $ obj { title = fromText $ T.toLower $ nameText $ title obj })
         )

data Info = Info { name :: Name "name"
                 , title :: Named CaseInsensitive "title"
                 , desc :: Name "description"
                 }
  deriving (Eq, Generic, Show)
instance ToJSON Info
instance FromJSON Info

instance ToJSON Foo
instance FromJSON Foo

instance ConvertName JSONStyle "CR95" "CR95 2"

instance TestShow ByteString
instance TestShow a => TestShow (Maybe a) where
  testShow = \case
    Nothing -> "NOTHING"
    Just a -> "JUST " <> testShow a
instance TestShow Info
#endif
