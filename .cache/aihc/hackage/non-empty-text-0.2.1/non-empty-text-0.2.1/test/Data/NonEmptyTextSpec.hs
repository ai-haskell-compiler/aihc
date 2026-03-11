{-# OPTIONS_GHC -Wno-orphans #-}
module Data.NonEmptyTextSpec (spec) where

import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(arbitrary))

import qualified Data.NonEmptyText as NEText


-- TODO: Find a better solution than raising a orphan instance warning
instance Arbitrary Text.Text where
    arbitrary = Text.pack <$> arbitrary


-- TODO: Find a better solution than raising a orphan instance warning
instance Arbitrary NEText.NonEmptyText where
    arbitrary = NEText.new <$> arbitrary <*> arbitrary


equiv :: Eq a => (NEText.NonEmptyText -> a) -> (Text.Text -> a)
              -> NEText.NonEmptyText -> Bool
equiv fnet ftxt = (==) <$> fnet <*> ftxt . NEText.toText


spec :: Spec
spec = do
    describe "singleton" $
        prop "toText . NEText.singleton == Text.singleton" $
            (==) <$> NEText.toText . NEText.singleton <*> Text.singleton

    describe "isSingleton" $
        prop "isSingleton . singleton" $
            NEText.isSingleton . NEText.singleton

    describe "toText" $
        prop "toText is never empty" $
            \t -> Text.length (NEText.toText t) > 0

    describe "fromText" $
        prop "fromMaybe \"\" (toText <$> fromText Text) == Text" $
            \t ->
                let net :: Maybe Text.Text
                    net = (NEText.toText <$> NEText.fromText t)
                in fromMaybe Text.empty net == t

    describe "length" $
        prop "length = length . toText" $
            equiv NEText.length Text.length

    describe "append" $
        prop "(NEText + NEText) = (Text + Text)" $
            \t1 t2 ->
                let net = NEText.append t1 t2
                    txt = Text.append (NEText.toText t1) (NEText.toText t2)
                in NEText.toText net == txt

    describe "head" $
        prop "head = head . toText" $
            equiv NEText.head Text.head

    describe "last" $
        prop "last = last . toText" $
            equiv NEText.last Text.last

    describe "tail" $
        prop "tail = tail . toText" $
            equiv NEText.tail Text.tail

    describe "init" $
        prop "init = init . toText" $
            equiv NEText.init Text.init
