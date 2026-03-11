{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.AST.Arbitrary
    ( AnyArgument(..)
    , AnyLocation(..)
    , AnyName(..)
    , AnyNode(..)
    , AnyObjectField(..)
    , AnyValue(..)
    , printArgument
    ) where

import qualified Language.GraphQL.AST.Document as Doc
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck (oneof, elements, listOf, resize, NonEmptyList (..))
import Test.QuickCheck.Gen (Gen (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Functor ((<&>))

newtype AnyPrintableChar = AnyPrintableChar
    { getAnyPrintableChar :: Char
    } deriving (Eq, Show)

alpha :: String
alpha = ['a'..'z'] <> ['A'..'Z']

num :: String
num = ['0'..'9']

instance Arbitrary AnyPrintableChar where
    arbitrary = AnyPrintableChar <$> elements chars
        where
           chars = alpha <> num <> ['_']

newtype AnyPrintableText = AnyPrintableText
    { getAnyPrintableText :: Text
    } deriving (Eq, Show)

instance Arbitrary AnyPrintableText where
    arbitrary = do
        nonEmptyStr <- getNonEmpty <$> (arbitrary :: Gen (NonEmptyList AnyPrintableChar))
        pure $ AnyPrintableText
            $ Text.pack
            $ map getAnyPrintableChar nonEmptyStr

-- https://spec.graphql.org/June2018/#Name
newtype AnyName = AnyName
    { getAnyName :: Text
    } deriving (Eq, Show)

instance Arbitrary AnyName where
    arbitrary = do
        firstChar <- elements $ alpha <> ['_']
        rest <- (arbitrary :: Gen [AnyPrintableChar])
        pure $ AnyName
            $ Text.pack
            $ firstChar : map getAnyPrintableChar rest

newtype AnyLocation = AnyLocation
    { getAnyLocation :: Doc.Location
    } deriving (Eq, Show)

instance Arbitrary AnyLocation where
    arbitrary = AnyLocation <$> (Doc.Location <$> arbitrary <*> arbitrary)

newtype AnyNode a = AnyNode
    { getAnyNode :: Doc.Node a
    } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (AnyNode a) where
    arbitrary = do
        (AnyLocation location') <- arbitrary
        node' <- flip Doc.Node location' <$> arbitrary
        pure $ AnyNode node'

newtype AnyObjectField a = AnyObjectField
    { getAnyObjectField :: Doc.ObjectField a
    } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (AnyObjectField a) where
    arbitrary = do
        name' <- getAnyName <$> arbitrary
        value' <- getAnyNode <$> arbitrary
        location' <- getAnyLocation <$> arbitrary
        pure $ AnyObjectField $ Doc.ObjectField name' value' location'

newtype AnyValue = AnyValue
    { getAnyValue :: Doc.Value
    } deriving (Eq, Show)

instance Arbitrary AnyValue
  where
    arbitrary =
        let variableGen :: Gen Doc.Value
            variableGen = Doc.Variable . getAnyName <$> arbitrary
            listGen :: Gen [Doc.Node Doc.Value]
            listGen = (resize 5 . listOf) nodeGen
            nodeGen :: Gen (Doc.Node Doc.Value)
            nodeGen = fmap getAnyNode arbitrary <&> fmap getAnyValue
            objectGen :: Gen [Doc.ObjectField Doc.Value]
            objectGen = resize 1
                $ fmap getNonEmpty arbitrary
                <&> map (fmap getAnyValue . getAnyObjectField)
         in AnyValue <$> oneof
            [ variableGen
            , Doc.Int <$> arbitrary
            , Doc.Float <$> arbitrary
            , Doc.String . getAnyPrintableText <$> arbitrary
            , Doc.Boolean <$> arbitrary
            , MkGen $ \_ _ -> Doc.Null
            , Doc.Enum . getAnyName <$> arbitrary
            , Doc.List <$> listGen
            , Doc.Object <$> objectGen
            ]

newtype AnyArgument a = AnyArgument
    { getAnyArgument :: Doc.Argument
    } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (AnyArgument a) where
    arbitrary = do
        name' <- getAnyName <$> arbitrary
        (AnyValue value') <- arbitrary
        (AnyLocation location') <- arbitrary
        pure $ AnyArgument $ Doc.Argument name' (Doc.Node value' location') location'

printArgument :: AnyArgument AnyValue -> Text
printArgument (AnyArgument (Doc.Argument name' (Doc.Node value' _) _)) =
    name' <> ": " <> (Text.pack . show) value'
