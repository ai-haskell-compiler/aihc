{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Data.Char
import           Data.Conduit
import           Data.Text                    as Text (Text, null, pack)
import           Data.Time.Clock
import           Data.Void
import           Data.XML.Types as XML
import           Generic.Random
import           Refined
import           Refined.Unsafe (reallyUnsafeRefine)
import           Test.QuickCheck.Instances
import           Test.QuickCheck.Modifiers
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.Atom.Conduit.Parse      as Parser
import           Text.Atom.Conduit.Render     as Renderer
import           Text.Atom.Types
import           URI.ByteString

main :: IO ()
main = defaultMain $ testGroup "Properties"
  [ roundtripProperty "AtomText" (renderAtomText "test") (atomText "test")
  , roundtripProperty "AtomPerson" (renderAtomPerson "test") (atomPerson "test")
  , roundtripProperty "AtomCategory" renderAtomCategory atomCategory
  , roundtripProperty "AtomLink" renderAtomLink atomLink
  , roundtripProperty "AtomGenerator" renderAtomGenerator atomGenerator
  , roundtripProperty "AtomSource" renderAtomSource atomSource
  , roundtripProperty "AtomContent" renderAtomContent atomContent
  , roundtripProperty "AtomEntry" renderAtomEntry atomEntry
  , roundtripProperty "AtomFeed" renderAtomFeed atomFeed
  ]


roundtripProperty :: Eq a => Arbitrary a => Show a
                  => TestName -> (a -> ConduitT () Event Maybe ()) -> ConduitM Event Void Maybe (Maybe a) -> TestTree
roundtripProperty name render parse = testProperty ("parse . render = id (" <> name <> ")") $ \input -> Just input === join (runConduit $ render input .| parse)


newtype Letter = Letter { getLetter :: Char } deriving (Eq, Ord, Read, Show)

instance Arbitrary Letter where
  arbitrary = Letter <$> choose ('a', 'z')


newtype Digit = Digit { getDigit :: Char } deriving (Eq, Ord, Read, Show)

instance Arbitrary Digit where
  arbitrary = Digit <$> (arbitrary `suchThat` isDigit)


newtype NonEmptyText = NonEmptyText { getNonEmptyText :: Text } deriving(Eq, Ord, Read, Show)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText . Text.pack <$> (map getAlphaNumChar <$> listOf1 arbitrary)


newtype AlphaNumChar = AlphaNumChar { getAlphaNumChar :: Char } deriving(Eq, Ord, Read, Show)

instance Arbitrary AlphaNumChar where
  arbitrary = AlphaNumChar <$> oneof [getLetter <$> arbitrary, getDigit <$> arbitrary]


-- | 'UTCTime' with rounded seconds.
newtype RoundedUTCTime = RoundedUTCTime { getRoundedUtcTime :: UTCTime } deriving(Eq, Ord, Read, Show)

instance Arbitrary RoundedUTCTime where
  arbitrary = RoundedUTCTime <$> do
    (UTCTime d s) <- arbitrary
    return $ UTCTime d (fromIntegral (round s :: Int))


instance Arbitrary Scheme where
  arbitrary = do
    a <- getLetter <$> arbitrary
    b <- listOf $ frequency [(10, getAlphaNumChar <$> arbitrary), (1, pure '+'), (1, pure '-'), (1, pure '.')]
    return $ Scheme $ encodeUtf8 $ pack (a:b)

instance Arbitrary Authority where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary UserInfo where
  arbitrary = UserInfo <$> (encodeUtf8 . getNonEmptyText <$> arbitrary)
                       <*> (encodeUtf8 . getNonEmptyText <$> arbitrary)

instance Arbitrary Host where
  arbitrary = Host <$> (encodeUtf8 . getNonEmptyText <$> arbitrary)

instance Arbitrary Port where
  arbitrary = Port <$> (getPositive <$> arbitrary)

instance Arbitrary Query where
  arbitrary = Query <$> listOf ((,) <$> (encodeUtf8 . getNonEmptyText <$> arbitrary) <*> (encodeUtf8 . getNonEmptyText <$> arbitrary))

instance Arbitrary URI where
  arbitrary = URI <$> arbitrary
                  <*> arbitrary
                  <*> (encodeUtf8 . ("/" <>) . getNonEmptyText <$> arbitrary)
                  <*> arbitrary
                  <*> (fmap (encodeUtf8 . getNonEmptyText) <$> arbitrary)

instance Arbitrary AtomURI where
  arbitrary = oneof [AtomURI <$> (arbitrary :: Gen (URIRef Absolute)), AtomURI <$> (arbitrary :: Gen (URIRef Relative))]

instance Arbitrary RelativeRef where
  arbitrary = RelativeRef <$> arbitrary
                          <*> (encodeUtf8 . ("/" <>) . getNonEmptyText <$> arbitrary)
                          <*> arbitrary
                          <*> (fmap (encodeUtf8 . getNonEmptyText) <$> arbitrary)

instance Arbitrary TextType where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary XML.Document where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary XML.Prologue where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary XML.Miscellaneous where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary XML.Instruction where
  arbitrary = XML.Instruction <$> (getNonEmptyText <$> arbitrary) <*> (getNonEmptyText <$> arbitrary)

instance Arbitrary XML.Doctype where
  arbitrary = XML.Doctype
    <$> (getNonEmptyText <$> arbitrary)
    <*> arbitrary

instance Arbitrary XML.ExternalID where
  arbitrary = oneof
    [ XML.SystemID <$> (getNonEmptyText <$> arbitrary)
    , XML.PublicID <$> (getNonEmptyText <$> arbitrary) <*> (getNonEmptyText <$> arbitrary)
    ]

instance Arbitrary XML.Element where
  arbitrary = scale (`div` 6) $ sized genElement

instance Arbitrary XML.Node where
  arbitrary = scale (`div` 5) $ sized genNode

instance Arbitrary XML.Name where
  arbitrary = XML.Name
    <$> (getNonEmptyText <$> arbitrary)
    <*> (fmap getNonEmptyText <$> arbitrary)
    <*> arbitrary

instance Arbitrary XML.Content where
  arbitrary = frequency [(10, ContentText <$> (getNonEmptyText <$> arbitrary)), (1, ContentEntity <$> (getNonEmptyText <$> arbitrary))]

genElement :: Int -> Gen XML.Element
genElement 0 = XML.Element <$> arbitrary <*> arbitrary <*> pure []
genElement n = XML.Element <$> arbitrary <*> arbitrary <*> (mergeNodes <$> listOf (genNode $ n `div` 2))

genNode :: Int -> Gen XML.Node
genNode 0 = oneof [XML.NodeInstruction <$> arbitrary, XML.NodeContent <$> arbitrary, XML.NodeComment <$> arbitrary]
genNode n = oneof [XML.NodeElement <$> genElement n, XML.NodeInstruction <$> arbitrary, XML.NodeContent <$> arbitrary, XML.NodeComment <$> arbitrary]

mergeNodes :: [Node] -> [Node]
mergeNodes (NodeContent (ContentText a):NodeContent (ContentText b):t) = mergeNodes $ NodeContent (ContentText $ a <> b):t
mergeNodes (node:t) = node:mergeNodes t
mergeNodes [] = []

genNotNullText :: Gen (Refined (Not Null) Text)
genNotNullText = reallyUnsafeRefine <$> arbitrary `suchThat` (not . Text.null)

instance Arbitrary AtomText where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary AtomPerson where
  arbitrary = AtomPerson <$> genNotNullText <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary AtomCategory where
  arbitrary = AtomCategory <$> genNotNullText <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary AtomLink where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary AtomGenerator where
  arbitrary = AtomGenerator <$> arbitrary <*> arbitrary <*> genNotNullText
  shrink = genericShrink

instance Arbitrary AtomSource where
  arbitrary = scale (`div` 5) $ AtomSource
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (fmap getRoundedUtcTime <$> arbitrary)

instance Arbitrary AtomContent where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary AtomEntry where
  arbitrary = scale (`div` 5) $ AtomEntry
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (fmap getRoundedUtcTime <$> arbitrary)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (getRoundedUtcTime <$> arbitrary)

instance Arbitrary AtomFeed where
  arbitrary = scale (`div` 5) $ AtomFeed <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> (getRoundedUtcTime <$> arbitrary)
