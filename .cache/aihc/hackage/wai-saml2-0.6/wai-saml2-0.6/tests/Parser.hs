{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
import Network.Wai.SAML2.EntityDescriptor
import Network.Wai.SAML2.Response
import Network.Wai.SAML2.XML
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Text.Show.Pretty
import Text.XML.Cursor
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Text.XML as XML

run :: forall t. (FromXML t, Show t) => FilePath -> IO BC.ByteString
run src = do
    doc <- XML.readFile XML.def src
    resp <- parseXML (fromDocument doc)
    pure $ BC.pack $ ppShow (resp :: t)

main :: IO ()
main = defaultMain $ testGroup "Parse SAML2 response"
    [ mkGolden @Response $ prefix </> "keycloak.xml"
    , mkGolden @Response $ prefix </> "okta.xml"
    , mkGolden @Response $ prefix </> "google.xml"
    , mkGolden @IDPSSODescriptor $ prefix </> "metadata/keycloak.xml"
    , mkGolden @IDPSSODescriptor $ prefix </> "metadata/google.xml"
    ]
    where
        prefix = "tests/data"
        mkGolden :: forall t. (FromXML t, Show t) => FilePath -> TestTree
        mkGolden path = goldenVsStringDiff
                (takeBaseName path)
                (\ref new -> ["diff", "-u", ref, new])
                (path <.> "expected")
                (run @t path)
