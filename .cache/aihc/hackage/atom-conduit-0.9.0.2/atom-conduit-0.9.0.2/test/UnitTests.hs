{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Combinators     as Conduit (yieldMany)
import           Data.Default
import           Data.Time.Clock
import           Data.XML.Types
import           Lens.Micro
import           Refined
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Atom.Conduit.Parse      as Parser
import           Text.Atom.Conduit.Render     as Renderer
import           Text.Atom.Lens
import           Text.Atom.Types
import qualified Text.XML.Stream.Parse        as XML
import           URI.ByteString

main :: IO ()
main = defaultMain $ testGroup "Unit tests"
  [ linkCase
  , personCase
  , generatorCase
  , sourceCase
  , textConstructCase
  ]

linkCase :: TestTree
linkCase = testCase "Link element" $ do
  result <- runResourceT . runConduit $ yieldMany input .| XML.parseText def .| XML.force "Invalid <link>" atomLink
  result ^. linkHrefL @?= AtomURI (RelativeRef Nothing "/feed" (Query []) Nothing)
  (result ^. linkRelL) @?= "self"
  where input = ["<link xmlns=\"http://www.w3.org/2005/Atom\" rel=\"self\" href=\"/feed\" />"]

personCase :: TestTree
personCase = testCase "Person construct" $ do
  result <- runResourceT . runConduit $ yieldMany input .| XML.parseText def .| XML.force "Invalid <author>" (atomPerson "author")
  unrefine (result ^. personNameL) @?= "John Doe"
  result ^. personEmailL @?= "JohnDoe@example.com"
  result ^. personUriL @?= Just (AtomURI $ URI (Scheme "http") (Just $ Authority Nothing (Host "example.com") Nothing) "/~johndoe" (Query []) Nothing)
  where input =
          [ "<author xmlns=\"http://www.w3.org/2005/Atom\">"
          , "<name>John Doe</name>"
          , "<email>JohnDoe@example.com</email>"
          , "<uri>http://example.com/~johndoe</uri>"
          , "</author>"
          ]

generatorCase :: TestTree
generatorCase = testCase "Generator element" $ do
  result <- runResourceT . runConduit $ yieldMany input .| XML.parseText def .| XML.force "Invalid <generator>" atomGenerator
  result ^. generatorUriL @?= Just (AtomURI $ RelativeRef Nothing "/myblog.php" (Query []) Nothing)
  (result ^. generatorVersionL) @?= "1.0"
  unrefine (result ^. generatorContentL) @?= "Example Toolkit"
  where input =
          [ "<generator xmlns=\"http://www.w3.org/2005/Atom\" uri=\"/myblog.php\" version=\"1.0\">"
          , "Example Toolkit"
          , "</generator>"
          ]

sourceCase :: TestTree
sourceCase = testCase "Source element" $ do
  result <- runResourceT . runConduit $ yieldMany input .| XML.parseText def .| XML.force "Invalid <source>" atomSource
  (result ^. sourceIdL) @?= "http://example.org/"
  (result ^. sourceTitleL) @?= Just (AtomPlainText TypeText "Fourty-Two")
  show <$> (result ^. sourceUpdatedL) @?= Just "2003-12-13 18:30:02 UTC"
  (result ^. sourceRightsL) @?= Just (AtomPlainText TypeText "© 2005 Example, Inc.")
  where input =
          [ "<source xmlns=\"http://www.w3.org/2005/Atom\">"
          , "<id>http://example.org/</id>"
          , "<title>Fourty-Two</title>"
          , "<updated>2003-12-13T18:30:02Z</updated>"
          , "<rights>© 2005 Example, Inc.</rights>"
          , "</source>"
          ]

textConstructCase :: TestTree
textConstructCase = testCase "Text construct" $ do
  a:b:c:_ <- runResourceT . runConduit $ yieldMany input .| XML.parseText def .| XML.many (atomText "title")
  a @?= AtomPlainText TypeText "AT&T bought by SBC!"
  b @?= AtomPlainText TypeHTML "AT&amp;T bought <b>by SBC</b>!"
  c @?= AtomXHTMLText (Element "{http://www.w3.org/1999/xhtml}div" []
    [ NodeContent (ContentText "AT&T bought ")
    , NodeElement (Element "{http://www.w3.org/1999/xhtml}b" [] [ NodeElement (Element "{http://www.w3.org/1999/xhtml}em" [] [NodeContent (ContentText "by SBC")]) ])
    , NodeContent (ContentText "!")
    ] )
  where input =
          [ "<title xmlns=\"http://www.w3.org/2005/Atom\" type=\"text\">AT&amp;T bought by SBC!</title>"
          , "<title xmlns=\"http://www.w3.org/2005/Atom\" type=\"html\">"
          , "AT&amp;amp;T bought &lt;b&gt;by SBC&lt;/b&gt;!"
          , "</title>"
          , "<title xmlns=\"http://www.w3.org/2005/Atom\" type=\"xhtml\">"
          , "<div xmlns=\"http://www.w3.org/1999/xhtml\">"
          , "AT&amp;T bought <b><em>by SBC</em></b>!"
          , "</div>"
          , "</title>"
          ]
