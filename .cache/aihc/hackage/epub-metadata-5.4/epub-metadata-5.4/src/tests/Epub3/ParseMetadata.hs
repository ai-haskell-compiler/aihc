module Epub3.ParseMetadata
   ( tests )
   where

import Control.Monad.Except
import qualified Data.Map.Strict as Map
import System.FilePath
import Test.HUnit

import Codec.Epub.Data.Metadata
import Codec.Epub.Parse


tests :: Test
tests = TestList
   [ testFull
   ]


{- A fairly comprehensive test containing all possible things
   Not complete at this time because the library can't parse it all yet!
-}
testFull :: Test
testFull = TestCase $ do
   xmlString <- readFile $ "util" </> "resources" </> "epub3-full.opf"
   actual <- runExceptT $ getMetadata xmlString
   let expected =
         Right Metadata
            { metaIds =
               [ Identifier (Just "isbn") Nothing Nothing "1-82057-821-9"
               , Identifier (Just "other") (Just "some-type")
                  (Just "some-scheme") "1386506873266"
               ]
            , metaTitles =
               [ Title Nothing (Just "main") (Just 1) "Title Of This Book"
               , Title Nothing (Just "subtitle") (Just 2) "The Subtitle"
               , Title (Just "fr") Nothing Nothing "Titre De Ce Livre"
               ]
            , metaLangs = ["en-US", "en-UK"]
            , metaContributors =
               [ Creator
                  (Just "ill")
                  (Just "Knickerbocker, Reginald Q.")
                  Nothing
                  "Reginald Q. Knickerbocker"
               , Creator
                  (Just "edt")
                  Nothing
                  Nothing
                  "Beverly Abercrombie"
               ]
            , metaCreators =
               [ Creator
                  (Just "aut")
                  (Just "Wiggins, Josephine B.")
                  (Just 2)
                  "Josephine B. Wiggins"
               , Creator
                  (Just "aut")
                  Nothing
                  (Just 1)
                  "Horatio Cromwell"
               , Creator
                  Nothing
                  Nothing
                  (Just 3)
                  "Natalia Jenkins"
               ]
            , metaDates = Map.fromList
                [ (Issued, DateValue "2011")
                , (Epub, DateValue "2012")
                , (Modified, DateValue "2013-08-31T13:06:32Z")
                ]
            , metaSources =
              [ Source Nothing Nothing Nothing "document source"
              , Source (Just "15") (Just "onix:codelist5") (Just "pagination") "another source"
              ]
            , metaType = Just "test OPF Package Document"
            , metaCoverages =
               [ "coverage information"
               , "an additional coverage"
               ]
            , metaDescriptions =
               [ Description Nothing "This document is a stub used for unit testing. It is missing the rest of the tags that normally occur after metadata."
               , Description Nothing "An additional description"
               ]
            , metaFormats =
               [ "ePub publication"
               , "an additional format"
               ]
            , metaPublishers =
               [ "Fictional Books Ltd."
               , "An additional publisher"
               ]
            , metaRelations =
               [ "document relation"
               , "an additional relation"
               ]
            , metaSubjects = ["Fiction","Science Fiction"]
            , metaRights =
               [ "Copyright: 2010 Dino Morelli, License: ISC"
               , "an additional rights"
               ]
            }

   assertEqual "epub3 - very full" expected actual
