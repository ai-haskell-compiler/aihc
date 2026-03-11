{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Combinators     as Conduit (decodeUtf8,
                                                          sourceFile)
import           Data.Default
import qualified Data.Text.Lazy.Encoding      as Lazy
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Golden            (findByExtension, goldenVsString)
import           Text.Atom.Conduit.Parse      as Parser
import           Text.Atom.Conduit.Render     as Renderer
import           Text.Atom.Types
import           Text.Pretty.Simple
import qualified Text.XML.Stream.Parse        as XML

main :: IO ()
main = defaultMain =<< do
  xmlFiles <- findByExtension [".xml"] "."

  return $ testGroup "Atom golden tests" $ do
    xmlFile <- xmlFiles
    let goldenFile = addExtension xmlFile ".golden"
        f file = fmap (Lazy.encodeUtf8 . pShowNoColor) $ runResourceT $ runConduit $ sourceFile file .| Conduit.decodeUtf8 .| XML.parseText def .| XML.force "Invalid <feed>" atomFeed
    return $ goldenVsString xmlFile goldenFile $ f xmlFile
