{-# LANGUAGE OverloadedStrings #-}
module Language.PlantUML.CallSpec (spec) where

import qualified Data.ByteString.Char8            as BS (filter, length)

import Test.Hspec

import Control.Monad                    (forM_)
import Data.ByteString.Char8            (ByteString)
import Language.PlantUML.Call           (DiagramType (..), drawPlantUMLDiagram)

spec :: Spec
spec =
  describe "existsInstance" $ do
    it "an empty specification cannot be drawn" $
      drawPlantUMLDiagram SVG "" `shouldThrow` anyIOException
    it "generates the correct solution for hello world and ASCIIArt" $
      (BS.filter (/= '\r') <$> drawPlantUMLDiagram ASCIIArt helloWorld) `shouldReturn`
        "     ,-.          ,-----.\n     |I|          |World|\n     `+'          `--+--'\n      |    hello     |   \n      |------------->|   \n     ,+.          ,--+--.\n     |I|          |World|\n     `-'          `-----'\n"
    forM_ [minBound ..] $ \what -> it ("hello world is working on " ++ show what) $
      ((> 200) . BS.length <$> drawPlantUMLDiagram what helloWorld) `shouldReturn` True

helloWorld :: ByteString
helloWorld = "@startuml\nI -> World : hello\n@enduml"
