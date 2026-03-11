{- |
Example program for TagSoup.
Detects basic syntax errors like isolated ampersands and unquoted attribute values.
-}
module Main where

import qualified Text.HTML.Tagchup.Parser      as Parser
import qualified Text.HTML.Tagchup.PositionTag as PosTag
import qualified Text.HTML.Tagchup.Tag         as Tag
import qualified Text.XML.Basic.Position       as Position
import qualified Text.XML.Basic.Name.MixedCase as Name

import System.Environment (getArgs, )

import Data.Maybe (mapMaybe, )


validate :: FilePath -> String -> String
validate fileName input =
   let tags :: [PosTag.T Name.T String]
       tags = Parser.runSoupWithPositionsName fileName input
       warnings =
          mapMaybe
             (\(PosTag.Cons pos tag) ->
                 fmap (\msg -> Position.toReportText pos ++ " " ++ msg) $
                 Tag.maybeWarning tag)
             tags
   in  unlines warnings

validateIO :: FilePath -> IO ()
validateIO fileName =
   putStrLn . validate fileName =<< readFile fileName


main :: IO ()
main = mapM_ validateIO =<< getArgs
