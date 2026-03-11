module Main where

import Test.HUnit
import System.Exit
import Apt
import Changes
import Control
import Dependencies
import Versions
import Debian.Sources
import Text.PrettyPrint

main :: IO ()
main =
    do (c,st) <- runTestText putTextToShowS (TestList (versionTests ++ [sourcesListTests] ++ dependencyTests ++ changesTests ++ controlTests ++ prettyTests ++ aptTests))
       putStrLn (st "")
       case (failures c) + (errors c) of
         0 -> return ()
         _ -> exitFailure

-- | I was converting from one pretty printing package to another and
-- was unclear how this should work.
prettyTests :: [Test]
prettyTests =
    [ TestCase (assertEqual
                "pretty0"
                (unlines
                 ["Usage: debian-report <old sources.list> <new sources.list>",
                  "",
                  "Find all the packages referenced by the",
                  "second sources.list which trump packages",
                  "found in the first sources.list."])
                (renderStyle (style {lineLength = 60}) (helpText "debian-report"))
               ) ]

helpText :: String -> Doc
helpText progName =
    (text "Usage:" <+> text progName <+> text "<old sources.list>" <+> text "<new sources.list>" $$
     text [] $$
     (fsep $ map text $ words $ "Find all the packages referenced by the second sources.list which trump packages found in the first sources.list.") $$
     text []
    )
