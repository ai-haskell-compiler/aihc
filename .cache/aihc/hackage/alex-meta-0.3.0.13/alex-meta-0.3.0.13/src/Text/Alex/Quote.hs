{-#OPTIONS_GHC -fno-warn-missing-fields#-}

module Text.Alex.Quote (
    parseAlex
--  , parseAlexGHC
  , compileAlex
  , Alex
  , alex
--  , CLIFlags(..)
  ) where

import Text.Alex(runAlex, Target(..), CLIFlags(..))
import Text.Alex.AlexTemplate

import Language.Haskell.TH.Quote
import Language.Haskell.TH

import Language.Haskell.Meta

type Alex = String

compileAlex :: Alex -> Q [Dec]
compileAlex = return . either error id . parseDecs

alex :: QuasiQuoter
alex = QuasiQuoter {quoteExp = litE . StringL . parseAlex}

parseAlex :: String -> Alex
parseAlex s = fst (runAlex [] Nothing s) ++ "\n" ++ alexTemplate HaskellTarget

-- parseAlexGHC :: String -> Alex
-- parseAlexGHC s = fst (runAlex [OptGhcTarget] Nothing s) ++ "\n" ++ alexTemplate GhcTarget

