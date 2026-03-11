{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TestQQDefs where

import Data.String
import Language.Haskell.TH.Quote


sq :: QuasiQuoter
sq = qqExtractor 1 "\n****"

sq_f = quoteFile sq
sq2_f = quoteFile $ qqExtractor 2 "\n****"
sq3_f = quoteFile $ qqExtractor 3 "\n****"
sq4_f = quoteFile $ qqExtractor 4 "\n****"
sq5_f = quoteFile $ qqExtractor 5 "\n****"
sq6_f = quoteFile $ qqExtractor 6 "\n****"

uq :: QuasiQuoter
uq = qqExtractor 1 "\n____"

uq_f = quoteFile uq
uq2_f = quoteFile $ qqExtractor 2 "\n____"
uq3_f = quoteFile $ qqExtractor 3 "\n____"

qqExtractor idx sep = QuasiQuoter (extractor idx sep)
                      (error "no patterns supported")
                      (error "no types supported")
                      (error "no declarations supported")

-- extractor :: String -> String -> QState
extractor idx sep s =
  case inSeps idx sep $ filter (/= '\r') s of
    Post a -> [|fromString a|]
    Pre n _ -> error $ "No starting line found for block " <> show n
    MatchLine n -> error $ "Only starting line found for block " <> show n
    Pass n _ -> error $ "No ending line found for block " <> show n

data QState = Pre Int String | MatchLine Int | Pass Int String | Post String

inSeps :: Int -> String -> String -> QState
inSeps idx sep =
  let sepl = length sep
      nxtC :: QState -> Char -> QState
      nxtC (Pre n p) c = let p' = c : p
                             l = length p'
                         in if reverse p' == take l sep
                            then if l == sepl then MatchLine n else Pre n p'
                            else Pre n $ take (sepl-1) p'
      nxtC p@(MatchLine n) c = if '\n' == c then Pass n "" else p
      nxtC (Pass n s) c = let s' = c : s
                              sl = reverse $ take sepl s'
                          in if sl == sep
                             then if n == 1
                                  then Post (reverse $ drop (sepl-1) s')
                                  else Pre (n - 1) ""
                             else Pass n s'
      nxtC (Post s) _ = Post s
    in foldl nxtC (Pre idx "")
