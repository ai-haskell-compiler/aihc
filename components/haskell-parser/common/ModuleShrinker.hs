{-# LANGUAGE OverloadedStrings #-}

module ModuleShrinker
  ( shrinkModule,
    shrinkModuleWithExtensions,
  )
where

import Control.Monad ((<=<))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.LanguageExtensions.Type (Extension)
import qualified GhcOracle
import HseExtensions (toHseExtension)
import qualified Language.Haskell.Exts as HSE
import ShrinkUtils (candidateTransformsWith, unique)

shrinkModule :: (Text -> Bool) -> Text -> Maybe Text
shrinkModule = shrinkModuleWithExtensions []

shrinkModuleWithExtensions :: [Extension] -> (Text -> Bool) -> Text -> Maybe Text
shrinkModuleWithExtensions exts keeps source
  | not (keeps source) = Nothing
  | otherwise =
      case parseCandidate exts source of
        Nothing -> Nothing
        Just candidate0 -> Just (runShrinks candidate0)
  where
    runShrinks candidate0 =
      let steps = iterateShrink 0 candidate0
       in candSource steps

    iterateShrink :: Int -> ShrinkCandidate -> ShrinkCandidate
    iterateShrink accepted candidate
      | accepted >= 200 = candidate
      | otherwise =
          case firstSuccessfulShrink candidate of
            Nothing -> candidate
            Just candidate' -> iterateShrink (accepted + 1) candidate'

    firstSuccessfulShrink candidate = tryCandidates (candidateTransforms candidate)
      where
        tryCandidates [] = Nothing
        tryCandidates (ast' : rest) =
          case normalizeCandidateAst ast' of
            Nothing -> tryCandidates rest
            Just candidate'
              | keeps (candSource candidate') -> Just candidate'
              | otherwise -> tryCandidates rest

    normalizeCandidateAst modu0 =
      Just
        ShrinkCandidate
          { candAst = modu0,
            candSource = T.pack (HSE.exactPrint modu0 [])
          }

candidateTransforms :: ShrinkCandidate -> [HSE.Module HSE.SrcSpanInfo]
candidateTransforms candidate = candidateTransformsWith trimSegment (candAst candidate)

hseParseMode :: HSE.ParseMode
hseParseMode =
  HSE.defaultParseMode
    { HSE.parseFilename = "<module-shrinker>",
      HSE.extensions = []
    }

data ShrinkCandidate = ShrinkCandidate
  { candAst :: HSE.Module HSE.SrcSpanInfo,
    candSource :: Text
  }

parseCandidate :: [Extension] -> Text -> Maybe ShrinkCandidate
parseCandidate exts source0 =
  case HSE.parseFileContentsWithMode (hseParseModeFor exts) (T.unpack source0) of
    HSE.ParseFailed _ _ -> Nothing
    HSE.ParseOk modu0 ->
      Just
        ShrinkCandidate
          { candAst = modu0,
            candSource = T.pack (HSE.exactPrint modu0 [])
          }

trimSegment :: String -> [String]
trimSegment segment =
  unique
    [ candidate
    | n <- [1 .. length segment - 1],
      let candidate = take n segment,
      not (null candidate)
    ]

hseParseModeFor :: [Extension] -> HSE.ParseMode
hseParseModeFor exts =
  hseParseMode
    { HSE.extensions =
        mapMaybe
          ( toHseExtension
              <=< GhcOracle.fromGhcExtension
          )
          exts
    }
