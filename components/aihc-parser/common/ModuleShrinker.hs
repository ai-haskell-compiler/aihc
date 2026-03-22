{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ModuleShrinker
  ( shrinkModule,
    shrinkModuleWithExtensions,
  )
where

import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad ((<=<))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.LanguageExtensions.Type (Extension)
import qualified GhcOracle
import HseExtensions (toHseExtension)
import qualified Language.Haskell.Exts as HSE
import ShrinkUtils (candidateTransformsWith, unique)
import System.IO.Unsafe (unsafePerformIO)

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
      safeExactPrint modu0 >>= \src ->
        Just
          ShrinkCandidate
            { candAst = modu0,
              candSource = src
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
      safeExactPrint modu0 >>= \src ->
        Just
          ShrinkCandidate
            { candAst = modu0,
              candSource = src
            }

-- | Safely call HSE.exactPrint, catching any exceptions.
-- HSE can throw exceptions like "ExactP: QName is given wrong number of srcInfoPoints"
-- when the AST has malformed source location info.
safeExactPrint :: HSE.Module HSE.SrcSpanInfo -> Maybe Text
safeExactPrint modu =
  -- Using unsafePerformIO here is acceptable because:
  -- 1. HSE.exactPrint is conceptually pure (it just formats an AST)
  -- 2. The exception is a bug in HSE, not an expected IO effect
  -- 3. We want to gracefully degrade rather than crash
  unsafePerformIO $ do
    -- Use force to fully evaluate the string, since exactPrint is lazy
    -- and exceptions may be thrown during traversal
    result <- try (evaluate (force (HSE.exactPrint modu [])))
    pure $ case result of
      Left (_ :: SomeException) -> Nothing
      Right s -> Just (T.pack s)

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
