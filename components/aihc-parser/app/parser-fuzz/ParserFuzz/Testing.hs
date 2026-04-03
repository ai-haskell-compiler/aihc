module ParserFuzz.Testing (runWithOptions) where

import Aihc.Parser.Syntax qualified as Syntax
import Data.Text qualified as T
import Language.Haskell.Exts qualified as HSE
import ParserFuzz.Arbitrary (generateCandidate, normalizeCandidateAst, qcGenStream, shrinkGeneratedModule)
import ParserFuzz.CLI (Options (..))
import ParserFuzz.Types (Candidate (..), SearchResult (..))
import ParserValidation (validateParser)
import ShrinkUtils (candidateTransformsWith)
import System.Random (randomIO)
import Test.QuickCheck (shrink)
import Test.QuickCheck.Random (QCGen)

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  seed <- resolveSeed (optSeed opts)
  found <- findFirstFailure opts seed
  case found of
    Nothing -> do
      putStrLn ("Seed: " <> show seed)
      putStrLn ("Tests tried: " <> show (optMaxTests opts))
      putStrLn "No failing module found."
    Just result -> do
      let (minimized, shrinksAccepted) = shrinkCandidate opts (srCandidate result)
          minimizedSource = candSource minimized
      putStrLn ("Seed: " <> show seed)
      putStrLn ("Tests tried: " <> show (srTestsTried result))
      putStrLn ("Shrink passes accepted: " <> show shrinksAccepted)
      if lineCount minimizedSource <= 5
        then do
          putStrLn "Minimized source:"
          putStrLn "---8<---"
          putStrLn minimizedSource
          putStrLn "--->8---"
        else
          putStrLn "Minimized source omitted (>5 lines)."
      case validateParser "arbitrary" Syntax.Haskell2010Edition [] (T.pack minimizedSource) of
        Nothing -> pure ()
        Just err -> do
          putStrLn "Validation failure:"
          print err
      case optOutput opts of
        Nothing -> pure ()
        Just path -> do
          writeFile path minimizedSource
          putStrLn ("Wrote minimized source to " <> path)

resolveSeed :: Maybe Int -> IO Int
resolveSeed (Just seed) = pure seed
resolveSeed Nothing = randomIO

findFirstFailure :: Options -> Int -> IO (Maybe SearchResult)
findFirstFailure opts seed = go 1 (qcGenStream seed)
  where
    go :: Int -> [QCGen] -> IO (Maybe SearchResult)
    go _ [] = pure Nothing
    go idx (g : gs)
      | idx > optMaxTests opts = pure Nothing
      | otherwise =
          let candidate = generateCandidate (optSize opts) g
           in do
                printGeneratedModule opts idx candidate
                if oursFails (candSource candidate)
                  then pure (Just (SearchResult idx candidate))
                  else go (idx + 1) gs

printGeneratedModule :: Options -> Int -> Candidate -> IO ()
printGeneratedModule opts idx candidate
  | not (optPrintGeneratedModules opts) = pure ()
  | otherwise = do
      putStrLn ("Generated module #" <> show idx <> ":")
      putStrLn "---8<---"
      putStrLn (candSource candidate)
      putStrLn "--->8---"

shrinkCandidate :: Options -> Candidate -> (Candidate, Int)
shrinkCandidate opts = go 0
  where
    go :: Int -> Candidate -> (Candidate, Int)
    go accepted candidate
      | accepted >= optMaxShrinkPasses opts = (candidate, accepted)
      | otherwise =
          case firstSuccessfulShrink candidate of
            Nothing -> (candidate, accepted)
            Just nextCandidate -> go (accepted + 1) nextCandidate

firstSuccessfulShrink :: Candidate -> Maybe Candidate
firstSuccessfulShrink candidate = tryCandidates (candidateTransforms candidate)
  where
    tryCandidates :: [HSE.Module HSE.SrcSpanInfo] -> Maybe Candidate
    tryCandidates [] = Nothing
    tryCandidates (ast' : rest) =
      case normalizeCandidateAst "firstSuccessfulShrink" ast' of
        Left msg -> error msg
        Right candidate' ->
          if oursFails (candSource candidate')
            then Just candidate'
            else tryCandidates rest

candidateTransforms :: Candidate -> [HSE.Module HSE.SrcSpanInfo]
candidateTransforms candidate =
  candidateTransformsWith shrink (candAst candidate)
    <> shrinkGeneratedModule (candAst candidate)
    <> []

-- TODO: Add leaf-pruning transforms for exports, imports, and decl lists.
-- TODO: Add recursive subtree-pruning transforms across declaration/expr/type AST nodes.

oursFails :: String -> Bool
oursFails source =
  case validateParser "arbitrary" Syntax.Haskell2010Edition [] (T.pack source) of
    Nothing -> False
    Just _ -> True

lineCount :: String -> Int
lineCount = length . lines
