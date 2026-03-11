{-# LANGUAGE OverloadedStrings, StrictData #-}

module Include where

import Data.List (intersperse, mapAccumL)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (Builder, fromLazyText)
import Data.Text.Lazy.IO (hGetContents)
import System.IO (IOMode(ReadMode), openFile)

import HaskellLex (splitCodeOn, balancedCode)
import Report
import Text (linebreaks, splitOnOnce, textLiteral)


data Inclusion = Inclusion {

  -- | Which filename are we including?
  filename :: FilePath,

  -- | The delimiter which begins a code section in a template
  --
  -- Default: "$"
  openCode :: Text,

  -- | The delimiter which ends a code section in a template
  --
  -- Default: "$"
  closeCode :: Text,

  -- | Do we start in a literal section?
  --
  -- Default: True
  startLiteral :: Bool,

  -- | May we end in a literal section?
  --
  -- Default: True
  mayEndLiteral :: Bool,

  -- | May we end in a code section?
  --
  -- Default: False
  mayEndCode :: Bool,

  -- | Should we check that code sections are well-formed?
  --
  -- Default: True
  checkCodeBalanced :: Bool
}


data Chunk = Code Text | Literal Text


readTemplate :: Inclusion -> I Builder
readTemplate i = useChunks <$> ((liftE . formChunks 1) =<< liftIO input) where

  input :: IO Text
  input = hGetContents =<< openFile (filename i) ReadMode

  formChunks :: Int -> Text -> E [Chunk]
  formChunks
    | startLiteral i = formChunksLiteral
    | otherwise      = formChunksCode

  formChunksLiteral :: Int -> Text -> E [Chunk]
  formChunksLiteral n a
    | T.null a  = if mayEndLiteral i
                    then pure []
                    else illegalEnd
    | otherwise = f $ splitOnOnce (openCode i) a where
        illegalEnd = failure ("Template ends illegally in a literal beginning on line " <> T.pack (show n))
        f Nothing = if mayEndLiteral i
                      then pure [Literal a]
                      else illegalEnd
        f (Just (h,t)) = (Literal h:) <$> formChunksCode (n+p) t where
          p = linebreaks h

  formChunksCode :: Int -> Text -> E [Chunk]
  formChunksCode n a
    | T.null a  = if mayEndCode i
                    then pure []
                    else illegalEnd
    | otherwise = f $ splitCodeOn (closeCode i) a where
        illegalEnd = failure ("Template ends illegally in a code block beginning on line " <> T.pack (show n))
        f Nothing = if mayEndCode i
                      then pure . snd <$> makeChunkCode n a
                      else illegalEnd
        f (Just (h,t)) = do
          (n',c) <- makeChunkCode n h
          (c:) <$> formChunksLiteral (n'-1) t

  makeChunkCode :: Int -> Text -> E (Int, Chunk)
  makeChunkCode n t
    | balanced  = pure (n', Code $ T.intercalate "\n" clines)
    | otherwise = unbalanced where
        unbalanced = failure ("Unbalanced parentheses in code section beginning on line " <> T.pack (show n))
        (n', clines) = mapAccumL g n $ T.splitOn "\n" t where
          g k l = (k+1, l)
        balanced = not (checkCodeBalanced i) || balancedCode t

  useChunks :: [Chunk] -> Builder
  useChunks l = "makeTemplate (" <> innards <> ")" where
    innards = foldMap f . intersperse Nothing $ fmap Just l where
      f (Just (Literal a)) = "embedConst " <> textLiteral a
      f (Just (Code a)) = "(" <> fromLazyText a <> ")"
      f Nothing = " >>> "
