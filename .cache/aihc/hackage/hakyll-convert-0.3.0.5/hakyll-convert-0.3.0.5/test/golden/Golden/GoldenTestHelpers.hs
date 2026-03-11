{-# LANGUAGE StandaloneDeriving #-}

module Golden.GoldenTestHelpers where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import Hakyll.Convert.Common
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

class CustomShow a where
  customShow :: a -> String

deriving instance Show DistilledPost

instance CustomShow DistilledPost where
  customShow = show

readPostsHelper ::
  (CustomShow a) =>
  (FilePath -> IO (Maybe [a])) ->
  String ->
  String ->
  ([a] -> Maybe a) ->
  TestTree
readPostsHelper readPosts dir testName selector =
  goldenVsString
    testName
    (dir ++ (map spaceToDash testName) ++ ".golden")
    ( do
        posts <- readPosts (dir ++ "input.xml")
        return $
          fromMaybe
            LBS.empty
            ( posts
                >>= selector
                >>= (return . LBS.pack . customShow)
            )
    )
  where
    spaceToDash c = if c == ' ' then '-' else c

readAndDistillHelper ::
  (FilePath -> IO (Maybe [a])) ->
  (Bool -> a -> DistilledPost) ->
  String ->
  String ->
  ([DistilledPost] -> Maybe DistilledPost) ->
  TestTree
readAndDistillHelper readPosts distill dir testName selector =
  let fut filepath = do
        posts <- readPosts filepath
        let extractComments = True
        return $ fmap (map (distill extractComments)) posts
   in readPostsHelper fut dir testName selector
