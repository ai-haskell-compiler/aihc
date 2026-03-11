{- |
   Module      : Path.Utils
   License     : MIT
   Stability   : experimental

Miscellaneous utility functions for the path library.
-}

module Path.Utils (
  (</$>)
, changeDir
, splitPath
, toFilePathText
) where

import Control.Monad.Catch
import qualified Data.Text as T
import Data.Text (Text)
import Data.List.Split
import Path

-- | Apply `(</>)` to every relative path inside a functor.
(</$>) :: Functor f => Path b Dir -> f (Path Rel t) -> f (Path b t)
(</$>) d = fmap (d </>)

-- | Change directory from src to dst.
changeDir :: MonadThrow m => Path b Dir -> Path b' Dir -> Path b t -> m (Path b' t)
changeDir src dst fp = (dst </>) <$> stripProperPrefix src fp

-- | Split a relative path into text sections.
splitPath :: Path Rel t -> [Text]
splitPath = fmap T.pack . splitOn "/" . toFilePath

-- | Convert a `Path` to `Text`.
toFilePathText :: Path b t -> Text
toFilePathText = T.pack . toFilePath
