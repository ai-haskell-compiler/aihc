{-# LANGUAGE CPP #-}

-- |
--
-- @since 0.1.0.0
module Database.Persist.Discover
    ( findPersistentModelFiles
    ) where

import Database.Persist.Discover.Exe (getFilesRecursive, stripSuffix)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (makeRelativeToProject)
import Data.FileEmbed

-- | Returns a list of all files with the @.persistentmodels@ suffix.
--
-- > allFiles :: [FilePath]
-- > allFiles = $$(findPersistentModelFiles "config/models/")
--
-- @since 0.1.0.0
findPersistentModelFiles
    :: FilePath
    -- ^ The root directory to search from.
    -> Q (TExp [FilePath])
findPersistentModelFiles root = do
    projectRoot <- makeRelativeToProject root
    files <- runIO $ filter isPersistentModelFile <$> getFilesRecursive projectRoot
#if MIN_VERSION_template_haskell(2,17,0)
    examineCode $ liftTyped files
#else
    liftTyped files
#endif
  where
    isPersistentModelFile filename =
        case stripSuffix ".persistentmodels" filename of
            Just _ ->
                True
            _ ->
                False
