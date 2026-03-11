{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Yesod.GitRepo
    ( GitRepo
    , grRefresh
    , grContent
    , gitRepo
    , gitRepoDev
    , Route (..)
    ) where

import           Control.Applicative        ((<$>))
import           Control.Concurrent         (forkIO)
import           Control.Monad              (forever, void)
import           Data.Foldable              (fold)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack, unpack)
import           Network.HTTP.Types         (status200)
import           Network.Wai                (responseLBS)
import           Prelude                    (Eq, IO, Maybe (..), Monad (..),
                                             Read, Show, error, error, map,
                                             show, ($), FilePath)
import           System.Directory           (getTemporaryDirectory,
                                             removeDirectory)
import           System.Exit                (ExitCode (ExitSuccess, ExitFailure))
import           UnliftIO
import qualified System.IO.Temp
import           System.Process             (createProcess, cwd, proc,
                                             waitForProcess)
import           Yesod.Core                 (ParseRoute (..), RenderRoute (..),
                                             YesodSubDispatch (..), typePlain,
                                             HandlerFor)
import           Yesod.Core.Types           (yreSite, ysreGetSub,
                                             ysreParentEnv)

-- | A combination of Yesod subsite to be used for creating a refresh route, as
-- well as action to extract the current value of the content and force a
-- refresh.
--
-- Since 0.1.0
data GitRepo a = GitRepo
    { grRefresh :: IO ()
    -- ^ Force a refresh of the content. Usually this is done automatically via
    -- a POST request to the subsite route.
    --
    -- Since 0.1.0
    , grContent :: IO a
    -- ^ Get the current value of the content.
    --
    -- Since 0.1.0
    }

-- | Create a new @GitRepo@ value that can be used as a refresh subsite, as
-- well as to extract the current value of the content.
--
-- Note that if the initial clone or user action fails, this function will
-- throw an exception. For subsequent refreshes, the exception will be stored
-- as an impure exception for future @grContent@ calls.
--
-- Since 0.1.0
gitRepo :: Text -- ^ URL
           -> Text -- ^ branch name
           -> (FilePath -> IO a) -- ^ what to do on clone/refresh
           -> IO (GitRepo a)
gitRepo url branch refresh = do
    tmpDir <- getTemporaryDirectory
    contentDir <- System.IO.Temp.createTempDirectory tmpDir "git-repo"
    removeDirectory contentDir
    git Nothing ["clone", "-b", branch, url, pack contentDir]
    ref <- refresh contentDir >>= newIORef
    var <- newEmptyMVar
    mask_ $ void $ forkIO $ forever $ do
        takeMVar var
        void $ tryAny $ do
            git (Just contentDir) ["fetch"]
            git (Just contentDir) ["reset", "--hard", "origin/" <> branch]
            refresh contentDir >>= writeIORef ref

    return GitRepo
        { grRefresh = void $ tryPutMVar var ()
        , grContent = readIORef ref
        }

instance RenderRoute (GitRepo a) where
    data Route (GitRepo a) = GitRepoRoute
        deriving (Show, Eq, Read)
    renderRoute _ = ([], [])

instance ParseRoute (GitRepo a) where
    parseRoute ([], []) = Just GitRepoRoute
    parseRoute _ = Nothing

-- | Like 'gitRepo', but intended to be used in a dev environment. It just uses
-- a hard-coded @FilePath@ and reloads the contents on each request.
--
-- Since 0.1.1
gitRepoDev :: FilePath
           -> (FilePath -> IO a)
           -> IO (GitRepo a)
gitRepoDev fp refresh = return GitRepo
    { grRefresh = return ()
    , grContent = refresh fp
    }

instance YesodSubDispatch (GitRepo a) site where
    yesodSubDispatch env _req send = do
        void $ forkIO $ grRefresh gr
        send $ responseLBS status200 [("Content-Type", typePlain)]
            "Reload initiated"
      where
        gr = ysreGetSub env $ yreSite $ ysreParentEnv env

git :: Maybe FilePath -> [Text] -> IO ()
git mdir args = do
    (Nothing, Nothing, Nothing, ph) <- createProcess
        (proc "git" $ map unpack args)
            { cwd = mdir
            }
    ec <- waitForProcess ph
    case ec of
        ExitSuccess -> return ()
        ExitFailure i -> error $ fold
            [ "Ran git in dir "
            , show mdir
            , " with args "
            , show args
            , " failed with exit code "
            , show i
            ]
