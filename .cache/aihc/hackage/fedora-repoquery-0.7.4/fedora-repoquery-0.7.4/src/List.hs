module List (
  listVersionsCmd
  ) where

import Data.List.Extra
import SimpleCmd ((+-+))

import Release (activeFedoraReleases, BodhiRelease(..))

listVersionsCmd :: IO ()
listVersionsCmd =
  activeFedoraReleases >>= mapM_ printRelease . sortOn releaseState
  where
    printRelease (Release _version state branch _composed _postbeta) =
      putStrLn $ branch +-+ state
