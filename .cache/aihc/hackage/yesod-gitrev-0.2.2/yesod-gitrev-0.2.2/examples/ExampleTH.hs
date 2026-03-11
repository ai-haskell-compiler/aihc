module ExampleTH (ensureGitContext) where

import Language.Haskell.TH (Q, runIO, Dec)
import System.Process (callProcess)

ensureGitContext :: Q [Dec]
ensureGitContext = runIO $ do
  callProcess "git" ["init", "--quiet"]
  return []
