{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Which (which, staticWhich, staticWhichNix) where

import qualified Shelly as Sh
import qualified Data.Text as T
import Language.Haskell.TH (Exp, Q, reportError, runIO)
import Data.Monoid ((<>))
import Data.List (isPrefixOf)

-- | Determine which executable would run if the given path were
--   executed, or return Nothing if a suitable executable cannot be
--   found
which :: FilePath -> IO (Maybe FilePath)
which f = fmap (fmap (T.unpack . Sh.toTextIgnore)) $ Sh.shelly $ Sh.which $ Sh.fromText $ T.pack f

staticWhichImpl :: (FilePath -> Maybe String) -> FilePath -> Q Exp
staticWhichImpl test f = do
  mf' <- runIO $ which f
  case mf' of
    Nothing -> compileError $ "Could not find executable for " <> show f
    Just f' -> case test f' of
      Just err -> compileError err
      Nothing -> [| f' |]
  where
    compileError msg' = do
      let msg = "staticWhich: " <> msg'
      reportError msg
      [| error msg |]

-- | Run `which` at compile time, and substitute the full path to the executable.
staticWhich :: FilePath -> Q Exp
staticWhich = staticWhichImpl (const Nothing)

-- A variant of 'staticWhich' that ensures the executable is in the nix store.
-- This is useful in NixOS to ensure that the resulting executable
-- contains the dependency in its closure and that it refers to the
-- same version at run time as at compile time
staticWhichNix :: FilePath -> Q Exp
staticWhichNix = staticWhichImpl $ \x ->
  if "/nix/store/" `isPrefixOf` x
    then Nothing
    else Just $ "Executable was found in " <> x <> " which is not in /nix/store."
