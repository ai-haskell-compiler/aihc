{- ORACLE_TEST pass -}
{-# LANGUAGE LambdaCase #-}

module LambdaCaseInfixApplication where

parseCommandLineOptions originalParsedOptions =
  if _packages transformedOptions == mempty then
    Left noPackagesError
  else
    Right transformedOptions
  where transformedOptions =
          (Options <$> __command
           <*> \case
             f | __withFlake f -> Flake
               | (hasShellArg . __shellPackages) f -> Flake
             _ | otherwise -> Traditional
           <*> __prioritiseLocalPinnedSystem) originalParsedOptions
        hasShellArg (Just ("shell":_)) = True
        hasShellArg _ = False

data Mode = Flake | Traditional
