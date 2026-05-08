{- ORACLE_TEST xfail shellify parse/pretty mismatch for \case in applicative chain -}
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
