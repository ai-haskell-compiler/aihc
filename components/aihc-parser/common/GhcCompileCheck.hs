module GhcCompileCheck
  ( checkFilesParseWithGhc,
  )
where

import Data.Char (toLower)
import Data.List (isInfixOf)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

checkFilesParseWithGhc :: [String] -> [FilePath] -> IO (Either String ())
checkFilesParseWithGhc extNames files = do
  if null files
    then pure (Right ())
    else do
      let args =
            [ "-fno-code",
              "-fno-write-interface",
              "-fforce-recomp",
              "-fdefer-type-errors",
              "-fdefer-out-of-scope-variables",
              "-Wno-deferred-type-errors",
              "-Wno-deferred-out-of-scope-variables",
              "-v0"
            ]
              <> map ("-X" <>) extNames
              <> files
      (code, stdoutTxt, stderrTxt) <- readProcessWithExitCode "ghc" args ""
      pure $
        case code of
          ExitSuccess -> Right ()
          ExitFailure _ ->
            if isParseErrorOutput stderrTxt
              then
                Left
                  ( unlines
                      [ "ghc reported parse errors:",
                        "ghc " <> unwords args,
                        "",
                        "stdout:",
                        stdoutTxt,
                        "",
                        "stderr:",
                        stderrTxt
                      ]
                  )
              else Right ()

isParseErrorOutput :: String -> Bool
isParseErrorOutput stderrTxt =
  let lower = map toLower stderrTxt
   in "parse error" `isInfixOf` lower
