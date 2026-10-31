module ResponseFileChecks (responseFileChecks) where

import GHC.ResponseFile (escapeArgs, expandResponse, getArgsWithResponseFiles, unescapeArgs)
import System.Environment (getExecutablePath, withArgs)

responseFileChecks :: IO Bool
responseFileChecks = do
  executable <- getExecutablePath
  let path = executable ++ ".response"
      arguments = ["two words", "single'quote", "double\"quote", "back\\slash", "line\nbreak", "λ界"]
      text = "--one 1 --'two' \"two words\" escaped\\ space @nested\n"
      expected = ["--one", "1", "--two", "two words", "escaped space", "@nested"]
  writeFile path text
  expanded <- expandResponse ["before", '@' : path, "after"]
  actual <- withArgs ['@' : path] getArgsWithResponseFiles
  pure
    ( expanded == ["before"] ++ expected ++ ["after"]
        && actual == expected
        && unescapeArgs (escapeArgs arguments) == arguments
        && escapeArgs ["a b", "'\"\\"] == "a\\ b\n\\'\\\"\\\\\n"
        && unescapeArgs "  '' \"\" a\t b\r\nc\\" == ["a", "b", "c"]
        && unescapeArgs "'a\\ b' \"c'd\" 'unfinished" == ["a b", "c'd", "unfinished"]
    )
