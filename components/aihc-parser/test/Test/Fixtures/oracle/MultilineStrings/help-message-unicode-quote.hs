{- ORACLE_TEST xfail String with Unicode quote -}
module HelpMessageUnicodeQuote where

helpMessage :: Bool -> String -> String
helpMessage isTerminal name = usageInfo header options ++ footer
  where
    b = bold isTerminal
    bu = boldUnderline isTerminal
    header = "A tool to generate reports from .tix and .mix files\n\
\\n\
\" ++ bu "USAGE:" ++ " " ++ b name ++ " [OPTIONS] TARGET\n\
\\n\
\" ++ bu "ARGUMENTS:" ++ "\n\
\  <TARGET>  Either a path to a .tix file or a 'TOOL:TEST_SUITE'.\n\
\            Supported TOOL values are 'stack' and 'cabal'.\n\
\            When the TOOL is 'stack' and building a project with\n\
\            multiple packages, use 'all' as the TEST_SUITE value\n\
\            to specify the combined report.\n\
\\n\
\" ++ bu "OPTIONS:"
    footer = "\
\\n\
\For more info, see:\n\
\\n\
\  https://github.com/8c6794b6/hpc-codecov#readme\n\
\"

bold :: Bool -> String -> String
bold _ x = x

boldUnderline :: Bool -> String -> String
boldUnderline _ x = x

usageInfo :: String -> [String] -> String
usageInfo _ _ = ""

options :: [String]
options = []
