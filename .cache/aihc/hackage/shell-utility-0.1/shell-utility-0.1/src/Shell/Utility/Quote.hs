module Shell.Utility.Quote (always, minimal, lazy) where

mustEscape, mustQuote :: [Char]
mustEscape = "\"$`\\!"
mustQuote = "' \t\n|&;()<>{}[]*?^#"


{- |
Escape a single character if it is necessary to escape it even within quotes.
The exclamation mark is also escaped
for compatibility with the history expansion misfeature of Bash.
-}
escapeChar :: Char -> String
escapeChar c = (if elem c mustEscape then ('\\':) else id) [c]


enclose :: String -> String
enclose txt = '"' : txt ++ '"' : []

{- |
Put a string in quotes and escape characters as necessary.
This allows you to construct shell commands
such that a shell interprets the arguments in the right way.
-}
always :: String -> String
always = enclose . concatMap escapeChar

{- |
Like 'always' but encloses in quotes only if necessary.
-}
minimal :: String -> String
minimal txt =
   if null txt || any (flip elem mustQuote) txt
      then always txt
      else concatMap escapeChar txt

{- |
Similar to 'minimal' but starts quoting only as soon as it becomes necessary.
This is lazy both with respect to quoting and with respect to processing.
-}
lazy :: String -> String
lazy "" = "\"\""
lazy txt =
   let go "" = ""
       go str@(c:cs) =
         if elem c mustQuote then always str else escapeChar c ++ go cs
   in go txt
