module Text.Heredoc (here, there, str) where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Language.Haskell.TH
  ( litE
  , stringL
  )

import Language.Haskell.TH.Quote
  ( QuasiQuoter
    ( QuasiQuoter
    , quoteExp
    , quotePat
    , quoteType
    , quoteDec
    )
  , quoteFile
  )

data Ctx = Exp | Pat | Type | Dec

qq :: String -> Ctx -> QuasiQuoter
qq qqName correctCtx = QuasiQuoter
  { quoteExp  = const $ error $ errorString Exp
  , quotePat  = const $ error $ errorString Pat
  , quoteType = const $ error $ errorString Type
  , quoteDec  = const $ error $ errorString Dec
  }
  where
    errorString ctx =
      "You have used the `" ++ qqName ++ "` QuasiQuoter " ++
      "in " ++ ctxName ctx ++ " context; " ++
      "you must only use it in " ++ ctxName correctCtx ++ " context"

    ctxName c = case c of
      Exp  -> "an expression"
      Pat  -> "a pattern"
      Type -> "a type"
      Dec  -> "a declaration"


toUnix :: String -> String
toUnix cs = case cs of
  '\r':'\n' : cs -> '\n' : toUnix cs
  '\r'      : cs -> '\n' : toUnix cs
  c         : cs -> c    : toUnix cs
  []             -> []

{-| Create a string-literal expression from the string being quoted.

    Newline literals are normalized to UNIX newlines (one '\n' character).
-}
here :: QuasiQuoter
here = (qq "here" Exp) { quoteExp  = litE . stringL . toUnix }

{-| Create a string-literal expression from
    the contents of the file at the filepath being quoted.

    Newline literals are normalized to UNIX newlines (one '\n' character).
-}
there :: QuasiQuoter
there = quoteFile here

{-| Create a multi-line string literal whose left edge is demarcated by the
    "pipe" character ('|'). For example,

    >famousQuote = [str|Any dictator would admire the
    >                  |uniformity and obedience of the U.S. media.
    >                  |
    >                  |    -- Noam Chomsky
    >                  |]

    is functionally equivalent to

    >famousQuote = "Any dictator would admire the\n" ++
    >              "uniformity and obedience of the U.S. media.\n" ++
    >              "\n" ++
    >              "    -- Noam Chomsky\n"

    If desired, you can have a ragged left-edge, so

    >myHtml = [str|<html>
    >                 |<body>
    >                     |<h1>My home page</h1>
    >                 |</body>
    >             |</html>
    >             |]

    is functionally equivalent to

    >myHtml = "<html>\n" ++
    >         "<body>\n" ++
    >         "<h1>My home page</h1>\n" ++
    >          "</body>\n" ++
    >         "</html>\n"
-}
str :: QuasiQuoter
str = (qq "str" Exp)
      { quoteExp = litE . stringL . intercalate "\n" . unPipe . lines . toUnix }
  where
    unPipe ls = case ls of
      []     -> []
      l : ls -> l : case splitLast ls of
        Nothing              -> []
        Just (middles, last) ->
          map removePipe middles ++ [fromMaybe "" (tryRemovePipe last)]
            where
            removePipe cs = case tryRemovePipe cs of
              Nothing -> error "no pipe character found in line '" ++ cs ++ "'"
              Just cs -> cs

            tryRemovePipe cs = case dropWhile (/='|') cs of
              []   -> Nothing
              c:cs -> Just cs

    splitLast :: [a] -> Maybe ([a], a)
    splitLast xs = case reverse xs of
      []  -> Nothing
      l:i -> Just (reverse i, l)
