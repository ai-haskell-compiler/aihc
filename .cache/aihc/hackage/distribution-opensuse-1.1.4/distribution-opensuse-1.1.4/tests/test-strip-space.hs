{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import OpenSuse.Prelude
import OpenSuse.StripSpace

{-# INLINE (~~>) #-}
(~~>) :: String -> String -> (Text, Text)
(~~>) input result = (packText input, packText result)

testCases :: [(Text, Text)]
testCases =
  [ "para1\n\r\n\n para2  \n\r\r\n\n" ~~> "para1\n\n para2\n"
  , "line1\nline2\r\nline3\n"         ~~> "line1\nline2\nline3\n"
  , "line1\nline2\n"                  ~~> "line1\nline2\n"
  , "line1"                           ~~> "line1\n"
  , "line 1 \r and still line 1"      ~~> "line 1 \r and still line 1\n"
  , ""                                ~~> ""
  , "\n"                              ~~> ""
  , "  \n"                            ~~> ""
  , "  \n  \n\n  \n  "                ~~> ""
  ]

main :: IO ()
main =
  forM_ testCases $ \(inp,expct) -> do
    let r = stripSpace inp
    unless (r == expct) $
      fail (unwords [ "stripped version of", show inp
                    , "is", show r
                    , "not the expected", show expct
                    ])
