-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- |
   Simple key/value style config file loading

   This module was motived by the desire to factor this repetitive 
   configuration file parsing code out of several of my projects.

   These functions offer very simple behavior which may be fine for 
   many tasks. For those needing something that does more, including 
   building and saving config data and .ini-style [section]s, may I 
   suggest Data.ConfigFile 
   <http://hackage.haskell.org/package/ConfigFile>.

   Example:

>     import Data.Map ( lookup )
>     import Prelude hiding ( lookup )
>     import TCE.Data.KVConf ( parseToMap )
>     
>     main = do
>        -- Parse a String into a KVConf datatype
>        conf <- parseToMap <$> readFile "kv-example.conf"
>     
>        -- It's just a map, so use Data.Map functions to access
>        print $ lookup "foo" conf
>        print $ lookup "baz-blorp" conf

   An example config file is given below.

-}
module TCE.Data.KVConf
   ( KVConf
   , parseToMap
   , parseToArgs
   )
   where

import Data.Char ( isSpace )
import Data.Map hiding ( map )
import Data.Maybe ( catMaybes )


-- | Convenience type synonym. Config data is just a simple Map
type KVConf = Map String String


{- |
   Parse config file data into a simple (Map String String).

   For example, this:

   >  --- file start ---
   >  foo=one
   >  # a comment
   >
   >  bar
   >  baz-blorp=2
   >
   >  # spaces are permitted around the =
   >  qux = false
   >  --- file end ---

   becomes:

   >  fromList [("foo","one"),("bar",""),("baz-blorp","2"),("qux","false")]

   Comments (prefixed with #) and blank lines in the config file 
   are discarded.
-}
parseToMap :: String -> KVConf
parseToMap = fromList . catMaybes . map parseKV . lines


parseKV :: String -> Maybe (String, String)
parseKV ('#' : _) = Nothing
parseKV ""        = Nothing
parseKV l         = Just
   ( trim . takeWhile p $ l
   , trim . tailSafe . dropWhile p $ l
   )
   where p = (/= '=')


tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe l  = tail l


dropFromTailWhile :: (a -> Bool) -> [a] -> [a]
dropFromTailWhile _ [] = []
dropFromTailWhile p s
  | p (last s) = dropFromTailWhile p $ init s
  | otherwise  = s


trim :: String -> String
trim = dropFromTailWhile isSpace . dropWhile isSpace


{- |
   Parse config file data into what looks like long args on a command 
   line.

   Sometimes it's convenient to be able to supply commonly used 
   long args in a config file. The idea here is you can prepend this 
   [String] to your other command line args and send the whole mess 
   to your System.Console.GetOpt-based code.

   For example, this:

   >  --- file start ---
   >  foo=one
   >  # a comment
   >
   >  bar
   >  baz-blorp=2
   >
   >  # spaces are permitted around the =
   >  qux = false
   >  --- file end ---

   becomes:

   >  [ "--foo=one", "--bar", "--baz-blorp=2", "--qux=false" ]

   As above, comments (prefixed with #) and blank lines in the config 
   file are discarded.
-}
parseToArgs :: String -> [String]
parseToArgs = map prependHyphens . map joinPair
   .  catMaybes . map parseKV . lines

   where
      prependHyphens s = '-' : '-' : s

      joinPair (k, "") = k
      joinPair (k, v ) = k ++ "=" ++ v
