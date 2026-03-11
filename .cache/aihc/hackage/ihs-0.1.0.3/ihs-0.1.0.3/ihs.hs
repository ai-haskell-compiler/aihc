#!/usr/bin/env runhaskell
module Main (main) where

import Data.Char (isSpace)
import Data.List (isSuffixOf, isPrefixOf, partition, dropWhileEnd, stripPrefix)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import System.Process (readProcess)

data Block
  = Comment
  | Code
  | Output
  deriving (Show)

data Strip
  = Strip
  | Keep
  deriving (Show)

data Token
  = String String
  | Block  Block Strip Strip String
  deriving (Show)

type Indent = NonEmpty Int

dropSpace, dropSpaceBegin, dropSpaceEnd :: String -> String
dropSpace      = dropSpaceBegin . dropSpaceEnd
dropSpaceBegin = dropWhile isSpace
dropSpaceEnd   = dropWhileEnd isSpace

parse :: String -> [Token]
parse ('{':'{':'-':'-':s) = parseBlock (Block Comment Strip) s
parse ('{':'{':'-':'=':s) = parseBlock (Block Output  Strip) s
parse ('{':'{':'=':s)     = parseBlock (Block Output  Keep)  s
parse ('{':'{':'-':s)     = parseBlock (Block Code    Strip) s
parse ('{':'{':s)         = parseBlock (Block Code    Keep)  s
parse (c:s)               = parseString [c] s
parse []                  = []

parseString :: String -> String -> [Token]
parseString s []            = [String (reverse s)]
parseString s t@('{':'{':_) = String (reverse s) : parse t
parseString t (c:s)         = parseString (c:t) s

parseBlock :: (Strip -> String -> Token) -> String -> [Token]
parseBlock f s = go [] s
  where go a ('-':'}':'}':t) = f Strip (reverse a) : parse t
        go a ('}':'}':t)     = f Keep  (reverse a) : parse t
        go a (c:t)           = go (c:a) t
        go _ []              = error "Expected end of block }}"

strip :: [Token] -> [Token]
strip (String s : Block b Strip r c : ts) =
  case dropSpaceEnd s of
    "" -> strip (stripBlock b Keep r c : ts)
    s' -> String s' : strip (stripBlock b Keep r c : ts)
strip (Block b l Strip c : String s : ts) =
  case dropSpaceBegin s of
    "" -> stripBlock b l Keep c : strip ts
    s' -> stripBlock b l Keep c : strip (String s' : ts)
strip (Block b l r c : ts) = stripBlock b l r c : strip ts
strip (t : ts) = t : strip ts
strip []       = []

stripBlock :: Block -> Strip -> Strip -> String -> Token
stripBlock b l r = Block b l r . dropSpace

indent :: Indent -> String -> ShowS
indent (n:|_) s | n <= 0    = (s ++) . ('\n':)
                | otherwise = (unlines (map (replicate n ' ' ++) (lines s)) ++)

renderCode :: Indent -> String -> [Token] -> ShowS
renderCode (_:|n:ns) "end" ts = render (n:|ns) ts
renderCode (_:|[])   "end" _  = error "Unexpected {{end}} block"
renderCode ns@(n:|ns') c ts
  | "let" `isPrefixOf` c && "do" `isSuffixOf` c = go ns c (n + 4 <| ns)
  | "let" `isPrefixOf` c && "="  `isSuffixOf` c = go ns (c ++ " do") (n + 4 <| ns)
  | "else" `isPrefixOf` c                       = go (n - 1 :| ns') (c ++ " do") ns
  | any (`isSuffixOf` c) ["then", "->"]         = go ns (c ++ " do") (n + 1 <| ns)
  | "do" `isSuffixOf` c                         = go ns c (n + 1 <| ns)
  | otherwise                                   = go ns c ns
  where go ns1 c1 ns2 = indent ns1 c1 . render ns2 ts

render :: Indent -> [Token] -> ShowS
render n = go
  where go (Block Comment _ _ _ : ts) = go ts
        go (Block Code   _ _ c : ts)  = renderCode n c ts
        go (Block Output _ _ c : ts)  = indent n ("render (" ++ c ++ ")") . go ts
        go (String s : ts)            = indent n ("render " ++ show s)    . go ts
        go []                         = id

isImport :: Token -> Bool
isImport (Block Code _ _ c) = "import " `isPrefixOf` c
isImport _ = False

renderModule :: [String] -> [Token] -> String
renderModule defs toks = let (imports, code) = partition isImport toks in
  "{-# LANGUAGE FlexibleInstances #-}\n\
  \{-# LANGUAGE DefaultSignatures #-}\n\
  \import Control.Monad\n\
  \import Control.Applicative\n\
  \import Data.Int\n\
  \import Data.Word\n\
  \import Data.Foldable\n\
  \import Data.Traversable\n"
  ++ render (0:|[]) imports "" ++
  "class Render a where\n\
  \  render :: a -> IO ()\n\
  \  default render :: Show a => a -> IO ()\n\
  \  render = putStr . show\n\
  \instance Render Char where render = putChar\n\
  \instance Render [Char] where render = putStr\n\
  \instance Render a => Render (IO a) where render = (>>= render)\n\
  \instance Render Int\n\
  \instance Render Int8\n\
  \instance Render Int16\n\
  \instance Render Int32\n\
  \instance Render Int64\n\
  \instance Render Word\n\
  \instance Render Word8\n\
  \instance Render Word16\n\
  \instance Render Word32\n\
  \instance Render Word64\n\
  \instance Render Integer\n\
  \instance Render Float\n\
  \instance Render Double\n\
  \main :: IO ()\n\
  \main = do\n"
  ++ foldMap (\d -> " let " ++ d ++ "\n") defs
  ++ render (1:|[]) code ""

main :: IO ()
main = do
  (opts,args) <- partition (isPrefixOf "-") <$> getArgs
  let compile = "-c" `elem` opts
      defs = mapMaybe (stripPrefix "-D") opts
  case args of
    [file] -> do
      code <- renderModule defs . strip . parse <$> readFile file
      if compile
        then putStr code
        else readProcess "runhaskell" [] code >>= putStr
    _ -> error "Usage: ihs [-c] FILE"
