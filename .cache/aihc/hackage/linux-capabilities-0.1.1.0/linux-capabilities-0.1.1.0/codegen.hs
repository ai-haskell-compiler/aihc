-- | A script to generate the data type
-- runhaskell codegen.hs | ormolu > src/System/Linux/Capabilities.hs
module Main (main) where

import Data.List
import Data.Maybe

-- | returns the list of caps with there comments
readCaps :: IO [String]
readCaps =
  readFile "/usr/include/linux/capability.h"
    >>= (pure . dropEnd . reverse . foldl' go []) . dropBegin . lines
  where
    -- The actual list starts after this line
    dropBegin = drop 1 . dropWhile (not . isInfixOf "POSIX-draft defined capabilities.")
    -- The list ends before this line
    dropEnd = takeWhile (not . isInfixOf "CAP_LAST_CAP")
    push s [] = [s]
    push s (x : xs) = (x <> s <> "\n") : xs
    go :: [String] -> String -> [String]
    go acc s
      -- Drop intermediary comment
      | s == " ** Linux-specific capabilities" = acc
      | "#define CAP_" `isPrefixOf` s = [""] <> push s acc
      | otherwise = push s acc

-- | create data type constructor with documentation
render :: String -> String
render = unlines . haddockPrefix . reverse . foldl' go [] . reverse . lines
  where
    go :: [String] -> String -> [String]
    go acc s
      | "#define CAP_" `isPrefixOf` s = ["    " <> ((!! 1) . words $ s)]
      | otherwise = case makeHaddock s of
        [] -> acc
        x -> acc <> [commentPrefix <> x]
    commentPrefix = "    -- "
    haddockPrefix (x : xs) = ("    -- | " <> stripPrefix' commentPrefix x) : xs
    haddockPrefix [] = []
    stripPrefix' :: String -> String -> String
    stripPrefix' prefix s = fromMaybe s (stripPrefix prefix s)
    makeHaddock :: String -> String
    makeHaddock s =
      stripPrefix' "   "
        . stripPrefix' " ** "
        . stripPrefix' " * "
        . dropWhileEnd (`elem` (['*', '/', ' '] :: [Char]))
        $ makeHaddockList s
    makeHaddockList s
      | "/* " `isPrefixOf` s = "- " <> drop 3 s
      | ":" `isSuffixOf` s = s <> "\n" <> commentPrefix
      | otherwise = s

p :: String -> IO ()
p = putStrLn

main :: IO ()
main = do
  p "{-# LANGUAGE DerivingStrategies #-}"
  p "-- |"
  p "-- Copyright: (c) 2021 Red Hat"
  p "-- SPDX-License-Identifier: Apache-2.0"
  p "-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>"
  p "--"
  p "module System.Linux.Capabilities (Capability (..)) where"
  p ""
  p "-- | Linux capabilities"
  p "data Capability = "
  readCaps >>= mapM_ p . intersperse "   |" . fmap render
  p "  deriving stock (Bounded, Enum, Eq, Read, Show)"
