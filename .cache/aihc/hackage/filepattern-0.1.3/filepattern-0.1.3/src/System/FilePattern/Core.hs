
-- | The type of patterns and wildcards, and operations working on parsed versions.
module System.FilePattern.Core(
    FilePattern,
    Pattern(..), parsePattern,
    Path(..), parsePath, renderPath,
    mkParts,
    match, substitute,
    arity
    ) where

import Data.Functor
import Control.Applicative
import System.FilePattern.Wildcard
import System.FilePath (isPathSeparator)
import Data.Either.Extra
import Data.Traversable
import qualified Data.Foldable as F
import System.FilePattern.Monads
import Data.List.Extra
import Prelude


-- | A type synonym for file patterns, containing @**@ and @*@. For the syntax
--   and semantics of 'FilePattern' see 'System.FilePattern.?=='.
--
--   Most 'FilePath' values lacking literal @.@ and @..@ components are suitable as 'FilePattern' values which match
--   only that specific file. On Windows @\\@ is treated as equivalent to @\/@.
--
--   You can write 'FilePattern' values as a literal string, or build them
--   up using the operators '<.>' and '</>' (but be aware that @\"\" '</>' \"foo\"@ produces @\"./foo\"@).
type FilePattern = String


newtype Path = Path [String]
    deriving (Show,Eq,Ord)

newtype Pattern = Pattern (Wildcard [Wildcard String])
    deriving (Show,Eq,Ord)


-- [Note: Split on ""]
--
-- For parsing patterns and paths, "" can either be [] or [""].
-- Assuming they are consistent, the only cases that are relevant are:
--
-- > match "" "" = Just []
-- > match "*" "" = if [] then Nothing else Just [""]
-- > match "**" "" = if [] then Just [] else Just [""]
--
-- We pick "" splits as [""] because that is slightly more permissive,
-- follows the builtin semantics of split, and matches the 'filepath'
-- library slightly better.

parsePath :: FilePath -> Path
parsePath = Path . split isPathSeparator

renderPath :: Path -> FilePattern
renderPath (Path x) = intercalate "/" x

parsePattern :: FilePattern -> Pattern
parsePattern = Pattern . fmap (map $ f '*') . f "**" . split isPathSeparator
    where
        f :: Eq a => a -> [a] -> Wildcard [a]
        f x xs = case split (== x) xs of
            pre:mid_post -> case unsnoc mid_post of
                Nothing -> Literal pre
                Just (mid, post) -> Wildcard pre mid post


-- [Note: Conversion of parts to String]
--
-- The match of * is String, but the match for ** is really [String].
-- To simplify the API, since everything else is String encoding [String],
-- we want to convert that [String] to String. We considered 3 solutions.
--
-- 1) Since we know the elements of [String] don't contain /, a natural
-- solution is to insert / characters between items with intercalate, but that
-- doesn't work because [] and [""] end up with the same representation, but
-- are very different, e.g.
--
-- > match "**/a" "a"  = Just []
-- > match "**/a" "/a" = Just [""]
--
-- 2) We can join with "/" after every component, so ["a","b"] becomes
-- "a/b/". But that causes / characters to appear from nowhere, e.g.
--
-- > match "**" "a" = Just ["a/"]
--
-- 3) Logically, the only sensible encoding for [] must be "". Because [""]
-- can't be "" (would clash), it must be "/". Therefore we follow solution 2 normally,
-- but switch to solution 1 iff all the components are empty.
-- We implement this scheme with mkParts/fromParts.
--
-- Even after all that, we still have weird corner cases like:
--
-- > match "**" "/" = Just ["//"]
--
-- But the only realistic path it applies to is /, which should be pretty rare.


mkParts :: [String] -> String
mkParts xs | all null xs = replicate (length xs) '/'
           | otherwise = intercalate "/" xs

fromParts :: String -> [String]
fromParts xs | all isPathSeparator xs = replicate (length xs) []
             | otherwise = split isPathSeparator xs

match :: Pattern -> Path -> Maybe [String]
match (Pattern w) (Path x) = f <$> wildcardMatch (wildcardMatch equals) w x
    where
        f :: [Either [[Either [()] String]] [String]] -> [String]
        f (Left x:xs) = rights (concat x) ++ f xs
        f (Right x:xs) = mkParts x : f xs
        f [] = []


substitute :: Pattern -> [String] -> Maybe Path
substitute (Pattern w) ps = do
    let inner w = concat <$> wildcardSubst getNext pure w
        outer w = concat <$> wildcardSubst (fromParts <$> getNext) (traverse inner) w
    (ps, v) <- runNext ps $ outer w
    if null ps then Just $ Path v else Nothing


arity :: Pattern -> Int
arity (Pattern x) = sum $ wildcardArity x : map wildcardArity (concat $ F.toList x)
