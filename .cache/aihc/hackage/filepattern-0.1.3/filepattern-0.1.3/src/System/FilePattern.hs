{-# LANGUAGE ConstraintKinds, RecordWildCards, ScopedTypeVariables #-}

-- | A module for matching files using patterns such as @\"src\/**\/*.png\"@ for all @.png@ files
--  recursively under the @src@ directory. See '?==' for the semantics of
--  'FilePattern' values. Features:
--
--  * All matching is /O(n)/. Most functions precompute some information given only one argument.
--
--  * Use 'match' and 'substitute' to extract suitable
--  strings from the @*@ and @**@ matches, and substitute them back into other patterns.
--
--  * Use 'step' and 'matchMany' to perform bulk matching
--  of many patterns against many paths simultaneously.
--
--  * Use "System.FilePattern.Directory" to perform optimised directory traverals using patterns.
module System.FilePattern(
    FilePattern, (?==), match, substitute, arity,
    -- * Multiple patterns and paths
    step, step_, Step(..), StepNext(..), matchMany
    ) where

import Control.Exception.Extra
import Data.Maybe
import Data.Tuple.Extra
import Data.List.Extra
import System.FilePattern.Tree
import System.FilePattern.Core(FilePattern, parsePattern, parsePath, renderPath)
import qualified System.FilePattern.Core as Core
import System.FilePattern.Step
import Prelude


---------------------------------------------------------------------
-- PATTERNS

-- | Match a 'FilePattern' against a 'FilePath'. There are two special forms:
--
-- * @*@ matches part of a path component, excluding any separators.
--
-- * @**@ as a path component matches an arbitrary number of path components.
--
--   Some examples:
--
-- * @test.c@ matches @test.c@ and nothing else.
--
-- * @*.c@ matches all @.c@ files in the current directory, so @file.c@ matches,
--   but @file.h@ and @dir\/file.c@ don't.
--
-- * @**/*.c@ matches all @.c@ files anywhere on the filesystem,
--   so @file.c@, @dir\/file.c@, @dir1\/dir2\/file.c@ and @\/path\/to\/file.c@ all match,
--   but @file.h@ and @dir\/file.h@ don't.
--
-- * @dir\/*\/*@ matches all files one level below @dir@, so @dir\/one\/file.c@ and
--   @dir\/two\/file.h@ match, but @file.c@, @one\/dir\/file.c@, @dir\/file.h@
--   and @dir\/one\/two\/file.c@ don't.
--
--   Patterns with constructs such as @foo\/..\/bar@ will never match
--   normalised 'FilePath' values, so are unlikely to be correct.
(?==) :: FilePattern -> FilePath -> Bool
(?==) w = isJust . match w


-- | Like '?==', but returns 'Nothing' on if there is no match, otherwise 'Just' with the list
--   of fragments matching each wildcard. For example:
--
-- @
-- isJust ('match' p x) == (p '?==' x)
-- 'match' \"**\/*.c\" \"test.txt\" == Nothing
-- 'match' \"**\/*.c\" \"foo.c\" == Just [\"",\"foo\"]
-- 'match' \"**\/*.c\" \"bar\/baz\/foo.c\" == Just [\"bar\/baz/\",\"foo\"]
-- @
--
--   On Windows any @\\@ path separators will be replaced by @\/@.
match :: FilePattern -> FilePath -> Maybe [String]
match w = Core.match (parsePattern w) . parsePath


---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

-- | How many @*@ and @**@ elements are there.
--
-- @
-- 'arity' \"test.c\" == 0
-- 'arity' \"**\/*.c\" == 2
-- @
arity :: FilePattern -> Int
arity = Core.arity . parsePattern


-- | Given a successful 'match', substitute it back in to a pattern with the same 'arity'.
--   Raises an error if the number of parts does not match the arity of the pattern.
--
-- @
-- p '?==' x ==> 'substitute' (fromJust $ 'match' p x) p == x
-- 'substitute' \"**\/*.c\" [\"dir\",\"file\"] == \"dir/file.c\"
-- @
substitute :: Partial => FilePattern -> [String] -> FilePath
substitute w xs = maybe (error msg) renderPath $ Core.substitute (parsePattern w) xs
    where
        msg = "Failed substitute, patterns of different arity. Pattern " ++ show w ++
              " expects " ++ show (arity w) ++ " elements, but got " ++ show (length xs) ++
              " namely " ++ show xs ++ "."


-- | Efficiently match many 'FilePattern's against many 'FilePath's in a single operation.
--   Note that the returned matches are not guaranteed to be in any particular order.
--
-- > matchMany [(a, pat)] [(b, path)] == maybeToList (map (a,b,) (match pat path))
matchMany :: [(a, FilePattern)] -> [(b, FilePath)] -> [(a, b, [String])]
matchMany [] = const []
matchMany pats = \files -> if null files then [] else f spats $ makeTree $ map (second $ (\(Core.Path x) -> x) . parsePath) files
    where
        spats = step pats

        f Step{..} (Tree bs xs) = concat $
            [(a, b, ps) | (a, ps) <- stepDone, b <- bs] :
            [f (stepApply x) t | (x, t) <- xs, case stepNext of StepOnly xs -> x `elem` xs; _ -> True]
