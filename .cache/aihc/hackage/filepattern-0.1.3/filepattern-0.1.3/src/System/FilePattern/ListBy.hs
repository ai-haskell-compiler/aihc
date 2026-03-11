
-- | Operations on lists, generated to an arbitrary generating equality
module System.FilePattern.ListBy(
    eqListBy, stripPrefixBy, stripSuffixBy, stripInfixBy
    ) where

import Control.Applicative
import Data.Tuple.Extra


eqListBy :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
eqListBy _ [] [] = Just []
eqListBy eq (a:as) (b:bs) = liftA2 (:) (eq a b) (eqListBy eq as bs)
eqListBy _ _ _ = Nothing


stripPrefixBy :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe ([c], [b])
stripPrefixBy eq [] bs = Just ([], bs)
stripPrefixBy eq _  [] = Nothing
stripPrefixBy eq (a:as) (b:bs) = do c <- eq a b; first (c:) <$> stripPrefixBy eq as bs

stripSuffixBy :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe ([b], [c])
stripSuffixBy eq [] bs = Just (bs, []) -- shortcut, but equal to the equation below
stripSuffixBy eq _  [] = Nothing       -- shortcut, but equal to the equation below
stripSuffixBy eq as bs = (\(c,b) -> (reverse b, reverse c)) <$> stripPrefixBy eq (reverse as) (reverse bs)

stripInfixBy :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe ([b], [c], [b])
stripInfixBy eq needle haystack | Just (ans, rest) <- stripPrefixBy eq needle haystack = Just ([], ans, rest)
stripInfixBy eq needle [] = Nothing
stripInfixBy eq needle (x:xs) = (\(a,b,c) -> (x:a,b,c)) <$> stripInfixBy eq needle xs
