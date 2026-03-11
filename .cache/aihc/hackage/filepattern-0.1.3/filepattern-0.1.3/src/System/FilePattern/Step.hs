{-# LANGUAGE ViewPatterns, DeriveFunctor, BangPatterns, TupleSections, RecordWildCards #-}

-- | Applying a set of paths vs a set of patterns efficiently
module System.FilePattern.Step(
    step, step_, Step(..), StepNext(..)
    ) where

import System.FilePattern.Core
import System.FilePattern.Tree
import System.FilePattern.Wildcard

import Control.Monad.Extra
import Data.List.Extra
import Data.Semigroup
import Data.Tuple.Extra
import Data.Functor
import Data.Either
import qualified Data.List.NonEmpty as NE
import Prelude


-- | What we know about the next step values.
data StepNext
    =
      -- | All components not listed will result in dull 'Step' values from 'stepApply',
      --   with 'stepNext' being @'StepOnly' []@ and 'stepDone' being @[]@. The field is a set - their order
      --   is irrelevant but there will be no duplicates in values arising from 'step'.
      StepOnly [String]
    | -- | All calls to 'stepApply' will return 'stepNext' being 'StepEverything' with a non-empty 'stepDone'.
      StepEverything
    | -- | We have no additional information about the output from 'stepApply'.
      StepUnknown
      deriving (Eq,Ord,Show)


mergeStepNext :: [StepNext] -> StepNext
mergeStepNext = f id
    where
        f rest [] = StepOnly $ rest []
        f rest (StepUnknown:xs) = if StepEverything `elem` xs then StepEverything else StepUnknown
        f rest (StepEverything:xs) = StepEverything
        f rest (StepOnly x:xs) = f (rest . (x ++)) xs

normaliseStepNext :: StepNext -> StepNext
normaliseStepNext (StepOnly xs) = StepOnly $ nubOrd xs
normaliseStepNext x = x


instance Semigroup StepNext where
    a <> b = sconcat $ NE.fromList [a,b]
    sconcat = normaliseStepNext . mergeStepNext . NE.toList

instance Monoid StepNext where
    mempty = StepOnly []
    mappend = (<>)
    mconcat = maybe mempty sconcat . NE.nonEmpty -- important: use the fast sconcat


-- | The result of 'step', used to process successive path components of a set of 'FilePath's.
data Step a = Step
    {stepDone :: [(a, [String])]
        -- ^ The files that match at this step. Includes the list that would have been produced by 'System.FilePattern.match',
        --   along with the values passed to 'step'. These results are not necessarily in order.
    ,stepNext :: StepNext
        -- ^ Information about the results of calling 'stepApply'. See 'StepNext' for details.
    ,stepApply :: String -> Step a
        -- ^ Apply one component from a 'FilePath' to get a new 'Step'.
    }
    deriving Functor

mergeStep :: (StepNext -> StepNext) -> [Step a] -> Step a
mergeStep f [] = mempty
mergeStep f [x] = x
mergeStep f xs = Step
    {stepDone = concatMap stepDone xs
    ,stepNext = f $ mergeStepNext $ map stepNext xs
    ,stepApply = \x -> mergeStep f $ map (`stepApply` x) xs
    }

instance Semigroup (Step a) where
    a <> b = sconcat $ NE.fromList [a,b]
    sconcat (NE.toList -> ss)
        | [s] <- ss = s
        | otherwise = Step
            {stepDone = concatMap stepDone ss
            ,stepNext = mconcatMap stepNext ss
            ,stepApply = \x -> fastFoldMap (`stepApply` x) ss
            }

instance Monoid (Step a) where
    mempty = Step [] mempty $ const mempty
    mappend = (<>)
    mconcat = maybe mempty sconcat . NE.nonEmpty -- important: use the fast sconcat

fastFoldMap :: Monoid m => (a -> m) -> [a] -> m
{- HLINT ignore fastFoldMap -}
fastFoldMap f = mconcat . map f -- important: use the fast mconcat


-- Invariant: No two adjacent Lits
-- Invariant: No empty Lits
data Pat = Lits [Wildcard String]
         | StarStar
         | End
           deriving (Show,Eq,Ord)

toPat :: Pattern -> [Pat]
toPat (Pattern (Literal xs)) = [Lits xs]
toPat (Pattern (Wildcard pre mid post)) = intercalate [StarStar] $ map lit $ pre : mid ++ [post]
    where lit xs = [Lits xs | xs /= []]


-- | Efficient matching of a set of 'FilePattern's against a set of 'FilePath's.
--   First call 'step' passing in all the 'FilePattern's, with a tag for each one.
--   Next call the methods of 'Step', providing the components of the 'FilePath's in turn.
--
--   Useful for efficient bulk searching, particularly directory scanning, where you can
--   avoid descending into directories which cannot match.
step :: [(a, FilePattern)] -> Step a
step = restore . ($ id) . f [] . makeTree . map (second $ toPat . parsePattern)
    where
        f :: [Pat] -> Tree Pat a -> (Parts -> Step [a])
        f seen (Tree ends nxts) = \parts -> mergeStep id $ map ($ parts) $ sEnds ++ sNxts
            where
                sEnds = case unroll ends (seen ++ [End]) of
                    _ | null ends -> []
                    Just ([], c) -> [c (error "step invariant violated (1)")]
                    _ -> error $ "step invariant violated (2), " ++ show seen

                sNxts = flip map nxts $ \(p,ps) ->
                    let seen2 = seen ++ [p] in
                    case unroll (error "step invariant violated (3)") seen2 of
                        Nothing -> f seen2 ps
                        Just (nxt, c) -> c (f [] $ retree nxt ps)

        retree [] t = t
        retree (p:ps) t = Tree [] [(p, retree ps t)]

        restore :: Step [a] -> Step a -- and restore the stepNext invariant
        restore Step{..} = Step
            {stepDone = [(a, b) | (as,b) <- stepDone, a <- as]
            ,stepNext = normaliseStepNext stepNext
            ,stepApply = restore . stepApply
            }

-- | Like 'step' but using @()@ as the tag for each 'FilePattern'.
step_ :: [FilePattern] -> Step ()
step_ = step . map ((),)


match1 :: Wildcard String -> String -> Maybe [String]
match1 w x = rights <$> wildcardMatch equals w x


type Parts = [String] -> [String]

-- Given a prefix of the pattern, if you can deal with it, return
-- the rest of the pattern in the prefix you didn't match, and something that given
-- a matcher for the rest of the pattern, returns a matcher for the whole pattern.
unroll :: a -> [Pat] -> Maybe ([Pat], (Parts -> Step a) -> Parts -> Step a)
-- normal path, dispatch on what you find next
unroll val [End] = Just ([], \_ parts -> mempty{stepDone = [(val, parts [])]})

-- two stars in a row, the first will match nothing, the second everything
unroll val [StarStar,StarStar] = Just ([StarStar], \cont parts -> cont (parts . ([]:)))

-- if you have literals next, match them
unroll val [Lits (l:ls)] = Just ([Lits ls | ls /= []], \cont parts -> Step
    {stepDone = []
    ,stepNext = case l of Literal v -> StepOnly [v]; Wildcard{} -> StepUnknown
    ,stepApply = \s -> case match1 l s of
        Just xs -> cont (parts . (xs++))
        Nothing -> mempty
    })

-- if anything else is allowed, just quickly allow it
unroll val [StarStar,End] = Just ([], \_ parts -> g parts [])
    where
        g parts rseen = Step
            {stepDone = [(val, parts [mkParts $ reverse rseen])]
            ,stepNext = StepEverything
            ,stepApply = \s -> g parts (s:rseen)
            }

-- if you have a specific tail prefix, find it
unroll val [StarStar,Lits (reverse &&& length -> (rls,nls)),End] = Just ([], \_ parts -> g parts 0 [])
    where
        g parts !nseen rseen = Step
            {stepDone = case zipWithM match1 rls rseen of
                _ | nseen < nls -> [] -- fast path
                Just xss -> [(val, parts $ mkParts (reverse $ drop nls rseen) : concat (reverse xss))]
                Nothing -> []
            ,stepNext = StepUnknown
            ,stepApply = \s -> g parts (nseen+1) (s:rseen)
            }

-- we know the next literal, and it doesn't have any constraints immediately after
unroll val [StarStar,Lits [l],StarStar] = Just ([StarStar], \cont parts -> g cont parts [])
    where
        g cont parts rseen = Step
            {stepDone = []
            ,stepNext = StepUnknown
            ,stepApply = \s -> case match1 l s of
                Just xs -> cont (parts . (++) (mkParts (reverse rseen) : xs))
                Nothing -> g cont parts (s:rseen)
            }

-- the hard case, a floating substring, accumulate at least N, then star testing in reverse
unroll val [StarStar,Lits (reverse &&& length -> (rls,nls)),StarStar] = Just ([StarStar], \cont parts -> g cont parts 0 [])
    where
        g cont parts !nseen rseen = Step
            {stepDone = []
            ,stepNext = StepUnknown
            ,stepApply = \s -> case zipWithM match1 rls (s:rseen) of
                _ | nseen+1 < nls -> g cont parts (nseen+1) (s:rseen) -- not enough accumulated yet
                Nothing -> g cont parts (nseen+1) (s:rseen)
                Just xss -> cont (parts . (++) (mkParts (reverse $ drop nls $ s:rseen) : concat (reverse xss)))
            }

unroll _ _ = Nothing
