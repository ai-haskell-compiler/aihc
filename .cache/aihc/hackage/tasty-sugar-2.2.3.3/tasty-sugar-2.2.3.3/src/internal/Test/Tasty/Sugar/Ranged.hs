-- | Provides the rangedParam and rangedParamAdjuster helper functions.

{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Sugar.Ranged
  ( rangedParam
  , rangedParamAdjuster
  )
where

import           Control.Applicative ( liftA2 )
import           Control.Monad.IO.Class ( MonadIO )
import           Data.Function ( on )
import qualified Data.List as L
import           Data.Maybe ( isNothing )
import qualified Data.Set as Set

import           Test.Tasty.Sugar.Types


-- | Given a Parameter Name and a boolean that indicates valid/not-valid for a
-- Parameter Value, update the expectations in the Sweets to treat the parameter
-- as a ranged value.
--
-- [This is the pure internals version; the recommended usage is via the
-- 'rangedParamAdjuster' wrapper specification in the 'sweetAdjuster' field of
-- the 'CUBE' structure.]
--
-- Normal sweets results expect a 1:1 match between parameter value and the
-- expected file markup, but this function modifies the sweets results to
-- accomodate a parameter range with range boundaries.  For example, if the test
-- might vary the output based on the version of clang used to compile the file,
-- the 'CUBE' might specify:
--
-- > mkCUBE { rootName = "*.c"
-- >        , expectedSuffix = "good"
-- >        , validParams = [ ("clang-range", Just ["pre_clang11", "pre_clang13" ] ) ]
-- >        ...
-- >        }
--
-- Then if the following files were present:
--
-- > foo.c
-- > foo-pre_clang11.good
-- > foo.good
--
-- Then a normal sweets response would include the expectations:
--
--  > foo-pre_clang11.good ==> Explicit "pre_clang11"
--  > foo.good             ==> Assumed  "pre_clang13"
--
-- The 'Test.Tasty.Sugar.withSugarGroups' callback would then be invoked with
-- these two expectations.  The callback might check the actual version of clang
-- available to run in the environment.  If it detected clang version 10 was
-- available, the best file would be the @foo-pre_clang11.good@, even though the
-- parameters didn't mention @clang9@ and the @foo.good@ would be the usual match
-- to pick when there was no explicit match.
--
-- To handle this case, the 'rangedParam' function is used to filter the sweets,
-- and is also given the version of clang locally available:
--
-- > let rangedSweets = rangedParam "clang-range" extract (<=) (Just "9") sweets
-- >     extract = readMaybe . drop (length "pre-clang")
-- > withSugarGroups rangedSweets TT.testGroup $ \sweet instnum exp ->
-- >   ... generate test ...
--
-- Where the above would result in a single call to the _generate test_ code with
-- the @foo-pre_clang11.good@ expectation.  The @extract@ function removes the
-- numeric value from the parameter value, and the @<=@ test checks to see if the
-- version supplied is less than or equal to the extracted parameter value.
--
-- The @>@ comparator could be used if the validParams values specified a lower
-- limit instead of an upper limit, and the comparator and extractor can be
-- extended to handle other ways of specifying ranges.
--
-- If the extract function returns Nothing, then the corresponding parameter
-- value is /not/ a ranged parameter value (there can be a mix of ranged values
-- and non-ranged values), and the corresponding value(s) will be used whenever
-- there is not a ranged match.  As an example, if the 'validParams' above was
-- extended with a "recent-clang" value; for actual clang versions up through 12
-- one of the pre_clang values provides the ranged match, but for clang versions
-- of 13 or later, there is no pre_clang match so recent-clang will be used.
-- Providing a non-extractable parameter value is recommended as the default to
-- select when no ranged value is applicable; the expected file does /not/ need
-- to have the same parameter value since a weak match (no parameter match) file
-- will match with the 'Assumed' value, which will be selected if no better
-- ranged match is applicable.

rangedParam :: Enum a => Ord a
            => String -> (String -> Maybe a) -> (a -> a -> Bool)
            -> Maybe a
            -> CUBE -> [Sweets] -> [Sweets]
rangedParam pname extractVal cmpVal targetVal cube sweets =
  let adj sweet = let exps = expected sweet
                  in sweet { expected = adjustExp exps }

      -- extracts all parameters except the named parameter
      paramsExceptPName = filter ((pname /=) . fst) . expParamsMatch

      -- Compares two assoc-lists for equality on the union of both.
      assocUnionEq = \case
        [] -> const True
        ((an,av):as) -> \case
          [] -> True
          bs -> case lookup an bs of
                  Nothing -> assocUnionEq as bs
                  Just bv -> av == bv && assocUnionEq as bs

      -- This divides a list into clusters of lists, where each sub-list contains
      -- members that satisfy a comparison predicate between the list members
      -- (comparing against the first member of each sub-list).  This is
      -- effectively List.groupBy, but with global clustering instead of local
      -- clustering.
      --
      -- > Data.List.groupBy (==) "Mississippi" = ["M", "i", "ss", "i", "ss" ...]
      -- > cluster (==) "Mississippi" = ["M", "iiii", "ssss", "pp"]
      clusterBy equiv = \case
        [] -> []
        (x:xs) -> let (same,diff) = L.partition (equiv x) xs
                  in (x:same) : clusterBy equiv diff

      adjustExp :: [Expectation] -> [Expectation]
      adjustExp exps = concatMap expInRange
                       $ clusterBy (assocUnionEq `on` paramsExceptPName) exps

      notRange e = maybe False
                   (isNothing . extractVal)
                   (getParamVal =<< lookup pname (expParamsMatch e))

      expInRange :: [Expectation] -> [Expectation]
      expInRange =
        case targetVal of
          Nothing ->
            -- User did not specify which target version of clang was desired.
            -- Iterate through the possible parameter values, extract the version
            -- associated with each, and return the expectations that would have
            -- been chosen for that version.  Also use a version that is the succ
            -- of the highest and the pred of the lowest, to ensure
            -- out-of-known-range values are also considered.
            case lookup pname (validParams cube) of
              Nothing ->
                -- Should not happen: this means the user called rangedParam with
                -- a parameter name that is not an actual parameter.  In this
                -- case, just return the inputs.
                id
              Just Nothing ->
                -- Cannot support ranges on existentials (parameters whose value
                -- can be *anything*).  This can happen if the user specifies a
                -- parameter name of this type.  In this case, there is no
                -- meaningful range that can be predicted, so just return the
                -- inputs
                id
              Just (Just vals) -> \exps ->
                -- Iterate through the possible values to extract the
                -- corresponding parameter value.  This may be a subset of the
                -- actual values that could be encountered, but it at least
                -- allows the proper expected file to be determined for this set
                -- of values.  For possible values that do not have a valid
                -- extraction, just pass those Expectation entires through
                -- directly.
                let withPVal = \case
                      Nothing -> filter notRange exps
                      Just v -> expInRangeFor v exps
                    vals' = Set.fromList (extractVal <$> vals)
                            -- Use a Set to eliminate duplicates, especially of
                            -- Nothing results.
                    vals'' = let vs = Nothing `Set.delete` vals'
                                 lower = pred <$> minimum vs
                                 higher = succ <$> maximum vs
                             in if Set.null vs
                                then vs
                                else lower `Set.insert` (higher `Set.insert` vs)
                in Set.toList $ Set.unions
                   -- Set operations combine/eliminate identical results
                   $ foldr (Set.insert . Set.fromList . withPVal) mempty vals''
          Just tv -> expInRangeFor tv

      -- expInRangeFor :: a -> [Expectation] -> [Expectation]
      expInRangeFor tgtVal exps =
            -- Find the expectations with the _cmpVal-est_ Explicit that is still
            -- a _cmpVal_ of the input value than the target value.  If none
            -- exist, use the expectations that Assume the target value.  There
            -- can be multiple matches because of differences in other parameter
            -- values; stated another way: for any set of parameter values, find
            -- the expectations with the cmpVal-est Explicit ...
            let explParam e = case lookup pname $ expParamsMatch e of
                                Just (Explicit v) ->
                                  maybe False (cmpVal tgtVal) $ extractVal v
                                _ -> False
                okParam e = case lookup pname $ expParamsMatch e of
                              Just (Assumed v) ->
                                  maybe False (cmpVal tgtVal) $ extractVal v
                              _ -> False
                pval e = do pm <- lookup pname $ expParamsMatch e
                            pv <- getParamVal pm
                            extractVal pv
                -- bestsBy finds the testVal-est value for each set of
                -- expectations whose other parameter values are the same.
                bestsBy getVal testVal = \case
                  [] -> []
                  (xp:xps) ->
                    let chk e bests =
                          -- e is an Expectation, bests is the best testVal-est
                          -- [Expectation] collected so-far.
                          let ev = getVal e
                              ep = paramsExceptPName e
                              matchE = assocUnionEq ep . paramsExceptPName
                              (yes,oBest) = L.partition matchE bests
                                -- yes is the entries in bests whose non-PName
                                -- parameters match e, so we can now determine if
                                -- yes or z is testVal-est (yes may have multiple
                                -- entries, but if it does they should have the
                                -- same value for pname, which mostly happens on
                                -- the Nothing case... param does not exist or
                                -- has NotSpecified value).
                                --
                                -- oBest has the other entries in bests that
                                -- don't match e and should therefore just be
                                -- passed through.  Note that due to adjustExp
                                -- this should usually be a null list.
                              yv = case yes of
                                     (yh:_) -> getVal yh
                                     [] -> error "yes nullity is checked before getting yv"
                          in case () of
                               _ | null yes -> e:bests
                               _ | ev == yv -> e:bests
                               _ -> case liftA2 testVal yv ev of
                                      Just True -> bests
                                      Just False -> e:oBest
                                      Nothing -> e:oBest
                                             -- maybe bests (const (e:oBest)) ev
                    in foldr chk [xp] xps
                exps' = let expl = L.filter explParam exps
                            assum = L.filter okParam exps
                            nonRanged = L.filter notRange exps
                        in if null expl
                           then if null assum
                                then nonRanged
                                else assum
                           else expl
            in bestsBy pval cmpVal $ exps'
  in adj <$> sweets


-- | Given a Parameter Name and a boolean that indicates valid/not-valid for a
-- Parameter Value, update the expectations in the Sweets to treat the parameter
-- as a ranged value.  This provides the functionality described by the
-- 'rangedParam' function and is intended for use via the 'sweetAdjuster' field
-- of the 'CUBE' structure.

rangedParamAdjuster :: Enum a => Ord a
                    => MonadIO m
                    => String -> (String -> Maybe a) -> (a -> a -> Bool)
                    -> Maybe a
                    -> CUBE -> [Sweets] -> m [Sweets]
rangedParamAdjuster pname extractVal cmpVal targetVal cube =
  return . rangedParam pname extractVal cmpVal targetVal cube
