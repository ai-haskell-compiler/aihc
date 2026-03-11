{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | This module provides management for tracking candidate files that might be a
-- root file, an expected file, or an associated file.

module Test.Tasty.Sugar.Candidates
  (
    candidateToPath
  , findCandidates
  , makeCandidate
  , candidateMatchPrefix
  , candidateMatchSuffix
  )
where

import           Control.Monad ( filterM, guard )
import           Data.Bifunctor ( first )
import qualified Data.List as DL
import           Data.Maybe ( fromMaybe, isNothing )
import           Numeric.Natural
import           System.Directory ( doesDirectoryExist, getCurrentDirectory
                                  , listDirectory, doesDirectoryExist )
import           System.FilePath ( (</>), isRelative, makeRelative
                                 , splitPath, takeDirectory, takeFileName)

import           Test.Tasty.Sugar.Iterations
import           Test.Tasty.Sugar.Types


-- | Given a CUBE and a target directory, find all files in that directory and
-- subdirectories that could be candidates for processing with tasty-sugar. Each
-- file is turned into a candidate via the 'makeCandidate' function.

findCandidates :: CUBE -> FilePath -> IO ([Either String CandidateFile])
findCandidates cube inDir =
  let collectDirEntries d =
        let recurse = takeFileName d == "*"
            top = if recurse then Just (takeDirectory d) else Nothing
            start = if recurse then takeDirectory d else d
        in dirListWithPaths top start
      dirListWithPaths topDir d =
        -- putStrLn ("Reading " <> show d) >>
        doesDirectoryExist d >>= \case
          True ->
            do dirContents <- listDirectory d
               case topDir of
                 Nothing -> do
                   let mkC = makeCandidate cube d []
                   return (Right . mkC <$> dirContents)
                 Just topdir -> do
                   let subs = filter (not . null)
                              (init
                               <$> init (splitPath
                                          $ makeRelative topdir (d </> "x")))
                   let mkC = makeCandidate cube topdir subs
                   subdirs <- filterM (doesDirectoryExist . (d </>)) dirContents
                   let here = Right . mkC <$> (filter (not . (`elem` subdirs)) dirContents)
                   subCandidates <- mapM (dirListWithPaths topDir)
                                    ((d </>) <$> subdirs)
                   return $ here <> (concat subCandidates)
          False -> do
            showD <- case isRelative d of
                       True -> do cwd <- getCurrentDirectory
                                  return $ "[" <> cwd <> "/]" <> d
                       False -> return d
            return [Left $ showD <> " does not exist"]
  in collectDirEntries inDir


-- | Create a CandidateFile entry for this top directory, sub-paths, and
-- filename.  In addition, any Explicit parameters with known values that appear
-- in the filename are captured.  Note that:
--
-- * There may be multiple possible matches for a single parameter (e.g. the
--   value is repeated in the name or path, or an undefind value (Nothing)
--   parameter could have multiple possible values extracted from the filename.
--
-- * File name matches are preferred over sub-path matches and will occlude the
--   latter.
--
-- * All possible filename portions and sub-paths will be suggested for non-value
-- * parameters (validParams with Nothing).

makeCandidate :: CUBE -> FilePath -> [String] -> FilePath -> CandidateFile
makeCandidate cube topDir subPath fName =
  let fl = DL.length fName
      isSep = (`elem` separators cube)
      firstSep = maybe fl (+1) $ DL.findIndex isSep fName
      fle = maybe fl (fl-) $ DL.findIndex isSep $ DL.reverse fName
      -- pmatches is all known parameter values found in the name or directory of
      -- the file.  Note that a single parameter with multiple values may result
      -- in multiple pmatches if more than one of the values is present in a
      -- single filename.
      pmatches = fst $ observeIAll
                 $ do p <- eachFrom "param for candidate" $ validParams cube
                      v <- eachFrom "value for param" (fromMaybe [] (snd p))
                      -- Note: there maybe multiple v values for a single p that
                      -- are matched in the name.  This is accepted here (and
                      -- this file presumably satisfies either with an Explicit
                      -- match).
                      let vl = DL.length v
                      i <- eachFrom "param starts"
                           $ DL.findIndices (`elem` (separators cube)) fName
                      let vs = i + 1
                      let ve = vs + vl
                      let chkStart = do guard $ v `elem` subPath
                                        return ((fst p, Explicit v), (0, 0))
                      if and [ ve + 1 < fl  -- v fits in fName[i..]
                             , v == DL.take vl (DL.drop vs fName)
                             ]
                         then case DL.drop ve fName of
                                (fnc:_) -> if fnc `elem` (separators cube)
                                           then return ((fst p, Explicit v), (toEnum vs, ve))
                                           else chkStart
                                [] -> chkStart
                         else chkStart
      -- pmatchArbitrary will find a parameter with an unspecified value and
      -- assigned otherwise unmatched portions of the filename to that parameter.
      pmatchArbitrary =
        case DL.find (isNothing . snd) $ validParams cube of
          Nothing -> []
          Just (p,_) ->
            let chkRange = [(firstSep, fle)]
                -- arbs is the (start,len) spans where arbitrary values could
                -- occur
                arbs = holes chkRange (snd <$> pmatches)
                -- getRange extracts a substring range from the fName
                getRange (s,e) = let s' = fromEnum s
                                 in DL.take (e - s' - 1) $ DL.drop s' fName
                -- holeVals are the separator-divided values extracted from the
                -- arbs ranges of fName.
                holeVals = let neither f a b = not $ or [f a, f b]
                               splitBySep = filter (not . all isSep)
                                            . DL.groupBy (neither isSep)
                               rangeVals r = (,r) <$> (splitBySep $ getRange r)
                           in
                             concatMap rangeVals arbs
                -- dirVals are the subdirectory elements that could be used for
                -- arbitrary value matching (i.e. they don't explicitly match).
                dirVals =
                  let pvals = getParamVal . snd . fst <$> pmatches
                  in (, (0,0)) <$> filter (not . (`elem` pvals) . Just) subPath
            in (first ((p,) . Explicit)) <$> (holeVals <> dirVals)
      pAll = pmatches <> pmatchArbitrary
      dropSeps i =
        let lst = last $ DL.group $ DL.take (fromEnum i) fName
        in case lst of
             (lh:_) -> if isSep lh
                       then i - (toEnum (length lst) - 1)
                       else i
             _ -> i
      mtchIdx = dropSeps
                $ minimum
                $ toEnum fle
                : filter (/= 0) (fst . snd <$> pAll)
  in CandidateFile { candidateDir = topDir
                   , candidateSubdirs = subPath
                   , candidateFile = fName
                   -- nub the results in case a v value appears twice in a single
                   -- file.  Sort the results for stability in testing.
                   , candidatePMatch = DL.nub $ DL.sort $ (fst <$> pAll)
                   , candidateMatchIdx = mtchIdx
                   }


-- Remove present from chkRange leaving holes.
holes :: [(Int,Int)] -> [(Natural,Int)] -> [(Natural,Int)]
holes chkRange present =
  let rmvKnown _ [] = []
      rmvKnown p@(ps,pe) ((s,e):rs) =
        if abs(fromEnum ps - fromEnum s) <= 1
        then if pe > e
             then rmvKnown (toEnum e,pe) rs
             else if abs(pe-e) <= 1
                  then rs
                  else (toEnum pe + 1, e) : rs
        else if ps >= s && fromEnum ps < e
             then if abs(pe - e) <= 1
                  then (s, fromEnum ps) : rs
                  else if pe < e
                       then (s, fromEnum ps) : (toEnum pe, e) : rs
                       else (s, fromEnum ps) : rmvKnown (toEnum e, pe) rs
             else rmvKnown p rs
      r' = filter (\x -> fst x /= snd x) chkRange
      p' = filter (\(x,y) -> not $ and [ x == 0, y == 0 ]) present
  in foldr rmvKnown (first toEnum <$> r') (DL.sort p')


-- | This converts a CandidateFile into a regular FilePath for access by standard
-- IO operations.

candidateToPath :: CandidateFile -> FilePath
candidateToPath c =
  candidateDir c </> foldr (</>) (candidateFile c) (candidateSubdirs c)


-- | Determines if the second CandidateFile argument matches the prefix of the
-- first CandidateFile, up to any separator (if applicable).  This can be used to
-- match possible expected files against the current root file, or possible
-- associated files against the current expected file.
candidateMatchPrefix :: Separators -> CandidateFile -> CandidateFile -> Bool
candidateMatchPrefix seps mf cf =
  let mStart = candidateFile mf
      mStartLen = length mStart
      f = candidateFile cf
      pfxlen = let cl = candidateMatchIdx cf
               in if fromEnum cl == length f
                  then if null seps then toEnum mStartLen else cl
                  else cl - 1
  in mStart == DL.take (fromEnum pfxlen) f


-- | Determines if the second candidate file matches the first by virtue of
-- having the same identified suffix.  If a non-null suffix is specified then
-- verify the second file is the conjunction of the first file with a separator
-- and the specified suffix with appropriate considerations for any separator in
-- the supplied suffix.  If no suffix is provided, then simply ensure that the
-- second file has no suffix.

candidateMatchSuffix :: Separators -> FileSuffix -> CandidateFile
                     -> CandidateFile -> Bool
candidateMatchSuffix seps sfx rootf cf =
  let f = candidateFile cf
      sfxsep = case sfx of
                 (sfxHead:_) -> sfxHead `elem` seps
                 _ -> False
  in if null sfx
     then f == DL.takeWhile (not . (`elem` seps)) f
     else and [ length f >= (length (candidateFile rootf) + length sfx)
              , sfx `DL.isSuffixOf` f
                -- is char before sfx a separator (and fEnd didn't start
                -- with a separator)?
              , if null seps
                then length f == length (candidateFile rootf) + length sfx
                else if sfxsep
                     then True
                     else maybe False ((`elem` seps) . fst)
                          $ DL.uncons
                          $ DL.drop (length sfx)
                          $ reverse f
              ]
