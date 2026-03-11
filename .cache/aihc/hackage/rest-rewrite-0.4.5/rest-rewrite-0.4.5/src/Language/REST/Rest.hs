

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-error=deprecations #-}

-- | This module contains the core REST algorithm
module Language.REST.Rest (
    rest
  , pathsResult
  , termsResult
  , PathsResult(..)
  , TermsResult
  , WorkStrategy(..)
  , RESTParams(..)
  , RESTResult(..)
  ) where

import           Control.Monad
import           Control.Monad.Trans
import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.List    as L
import qualified Data.HashMap.Strict     as M
import qualified Data.Maybe   as Mb

import Language.REST.OCAlgebra as AC
import Language.REST.RewriteRule
import Language.REST.Path
import Language.REST.ExploredTerms as ET
import Language.REST.Internal.ListT
import Language.REST.Internal.WorkStrategy

-- | The set of all 'Path's explored by REST.
newtype PathsResult rule term oc = PathsResult (S.HashSet (Path rule term oc))

-- | The set of all terms explored by REST.
newtype TermsResult rule term oc = TermsResult (S.HashSet term)

-- | An initial (empty) instance of 'PathsResult'
pathsResult :: PathsResult rule term oc
pathsResult = PathsResult S.empty

-- | An initial (empty) instance of 'TermsResult'
termsResult :: TermsResult rule term oc
termsResult = TermsResult S.empty

--  | This class encapsulates the mechanism for REST to store the result of its computation.
-- For example, we include two instances: 'PathsResult', which stores each 'Path' generated
-- by REST (useful for debugging and visualization); and 'TermsResult', which only stores the
-- resulting terms (which uses less memory and is likely more performant).
class RESTResult a where
  -- | Includes a term in the result
  includeInResult :: (Hashable oc, Eq oc, Hashable rule, Eq rule, Hashable term, Eq term) => Path rule term oc -> a rule term oc -> a rule term oc
  -- | Obtains the terms explored by REST
  resultTerms :: (Eq term, Hashable term) => a rule term oc -> S.HashSet term

instance RESTResult PathsResult where
  includeInResult p (PathsResult s) = PathsResult (S.insert p s)
  resultTerms (PathsResult s) = S.fromList (concatMap pathTerms $ S.toList s)

instance RESTResult TermsResult where
  includeInResult p (TermsResult s) = TermsResult (S.union s (S.fromList $ pathTerms p))
  resultTerms (TermsResult s)       = s


data RESTState m rule term oc et rtype = RESTState
  { finished   :: rtype rule term oc
  , working    :: [Path rule term oc]
  , explored   :: ExploredTerms term oc m
  , targetPath :: Maybe (Path rule term oc)
  }

data RESTParams m rule term oc rtype = RESTParams
  { re           :: S.HashSet rule
  , ru           :: S.HashSet rule
  , target       :: Maybe term
  , workStrategy :: WorkStrategy rule term oc
  , ocImpl       :: OCAlgebra oc term m
  , initRes      :: rtype rule term oc
  , etStrategy   :: ExploreStrategy
  }

-- @rest params terms@ performs the REST search from initial term @term@ with parameters@params@.
rest :: forall m rule term oc rtype .
  ( MonadIO m
  , RewriteRule m rule term
  , Hashable term
  , Eq term
  , Hashable rule
  , Hashable oc
  , Eq rule
  , Eq oc
  , Show oc
  , RESTResult rtype)
  => RESTParams m rule term oc rtype
  -> term
  -> m (rtype rule term oc, Maybe (Path rule term oc))
rest RESTParams{re,ru,ocImpl,workStrategy,initRes,target,etStrategy} t =
  rest' (RESTState initRes [([], PathTerm t S.empty)] initET Nothing)
  where
    (WorkStrategy ws) = workStrategy
    initET = ET.empty (EF (AC.union ocImpl) (AC.notStrongerThan ocImpl) (refine ocImpl)) etStrategy

    rest' (RESTState fin [] _ targetPath)            = return (fin, targetPath)
    rest' state@(RESTState _   paths et (Just targetPath))
      | ((steps, _), remaining) <- ws paths et
      , length steps >= length (fst targetPath)
      = rest' state{working = remaining}
    rest' state@(RESTState fin paths et targetPath) = do
      se <- shouldExplore ptTerm lastOrdering et
      if se
        then do
          evalRWs <- candidates re
          userRWs <- candidates ru
          acceptedUserRWs <- accepted userRWs
          go evalRWs userRWs acceptedUserRWs
        else
          rest' (state{ working = remaining })
      where

        (path@(ts, PathTerm ptTerm _), remaining) = ws paths et

        lastOrdering :: oc
        lastOrdering = if L.null ts then top ocImpl else ordering $ last ts

        tsTerms :: [term]
        tsTerms = pathTerms path

        liftSet :: S.HashSet a -> ListT m a
        liftSet s = ListT $ return $ S.toList s

        candidates :: S.HashSet rule -> m (S.HashSet (term, rule))
        candidates rules = fmap S.fromList res
          where
            res :: m [(term, rule)]
            res = runListT $ do
              r   <- liftSet rules
              t'  <- ListT $ S.toList <$> apply ptTerm r
              return (t', r)

        accepted :: S.HashSet (term, rule) -> m (M.HashMap term oc)
        accepted userRWs = M.fromList <$> runListT (do
          t' <- liftSet $ S.map fst userRWs
          guard $ L.notElem t' tsTerms
          let ord = refine ocImpl lastOrdering ptTerm t'
          ok <- lift $ isSat ocImpl ord
          guard ok
          return (t', ord))

        go evalRWs userRWs acceptedUserRewrites =
          do
            ep <- evalPaths
            up <- userPaths
            rest' (state' (ep ++ up))
          where

            state' p' = state
              { working  = p' ++ remaining
              , finished = if null p' then includeInResult (ts, pt) fin else fin
              , explored =
                  let
                    deps = S.map fst (S.union evalRWs userRWs)
                  in
                    ET.insert ptTerm lastOrdering deps et
              , targetPath =
                if Just ptTerm == target then
                  case targetPath of
                    Just (tp, _) | length tp <= length ts -> targetPath
                    _                                     -> Just (ts, pt)
                else
                  targetPath
              }


            pt = PathTerm ptTerm rejectedUserRewrites

            rejectedUserRewrites :: S.HashSet (term, rule)
            rejectedUserRewrites = S.fromList $ do
              (t', r) <- S.toList userRWs
              guard $ L.notElem t' tsTerms
              guard $ Mb.isNothing $ M.lookup t' acceptedUserRewrites
              return (t', r)


            evalPaths = runListT $ do
              (t', r) <- ListT $ return (S.toList evalRWs)
              guard $ L.notElem t' tsTerms
              let ord = refine ocImpl lastOrdering ptTerm t'
              lift (shouldExplore t' ord et) >>= guard
              return (ts ++ [Step pt r ord True], PathTerm t' S.empty)

            userPaths = runListT $ do
              (t', r) <- liftSet userRWs
              ord <- ListT $ return $ Mb.maybeToList $ M.lookup t' acceptedUserRewrites
              lift (shouldExplore t' ord et) >>= guard
              return (ts ++ [Step pt r ord False], PathTerm t' S.empty)
