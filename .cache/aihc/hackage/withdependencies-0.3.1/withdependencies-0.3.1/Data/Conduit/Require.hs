{-# LANGUAGE ScopedTypeVariables #-}
{- |
Runs computations depending on some values coming from a conduit. The computations are defined in applicative fashion.

> test :: IO [Int]
> test = inp =$ cnd $$ CL.consume
>     where
>         inp = sourceDirectory "/etc"
>         cnd :: ConduitT String Int IO ()
>         cnd = withRequirement (map Once comps) id (fmap length . readFile)
>         comps :: [Require String Int Int]
>         comps = [ (+) <$> require "/etc/passwd" <*> require "/etc/passwd"
>                 , (-) <$> require "/etc/resolv.conf" <*> require "/etc/nonexistent"
>                 , require "/etc/hosts"
>                 ]
-}

module Data.Conduit.Require (withRequirement, RunMode(..)) where

import Data.Conduit
import Control.Dependency
import Control.Monad
import Control.Monad.Trans

-- | This allows the user to parameter what happends once a requirement is
-- fulfilled.
data RunMode
   = Reset -- ^ The requirement will be reset, and can be run multiple times
   | Once -- ^ The requirement can only run once, first answer is kept on alts
   | OnceCorrect -- ^ The requirement can only run once, best answer is kept on alts
   deriving Show

-- | Given a stream of values, from which an identifier and a content can
-- be extracted, runs a list of computation that depend on these.
--
-- Each computation's output is `yield`ed downstream.
--
-- When all computations have been run, the conduit finishes processing.
withRequirement :: (Ord identifier, Eq identifier, Monad m, Functor m)
                => [(RunMode, Require identifier content x)] -- ^ The list of dependent computations
                -> (a -> identifier)              -- ^ Extracting the identifier
                -> (a -> m content)               -- ^ Extracting the content, possibly with effects
                -> ConduitT a x m ()
withRequirement computations getIdentifier getContent = run getIdentifier getContent compmap
    where
        compmap = [ (rm, req, []) | (rm, req) <- computations ]

run :: (Ord identifier, Eq identifier, Monad m, Functor m)
    => (a -> identifier)
    -> (a -> m content)
    -> [(RunMode, Require identifier content x, [(identifier, content)])]
    -> ConduitT a x m ()
run getIdentifier getContent computationList = do
    mi <- await
    case mi of
        Nothing ->
          let lastPass (runmode, req, contents) =
                case runmode of
                  OnceCorrect ->
                    case computeRequire contents req of
                      Just rs -> yield rs
                      Nothing -> pure ()
                  _ -> pure ()
          in  mapM_ lastPass computationList

        Just streamElement -> do
            let ident = getIdentifier streamElement
                tryComputeReq nc (runmode, req, contents) =
                    let previouscomputation = [(runmode, req, ncontents)]
                        droppedcontent = [(runmode, req, [])]
                        ncontents = (ident, nc) : contents
                    in  case runmode of
                          Once ->
                            case computeRequire ncontents req of
                              Just rs -> ([], [rs])
                              Nothing -> (previouscomputation, [])
                          Reset ->
                            case computeRequire ncontents req of
                              Just rs -> (droppedcontent, [rs])
                              Nothing -> (previouscomputation, [])
                          OnceCorrect ->
                            case computeRequireIntermediate ncontents req of
                              Just rs -> ([], [rs])
                              Nothing -> (previouscomputation, [])
                checkComputation (curCompList, curResults, curContent) requirement@(_,req,_) =
                    if triggersAnalyzer ident req
                        then do
                            nc <- case curContent of
                                      Just x -> return x
                                      _ -> getContent streamElement
                            let (resultingCompList, rcomp) = tryComputeReq nc requirement
                            return (resultingCompList ++ curCompList, rcomp ++ curResults, Just nc)
                        else return (requirement : curCompList, curResults, curContent)
            (newcomputationList, results, _) <- lift (foldM checkComputation ([], [], Nothing) computationList)
            mapM_ yield results
            unless (null newcomputationList) (run getIdentifier getContent newcomputationList)
