
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Identity
import Data.Time.Clock
import Data.Hashable
import qualified Data.Set as DS
import qualified Data.HashSet as S
import Text.Printf

import qualified Arith as A
import qualified Compiler as C
import qualified Group as G
import Language.REST.RESTDot
import Language.REST.Dot
import Language.REST.Internal.WorkStrategy
import DSL
import Nat
import Set
import qualified Multiset as MS
import NonTerm as NT
import qualified Lists as Li

import Language.REST.Core
import Language.REST.ConcreteOC
import Language.REST.ExploredTerms
import Language.REST.OCAlgebra
import Language.REST.OCToAbstract
import Language.REST.Op
import Language.REST.WQOConstraints as OC
import qualified Language.REST.WQOConstraints.Strict as SC
import qualified Language.REST.WQOConstraints.ADT    as AC
import Language.REST.KBO (kbo)
import Language.REST.LPO (lpo, lpoStrict)
import Language.REST.RPO
import Language.REST.ProofGen
import Language.REST.Rest
import Language.REST.Types
import Language.REST.SMT
import           Language.REST.RuntimeTerm as RT
import           Language.REST.Internal.Rewrite


data ConsType = Strict | Lazy | ADT

mkArithRESTGraph,
  mkCompilerRESTGraph,
  mkGroupRESTGraph,
  mkListsRESTGraph,
  mkSetsRESTGraph,
  mkNonTermRestGraph,
  mkMSRESTGraph
  :: SolverType -> String -> String -> GraphParams -> IO ()
mkArithRESTGraph ct = mkRESTGraph ct A.evalRWs A.userRWs
mkCompilerRESTGraph ct = mkRESTGraph ct C.evalRWs C.userRWs
mkGroupRESTGraph ct = mkRESTGraph ct G.evalRWs G.userRWs
mkListsRESTGraph ct = mkRESTGraph ct Li.evalRWs Li.userRWs
mkSetsRESTGraph ct = mkRESTGraph ct Set.evalRWs Set.userRWs
mkNonTermRestGraph ct = mkRESTGraph ct NT.evalRWs NT.userRWs
mkMSRESTGraph ct = mkRESTGraph ct MS.evalRWs MS.userRWs

explain :: (Show t2, Show t3, Show a) => (t2 -> t3 -> a) -> (t2, t3) -> IO ()
explain f0 (t, u) = printf "%s ≥ %s requires:\n%s\n\n\n" (show t) (show u) (show $ f0 t u)

explainOrient :: [String] -> IO ()
explainOrient ts0 = withZ3 go where
  go :: SolverHandle -> IO ()
  go _z3 =
    let
      ts             = map parseTerm ts0
      pairs          = zip ts (tail ts)
    in
      do
        mapM_ (explain (refine impl (top impl))) pairs
        printf "Result:\n%s\n" (show $ orient impl ts)
        isSatisfiable SC.strictOC (orient impl ts) >>= print
    where
      impl :: OCAlgebra (SC.StrictOC Op) RuntimeTerm Identity
      impl = lift SC.strictOC lpo

data GraphParams = GraphParams
  {  gShowConstraints :: Bool
  ,  gTarget          :: Maybe String
  ,  gGraphType       :: GraphType
  ,  gShowRejects     :: ShowRejectsOpt
  ,  gUseETOpt        :: Bool
  }

defaultParams :: GraphParams
defaultParams = GraphParams False Nothing Tree ShowRejectsWithoutRule True

withTarget :: String -> GraphParams -> GraphParams
withTarget target0 gp = gp{gTarget = Just target0}

withShowConstraints :: GraphParams -> GraphParams
withShowConstraints gp = gp{gShowConstraints = True}

withNoETOpt :: GraphParams -> GraphParams
withNoETOpt gp = gp{gUseETOpt = False}

withHideRejects :: GraphParams -> GraphParams
withHideRejects gp = gp{gShowRejects = HideRejects}

withShowRejectsRule :: GraphParams -> GraphParams
withShowRejectsRule gp = gp{gShowRejects = ShowRejectsWithRule}

data SolverType = LPOStrict | LPO | RPO | RPOConcrete [Op] | KBO | Fuel Int

mkRESTGraph ::
     SolverType
  -> S.HashSet Rewrite
  -> S.HashSet Rewrite
  -> String
  -> String
  -> GraphParams
  -> IO ()
mkRESTGraph LPOStrict evalRWs0 userRWs0 name term0 params =
  withZ3 $ \z3 -> mkRESTGraph' (lift (AC.adtOC z3) lpoStrict) evalRWs0 userRWs0 name term0 params
mkRESTGraph LPO evalRWs0 userRWs0 name term0 params =
  withZ3 $ \z3 -> mkRESTGraph' (lift (AC.adtOC z3) lpo) evalRWs0 userRWs0 name term0 params
mkRESTGraph RPO evalRWs0 userRWs0 name term0 params =
  withZ3 $ \z3 -> mkRESTGraph' (lift (AC.adtOC z3) rpo) evalRWs0 userRWs0 name term0 params
mkRESTGraph (RPOConcrete ops) evalRWs0 userRWs0 name term0 params =
  mkRESTGraph' (concreteOC $ DS.fromList ops) evalRWs0 userRWs0 name term0 params
mkRESTGraph KBO evalRWs0 userRWs0 name term0 params =
  withZ3 $ \z3 -> mkRESTGraph' (kbo z3) evalRWs0 userRWs0 name term0 params
mkRESTGraph (Fuel n) evalRWs0 userRWs0 name term0 params =
  mkRESTGraph' (fuelOC n) evalRWs0 userRWs0 name term0 params

mkRESTGraph' :: (MonadIO m, Show c, Hashable c, Ord c) =>
     OCAlgebra c RuntimeTerm m
  -> S.HashSet Rewrite
  -> S.HashSet Rewrite
  -> String
  -> String
  -> GraphParams
  -> m ()
mkRESTGraph' impl evalRWs0 userRWs0 name term0 params =
  do
    let pr (Rewrite t u _) = printf "%s → %s" (pp t) (pp u)
    liftIO $ mapM_ (putStrLn . pr) $ S.toList userRWs0
    liftIO $ mapM_ (putStrLn . pr) $ S.toList evalRWs0
    start <- liftIO getCurrentTime
    (PathsResult paths, targetPath) <- rest
      RESTParams
        { re           = evalRWs0
        , ru           = userRWs0
        , target       = fmap parseTerm (gTarget params)
        , workStrategy = bfs
        , ocImpl       = impl
        , initRes      = pathsResult
        , etStrategy   = if gUseETOpt params then ExploreWhenNeeded else ExploreAlways
        } (parseTerm term0)
    end <- liftIO getCurrentTime
    liftIO $ printf "REST run completed, in %s\n" $ show $ diffUTCTime end start
    liftIO $ putStrLn "Drawing graph"
    let showCons = if gShowConstraints params then show else const ""
    let prettyPrinter = PrettyPrinter pr pp showCons (gShowRejects params)
    liftIO $ writeDot name (gGraphType params) prettyPrinter (toOrderedSet paths)
    liftIO $ case gTarget params of
      Just target1 ->
        (case targetPath of
          Just tp -> printf "FOUND TARGET. Proof:\n%s\n" (toProof tp)
          Nothing -> printf "TARGET %s NOT FOUND\n" (pp (parseTerm target1)))
      Nothing -> return ()

setDistribRules :: S.HashSet Rewrite
setDistribRules = S.fromList
  [ distribL (/\) (\/)
  , distribR (/\) (\/)
  , distribL (\/) (/\)
  , distribR (\/) (/\)
  ]

challengeRulesNoCommute :: S.HashSet Rewrite
challengeRulesNoCommute = S.union setDistribRules $ S.fromList
  [ x /\ x        ~> x
  , x \/ x        ~> x
  , x \/ emptyset ~> x
  , x /\ emptyset ~> emptyset
  , assocL (\/)
  , assocR (\/)
  ]

main :: IO ()
main = do
  mkRESTGraph RPO S.empty (S.insert (s1 /\ s0 ~> emptyset) challengeRulesNoCommute) "fig4" "f(intersect(union(s₀,s₁), s₀))" (withNoETOpt defaultParams)
  mkRESTGraph RPO S.empty (S.fromList $ (x #+ y ~> y #+ x) : ((x #+ y) #+ v <~> x #+ (y #+ v))) "fig8-noopt" "a + (b + a)" (withNoETOpt defaultParams)
  mkRESTGraph RPO S.empty (S.fromList $ (x #+ y ~> y #+ x) : ((x #+ y) #+ v <~> x #+ (y #+ v))) "fig8-opt" "a + (b + a)" defaultParams
