{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module BagExample (mkBagGraph) where

import Prelude hiding (EQ, GT)


import           Control.Monad.Identity
import Language.REST.Dot
import Language.REST.ExploredTerms
import Language.REST.RESTDot
import Language.REST.OCToAbstract
import Language.REST.RewriteRule
import qualified Language.REST.Internal.MultiSet as M
import Language.REST.Internal.MultisetOrder
import Language.REST.Rest
import Language.REST.WQOConstraints as OC
import Language.REST.WQOConstraints.Strict as SC
import Language.REST.Internal.WorkStrategy
import Language.REST.Types
import Language.REST.SMT hiding (GTE)

import qualified Data.List as L
import qualified Data.HashSet as S
import qualified Data.Text as T
import GHC.Generics (Generic)
import           Data.Hashable

newtype PChar = PChar Char deriving (Eq, Ord, Generic, Hashable)

instance ToSMTVar PChar Int where
  toSMTVar c = SMTVar $ T.pack $ "char_" ++ show c

instance Show PChar where
  show (PChar c) = return c

newtype Bag = Bag String
  deriving (Eq, Ord, Generic, Hashable)

instance Show Bag where
  show = showBag


toMultiset :: Bag -> M.MultiSet PChar
toMultiset (Bag str) = M.fromList $ map PChar str


bag :: String -> Bag
bag = Bag

data Rewrite = Rewrite Bag (S.HashSet Bag)
  deriving (Eq, Ord, Generic, Hashable)

infixr 1 ~>
(~>) :: String -> [String] -> [String]
(~>) = (:)

instance RewriteRule IO Rewrite Bag where
  apply bag1 (Rewrite bag' result) | bag1 == bag' = return result
  apply _ _                            = return S.empty


fromPath :: [String] -> S.HashSet Rewrite
fromPath [] = S.empty
fromPath xs = S.fromList $ zipWith (curry go) xs (tail xs)
  where
    go :: (String, String) -> Rewrite
    go (x, y) = Rewrite (bag x) (S.singleton $ bag y)


fromPaths :: [[String]] -> S.HashSet Rewrite
fromPaths paths = S.unions $ map fromPath paths

start :: String
start = "AAB"

rules :: S.HashSet Rewrite
rules = fromPaths
  [  start ~> "ACD" ~> "AAAA" ~> "ABDD" ~> []
  ,  start ~> "ABD" ~> "AB"  ~> "BBD" ~> []
  ]

showBag :: Bag -> String
showBag (Bag bag1) = "{ " ++ L.intercalate ", " (map return bag1) ++ " }"

showRule :: Rewrite -> String
showRule _ = ""


compareChar :: ConstraintGen impl PChar PChar Identity
compareChar impl GTE oc c1 c2 | c1 /= c2 = compareChar impl GT oc c1 c2
compareChar impl EQ  _  c1 c2 | c1 /= c2 = return $ OC.unsatisfiable impl
compareChar impl r   oc c1 c2            = return $ intersectRelation impl oc (c1, c2, r)


mkBagGraph :: IO ()
mkBagGraph =
  do
    (PathsResult paths, _) <- rest
      RESTParams
        { re           = S.empty
        , ru           = rules
        , target       = Nothing
        , workStrategy = bfs
        , ocImpl       = impl
        , initRes      = pathsResult
        , etStrategy   = ExploreWhenNeeded
        } (bag start)
    let prettyPrinter = PrettyPrinter showRule showBag show ShowRejectsWithRule
    writeDot "example" Tree prettyPrinter (toOrderedSet paths)
  where
    impl = lift SC.strictOC $ cmapConstraints toMultiset (multisetOrder compareChar)
