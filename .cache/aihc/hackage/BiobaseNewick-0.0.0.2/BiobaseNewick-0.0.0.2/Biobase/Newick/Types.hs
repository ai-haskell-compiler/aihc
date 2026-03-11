
module Biobase.Newick.Types where

import Control.Applicative
import Control.Monad
import Data.Aeson (FromJSON,ToJSON)
import Data.Binary (Binary)
import Data.Monoid
import Data.Serialize (Serialize)
import Data.Serialize.Text
import Data.Text.Binary
import Data.Text (Text)
import Data.Tree
import GHC.Generics
import Test.QuickCheck



-- | Node and leaf information in Newick trees.

data Info = Info
  { label     :: Text
  , distance  :: Double
  } deriving (Eq,Show,Generic)

instance Binary    Info
instance Serialize Info
instance FromJSON  Info
instance ToJSON    Info

instance Arbitrary Info where
  arbitrary = Info <$> pure "" <*> (maybe 0 getPositive <$> arbitrary)
  shrink (Info lbl d)
    | d == 0    = []
    | otherwise = [Info lbl 0]



-- | Newick tree newtype wrapper.

newtype NewickTree = NewickTree { getNewickTree :: Tree Info }
  deriving (Eq,Show,Generic)

instance Binary    NewickTree
instance Serialize NewickTree
instance FromJSON  NewickTree
instance ToJSON    NewickTree

instance Arbitrary NewickTree where
  arbitrary = NewickTree <$> (arbNewickTree =<< (getSmall <$> arbitrary))
  shrink (NewickTree (Node lbl [])) = [NewickTree (Node l []) | l <- shrink lbl]
  shrink (NewickTree (Node lbl cs)) = [NewickTree (Node l ds) | l <- shrink lbl
                                                              , ds <- map (map getNewickTree) . shrink $ map NewickTree cs]

arbNewickTree :: Int -> Gen (Tree Info)
arbNewickTree k | k<=0 = Node <$> arbitrary <*> pure []
arbNewickTree k = do
  n  <- choose (0,5)
  ds <- replicateM n $ choose (0,k-1)
  cs <- mapM arbNewickTree ds
  Node <$> arbitrary <*> pure cs

