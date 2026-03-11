-- RandomTree module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to the creation of a random tree

module Math.RandomTree.Algorithms where

-- Built-in
import Data.Maybe
import Data.Tree
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Applicative

--Cabal
import Control.Monad.Random
import Control.Lens ((??))
import Math.TreeFun.Tree
import Math.TreeFun.Types

-- Local
import Math.RandomTree.Types
import Math.RandomTree.Label

-- | Run the monad transformer for the generation of a random tree. The
-- minChildren value, if chosen, results in a leaf
runTree :: ReaderStateRandom a
        -> Int
        -> Int
        -> Int
        -> Int
        -> StdGen
        -> Maybe a
runTree start minS maxS minC maxC g
    | minS > maxS = error "Minimum size is larger than maximum size."
    | otherwise   = (evalRand ?? g)
                  . runMaybeT
                  . (evalStateT ?? treeState)
                  . (runReaderT ?? treeConfig)
                  . runReaderStateRandom
                  $ start
  where
    treeState = TreeState { size = 0 }
    treeConfig = TreeConfig { maxSize     = maxS
                            , minSize     = minS
                            , minChildren = minC
                            , maxChildren = maxC }

-- | The recursion for each step of the tree
treeRecursion :: ReaderStateRandom (Tree Int)
treeRecursion = do
    treeConfig <- ask
    treeState <- get
    -- Cut it short if too big
    when (size treeState > maxSize treeConfig) mzero
    -- Cut it short if just right
    when ( size treeState >= minSize treeConfig
        && size treeState <= maxSize treeConfig )
        $ return ()
    -- Otherwise continue
    put (treeState { size = size treeState + 1 })
    r <- getRandomR (minChildren treeConfig, maxChildren treeConfig)
    if r == minChildren treeConfig
        then return Node { rootLabel = size treeState, subForest = [] }
        else do
            newSubForest <- replicateM r treeRecursion
            return Node { rootLabel = size treeState
                        , subForest = newSubForest }

-- | The check for the lower bound: if not fulfilled, returns Nothing
checkLowerBound :: ReaderStateRandom (Tree Int)
checkLowerBound = do
    put TreeState { size = 0 }
    tree <- treeRecursion
    treeState <- get
    treeConfig <- ask
    guard $ size treeState >= minSize treeConfig
    return tree

-- | Recursion which continues to make trees until the bounds are met
getTree :: ReaderStateRandom (Tree Int)
getTree = checkLowerBound `mplus` getTree

-- | Return String trees
makeTree :: [String]
         -> Int
         -> Int
         -> Int
         -> Int
         -> Int
         -> Bool
         -> IO (PropertySuperTree String String)
makeTree propertyList neighborDistance minS maxS minC maxC clumpBool = do
    gen1         <- newStdGen
    gen2         <- newStdGen

    let (Just intTree) = runTree getTree minS maxS minC maxC gen1
        tree           = show <$> intTree
        propertyMap    = emptyPropertyMap . leaves $ tree
        filledPropertyList  = take (length . leaves $ tree)
                            . concat
                            . repeat
                            $ propertyList
        currentSuperTree = toSuperNodeTree SuperRoot $ tree
        newPropertyMap   = assignRandomClumpedProperties
                           filledPropertyList
                           neighborDistance
                           currentSuperTree
                           gen2
                           propertyMap
        newUniformPropertyMap = assignRandomProperties
                                filledPropertyList
                                gen2
                                propertyMap

    if clumpBool
        then return
           $ PropertySuperTree { superTree = currentSuperTree
                               , superProperties = M.map fromJust newPropertyMap }
        else return
           $ PropertySuperTree { superTree = currentSuperTree
                               , superProperties = M.map fromJust newUniformPropertyMap }
