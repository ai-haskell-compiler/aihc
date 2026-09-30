-- | The shape of the control flow of one Lir function, for the structured
-- control flow of WebAssembly.
--
-- The backend follows "Beyond Relooper" (Norman Ramsey, ICFP 2022). A
-- reducible control-flow graph becomes nested @block@, @loop@, and @if@
-- constructs that follow its dominator tree:
--
-- * A block that is the target of a back edge is a loop header. Its code
--   is in a @loop@, and a back edge continues the loop.
-- * A block with two or more forward edges into it is a merge node. Its
--   code follows a @block@ in the code of its immediate dominator, and a
--   forward edge to it leaves that @block@.
-- * The code of every other block goes at the place of its one forward
--   edge.
--
-- An irreducible graph has a retreating edge whose target does not
-- dominate its source. This module gives no shape for such a graph.
module Aihc.Wasm.ControlFlow
  ( ControlFlow (..),
    analyzeControlFlow,
  )
where

import Aihc.Lir.Syntax (Block (..), Label, Target (..), terminatorTargets)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set

data ControlFlow = ControlFlow
  { -- | The reverse postorder number of each reachable block. An edge is
    -- backward when its target has a number that is not larger than the
    -- number of its source.
    flowOrder :: !(Map Label Int),
    flowLoopHeaders :: !(Set Label),
    flowMergeNodes :: !(Set Label),
    -- | The children of each block in the dominator tree that are merge
    -- nodes, with the largest reverse postorder number first.
    flowMergeChildren :: !(Map Label [Label])
  }

-- | The shape of a reducible graph. The first block is the entry.
analyzeControlFlow :: [Block] -> Maybe ControlFlow
analyzeControlFlow blocks =
  case blocks of
    [] -> Nothing
    entryBlock : _
      | all isBackEdge retreating ->
          Just
            ControlFlow
              { flowOrder = number,
                flowLoopHeaders = Set.fromList (map snd retreating),
                flowMergeNodes = merges,
                flowMergeChildren =
                  Map.map
                    (sortOn (Down . (number Map.!)) . filter (`Set.member` merges))
                    (Map.fromListWith (<>) [(idom, [label]) | (label, idom) <- Map.toList idoms, label /= entry])
              }
      | otherwise -> Nothing
      where
        entry = blockLabel entryBlock
        successors label = Map.findWithDefault [] label successorMap
        successorMap = Map.fromList [(blockLabel block, map targetLabel (terminatorTargets (blockTerminator block))) | block <- blocks]
        order = reversePostorder successors entry
        number = Map.fromList (zip order [0 ..])
        -- Every edge between reachable blocks, once for each target that a
        -- terminator names. Two edges from one block to the same target
        -- make that target a merge node.
        edges = [(source, target) | source <- order, target <- successors source]
        predecessors = Map.fromListWith (flip (<>)) [(target, [source]) | (source, target) <- edges]
        idoms = immediateDominators number order (\label -> Map.findWithDefault [] label predecessors)
        retreating = [edge | edge@(source, target) <- edges, number Map.! target <= number Map.! source]
        isBackEdge (source, target) = dominates target source
        dominates dominator label
          | label == dominator = True
          | label == entry = False
          | otherwise = dominates dominator (idoms Map.! label)
        merges =
          Map.keysSet
            ( Map.filter
                (>= (2 :: Int))
                (Map.fromListWith (+) [(target, 1) | (source, target) <- edges, number Map.! target > number Map.! source])
            )

-- | The blocks that the entry reaches, in reverse postorder.
reversePostorder :: (Label -> [Label]) -> Label -> [Label]
reversePostorder successors entry = snd (visit (Set.empty, []) entry)
  where
    visit (seen, finished) label
      | Set.member label seen = (seen, finished)
      | otherwise =
          let (seen', finished') = foldl' visit (Set.insert label seen, finished) (successors label)
           in (seen', label : finished')

-- | The immediate dominator of each reachable block, by the iterative
-- algorithm of Cooper, Harvey, and Kennedy. The entry is its own immediate
-- dominator.
immediateDominators :: Map Label Int -> [Label] -> (Label -> [Label]) -> Map Label Label
immediateDominators number order predecessors =
  case order of
    [] -> Map.empty
    entry : rest -> fixpoint rest (Map.singleton entry entry)
  where
    fixpoint rest idoms =
      let idoms' = foldl' update idoms rest
       in if idoms' == idoms then idoms else fixpoint rest idoms'
    update idoms label =
      case filter (`Map.member` idoms) (predecessors label) of
        [] -> idoms
        first : others -> Map.insert label (foldl' (intersect idoms) first others) idoms
    intersect idoms left right
      | left == right = left
      | number Map.! left > number Map.! right = intersect idoms (idoms Map.! left) right
      | otherwise = intersect idoms left (idoms Map.! right)
