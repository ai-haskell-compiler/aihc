-- | The optimization level of a build.
--
-- The level selects the optional passes of the native backends. Level 2 is
-- the default and runs every pass. Level 0 skips the register hints of the
-- allocator, the fusion of a compare into its branch, and the elision of
-- slot reloads, so the machine code follows the Lir text one instruction at
-- a time. Both levels give the same program behavior.
module Aihc.Lir.Optimization
  ( OptimizationLevel (..),
    defaultOptimizationLevel,
    parseOptimizationLevel,
    renderOptimizationLevel,
  )
where

data OptimizationLevel
  = -- | No optional pass runs.
    O0
  | -- | Every optional pass runs.
    O2
  deriving (Eq, Ord, Show, Enum, Bounded)

defaultOptimizationLevel :: OptimizationLevel
defaultOptimizationLevel = O2

-- | Parse the digit of a @-O@ option.
parseOptimizationLevel :: String -> Either String OptimizationLevel
parseOptimizationLevel value =
  case value of
    "0" -> Right O0
    "2" -> Right O2
    _ -> Left "expected 0 or 2"

-- | The digit of a level, as the @-O@ option takes it.
renderOptimizationLevel :: OptimizationLevel -> String
renderOptimizationLevel level =
  case level of
    O0 -> "0"
    O2 -> "2"
