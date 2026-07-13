-- | Strict graph-reduction intermediate language.
--
-- GRIN evaluation is strict: operands are values, and sequencing is explicit
-- through 'GrinBind'. Haskell laziness is represented by heap-allocated thunk
-- nodes and the explicit 'GrinEval' and 'GrinApply' operations.
module Aihc.Grin.Syntax
  ( GrinProgram (..),
    GrinFunction (..),
    FunctionName (..),
    GrinVar (..),
    GrinExpr (..),
    GrinValue (..),
    GrinNode (..),
    GrinNodeTag (..),
    GrinAlt (..),
    GrinAltCon (..),
    GrinLiteral (..),
    GrinForeignCall (..),
    GrinForeignSignature (..),
    GrinForeignResult (..),
    GrinForeignType (..),
  )
where

import Data.Text (Text)

-- | A whole GRIN program.
data GrinProgram = GrinProgram
  { grinConstructors :: ![(Text, Int)],
    grinPrimitives :: ![(GrinVar, Int)],
    grinForeignCalls :: ![GrinForeignCall],
    grinCafs :: ![(GrinVar, GrinNode)],
    grinFunctions :: ![GrinFunction]
  }
  deriving (Eq, Show)

-- | A first-order code definition. Closures and thunks refer to functions by
-- name and carry their environment as node fields.
data GrinFunction = GrinFunction
  { grinFunctionName :: !FunctionName,
    grinFunctionParameters :: ![GrinVar],
    grinFunctionBody :: !GrinExpr
  }
  deriving (Eq, Show)

newtype FunctionName = FunctionName
  { unFunctionName :: Text
  }
  deriving (Eq, Ord, Show)

-- | GRIN erases source types but preserves variable identity.
data GrinVar = GrinVar
  { grinVarName :: !Text,
    grinVarUnique :: !Int
  }
  deriving (Show)

instance Eq GrinVar where
  left == right =
    grinVarName left == grinVarName right
      && grinVarUnique left == grinVarUnique right

instance Ord GrinVar where
  compare left right =
    compare
      (grinVarName left, grinVarUnique left)
      (grinVarName right, grinVarUnique right)

-- | Strict expressions. Every constructor except 'GrinStoreRec' produces one
-- value; 'GrinBind' names that value for the following expression.
data GrinExpr
  = GrinReturn !GrinValue
  | GrinBind !GrinVar !GrinExpr !GrinExpr
  | GrinStore !GrinNode
  | GrinStoreRec ![(GrinVar, GrinNode)] !GrinExpr
  | GrinFetch !GrinValue
  | GrinUpdate !GrinValue !GrinValue
  | GrinEval !GrinValue
  | GrinApply !GrinValue !GrinValue
  | GrinCase !GrinValue !GrinVar ![GrinAlt]
  | GrinDictSelect !GrinValue !Int
  | GrinThrow !GrinValue
  | GrinCatch !GrinValue !GrinValue !GrinValue
  deriving (Eq, Show)

-- | Atomic operands in the strict language.
data GrinValue
  = GrinVarValue !GrinVar
  | GrinLitValue !GrinLiteral
  | GrinNodeValue !GrinNode
  deriving (Eq, Show)

data GrinNode = GrinNode
  { grinNodeTag :: !GrinNodeTag,
    grinNodeFields :: ![GrinValue]
  }
  deriving (Eq, Show)

data GrinNodeTag
  = GrinConstructor !Text
  | GrinClosure !FunctionName
  | GrinThunk !FunctionName
  | GrinPrimitive !Text !Int
  | GrinForeign !GrinForeignCall
  | GrinForeignIOAction !GrinForeignCall
  | GrinDictionary
  deriving (Eq, Show)

data GrinAlt = GrinAlt
  { grinAltCon :: !GrinAltCon,
    grinAltBinders :: ![GrinVar],
    grinAltRhs :: !GrinExpr
  }
  deriving (Eq, Show)

data GrinAltCon
  = GrinDataAlt !Text
  | GrinLitAlt !GrinLiteral
  | GrinDefaultAlt
  deriving (Eq, Show)

data GrinLiteral
  = GrinLitInt !Integer
  | GrinLitChar !Char
  | GrinLitString !Text
  deriving (Eq, Show)

data GrinForeignCall = GrinForeignCall
  { grinForeignCallName :: !Text,
    grinForeignCallSymbol :: !Text,
    grinForeignCallSignature :: !GrinForeignSignature
  }
  deriving (Eq, Show)

data GrinForeignSignature = GrinForeignSignature
  { grinForeignArgumentTypes :: ![GrinForeignType],
    grinForeignResult :: !GrinForeignResult
  }
  deriving (Eq, Show)

data GrinForeignResult
  = GrinForeignPure !GrinForeignType
  | GrinForeignIO !GrinForeignType
  deriving (Eq, Show)

data GrinForeignType
  = GrinForeignCInt
  deriving (Eq, Show)
