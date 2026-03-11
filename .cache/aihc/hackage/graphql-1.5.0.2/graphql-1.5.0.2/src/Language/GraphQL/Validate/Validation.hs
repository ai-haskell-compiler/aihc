{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

-- | Definitions used by the validation rules and the validator itself.
module Language.GraphQL.Validate.Validation
    ( Error(..)
    , Rule(..)
    , RuleT
    , Validation(..)
    ) where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Sequence (Seq)
import Language.GraphQL.AST.DirectiveLocation (DirectiveLocation(..))
import Language.GraphQL.AST.Document
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema (Schema)

-- | Validation error.
data Error = Error
    { message :: String
    , locations :: [Location]
    } deriving (Eq, Show)

-- | Validation rule context.
data Validation m = Validation
    { ast :: Document
    , schema :: Schema m
    }

-- | 'Rule' assigns a function to each AST node that can be validated. If the
-- validation fails, the function should return an error message, or 'Nothing'
-- otherwise.
data Rule m
    = DefinitionRule (Definition -> RuleT m)
    | OperationDefinitionRule (OperationDefinition -> RuleT m)
    | FragmentDefinitionRule (FragmentDefinition -> RuleT m)
    | SelectionRule (Maybe (Out.Type m) -> Selection -> RuleT m)
    | FragmentRule (FragmentDefinition -> RuleT m) (InlineFragment -> RuleT m)
    | FragmentSpreadRule (FragmentSpread -> RuleT m)
    | FieldRule (Maybe (Out.Type m) -> Field -> RuleT m)
    | ArgumentsRule (Maybe (Out.Type m) -> Field -> RuleT m) (Directive -> RuleT m)
    | DirectivesRule (DirectiveLocation -> [Directive] -> RuleT m)
    | VariablesRule ([VariableDefinition] -> RuleT m)
    | ValueRule (Maybe In.Type -> Node Value -> RuleT m) (Maybe In.Type -> Node ConstValue -> RuleT m)

-- | Monad transformer used by the rules.
type RuleT m = ReaderT (Validation m) Seq Error
