-- | Type equality axioms exported across incremental compilation boundaries.
module Aihc.Fc.Axiom
  ( AxiomInterface,
    extractAxiomInterface,
    lookupAxiomDecl,
  )
where

import Aihc.Fc.Syntax
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Equality declarations indexed by the names used in 'AxiomInstCo'.
-- Local declarations take precedence when interfaces are combined.
newtype AxiomInterface = AxiomInterface
  { axiomsByName :: Map Text FcAxiomDecl
  }
  deriving (Eq, Show, Read)

instance Semigroup AxiomInterface where
  AxiomInterface left <> AxiomInterface right = AxiomInterface (right <> left)

instance Monoid AxiomInterface where
  mempty = AxiomInterface Map.empty

-- | Collect explicit axioms and the implicit representational axioms carried
-- by newtype declarations.
extractAxiomInterface :: FcProgram -> AxiomInterface
extractAxiomInterface (FcProgram _ _ topBinds) =
  AxiomInterface
    ( Map.fromList
        [ (fcAxiomName declaration, declaration)
        | topBind <- topBinds,
          declaration <- axiomDeclarations topBind
        ]
    )
  where
    axiomDeclarations topBind =
      case topBind of
        FcAxiom declaration -> [declaration]
        FcNewtype declaration ->
          [ FcAxiomDecl
              { fcAxiomName = fcNewtypeName declaration,
                fcAxiomTyVars = fcNewtypeTyVars declaration,
                fcAxiomRole = FcRepresentational,
                fcAxiomLeft = fcNewtypeResult declaration,
                fcAxiomRight = fcNewtypeRepresentation declaration
              }
          ]
        _ -> []

lookupAxiomDecl :: Text -> AxiomInterface -> Maybe FcAxiomDecl
lookupAxiomDecl name = Map.lookup name . axiomsByName
