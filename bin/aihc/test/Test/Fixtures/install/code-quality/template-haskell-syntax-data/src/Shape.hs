{-# LANGUAGE DeriveDataTypeable #-}

-- The shape of DatatypeInfo and ConstructorInfo in th-abstraction.
module Shape (Shape (..)) where

import Data.Data (Data)
import Language.Haskell.TH.Syntax

data Shape = Shape
  { shapeContext :: [Type],
    shapeName :: Name,
    shapeVars :: [TyVarBndr ()],
    shapeFields :: [Name],
    shapeBody :: Exp,
    shapeDecs :: [Dec],
    shapeInfo :: Info
  }
  deriving (Data)
