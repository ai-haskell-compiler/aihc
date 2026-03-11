module Data.Proof.PublicTokenCreate
  ( HasCreatePublicTokenBody
  , proveCreatePublicTokenBody
  ) where

import           Data.Api.Types
import           Data.Proof.Named
import           Data.Text        (Text)

data HasCreatePublicTokenBody plaidEnv institutionId productList plaidOptions = Proof

-- | Check if all elements for creating a public token are present
proveCreatePublicTokenBody
  :: Named plaidEnv PlaidEnv
  -> Named institutionId InstitutionId
  -> Named productList [PlaidProduct]
  -> Named options (Maybe PlaidOptions)
  -> Either Text (HasCreatePublicTokenBody plaidEnv institutionId productList options)
proveCreatePublicTokenBody _ institutionId productList _
  | null (unWrapNamed productList) =
      Left "Initial products list cannot be empty"
  | unWrapNamed institutionId `elem` validInstitutionIds
      = Right Proof
  | otherwise = Left (unInstitutionId (unWrapNamed institutionId)
                  <> " is not a valid institution id. Check the plaid documentation.")
