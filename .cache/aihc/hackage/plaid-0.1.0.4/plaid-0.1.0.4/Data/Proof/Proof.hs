module Data.Proof.Proof
  ( HasAuthGetBody
  , HasGetBalanceBody
  , HasCreatePublicTokenBody
  , HasTransactionsGetBody
  , HasIdentityGetBody
  , HasIncomeGetBody
  , name
  , unWrapNamed
  , proveAuthGetBody
  , proveTransactionsGetBody
  , proveIdentityGetBody
  , proveGetBalanceBody
  , proveCreatePublicTokenBody
  , proveIncomeGetBody
  ) where

import           Data.Proof.AuthGetBody         (HasAuthGetBody, proveAuthGetBody)
import           Data.Proof.BalanceGetBody      (HasGetBalanceBody, proveGetBalanceBody)
import           Data.Proof.IdentityGetBody     (HasIdentityGetBody, proveIdentityGetBody)
import           Data.Proof.IncomeGetBody       (HasIncomeGetBody, proveIncomeGetBody)
import           Data.Proof.Named               (name, unWrapNamed)
import           Data.Proof.PublicTokenCreate   (HasCreatePublicTokenBody,
                                                 proveCreatePublicTokenBody)
import           Data.Proof.TransactionsGetBody (HasTransactionsGetBody, proveTransactionsGetBody)
