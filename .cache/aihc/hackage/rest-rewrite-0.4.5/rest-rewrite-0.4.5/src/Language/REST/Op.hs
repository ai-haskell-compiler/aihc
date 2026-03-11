{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.REST.Op where

import Data.Text as T
import Data.Hashable
import Data.String
import GHC.Generics (Generic)
import Language.REST.SMT

-- | The operators used in 'RuntimeTerm' and 'MetaTerm'
newtype Op = Op Text deriving (Eq, Ord, Hashable, Generic)

instance Show Op where
  show (Op o) = unpack o

instance IsString Op where
  fromString = Op . pack

instance ToSMTVar Op Int where
  toSMTVar (Op "ite") = SMTVar "_ite_"
  toSMTVar (Op op) = SMTVar $ T.append "op_" $ T.concatMap go op
    where
        go '∅'  = "_empty_"
        go '₀'  = "0"
        go '₁'  = "1"
        go '$'  = "_dollar_"
        go '['  = "_lb_"
        go ']'  = "_rb_"
        go ':'  = "_colon_"
        go '.'  = "_dot_"
        go '#'  = "_pound_"
        go '\'' = "_comma_"
        go '/'  = "_slash_"
        go ' '  = "_space_"
        go '∪'  = "_cup_"
        go '\\' = "_bslash_"
        go '(' = "_lp_"
        go ')' = "_rp_"
        go c    = singleton c
