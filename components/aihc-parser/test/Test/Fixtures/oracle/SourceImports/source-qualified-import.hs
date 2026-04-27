{- ORACLE_TEST xfail SOURCE pragma with qualified import not yet supported -}
{-# LANGUAGE ImportQualifiedPost #-}

module SourceImportQualified where

import {-# SOURCE #-} qualified Test.ChasingBottoms.IsBottom as B
