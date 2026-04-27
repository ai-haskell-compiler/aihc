{- ORACLE_TEST pass -}
{-# LANGUAGE ImportQualifiedPost #-}

module SourceImportQualified where

import {-# SOURCE #-} qualified Test.ChasingBottoms.IsBottom as B
