-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.CxtGen
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.Types (
    ClassName
  , TypeName
  , ContextGenerator)
where
    
import Language.Haskell.TH

type ClassName = Name
type TypeName = Name
type ContextGenerator = ClassName -> TypeName -> Q Cxt