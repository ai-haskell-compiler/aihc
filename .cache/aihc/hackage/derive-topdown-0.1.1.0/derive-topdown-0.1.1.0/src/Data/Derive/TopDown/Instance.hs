-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.Instance
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.Instance
  ( instance_
  , instance_with_breaks
  , instances
  , instancess
  , instance_with
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.Derive.TopDown.CxtGen     ( genInferredContext )
import           Data.Derive.TopDown.IsInstance
import           Data.Derive.TopDown.Lib
import           Data.Derive.TopDown.Types
import           Data.List                      ( foldl1' )
import           Language.Haskell.TH

gen_instance_decl
  :: ClassName
  -> TypeName
  -> [TypeName]  -- ^ a list of types that breaks the generation process
  -> Maybe Overlap
  -> ContextGenerator -- ^ a context generator
  -> StateT [Type] Q [Dec]
gen_instance_decl cn tn breaks mo cg = do
  (tvbs, cons) <- lift $ getTyVarCons tn
  let typeNames = map getTVBName tvbs
  isCnHighOrderClass <- lift $ isHigherOrderClass cn
  -- prevent calling isInstance class with * -> * and type with *
  if isCnHighOrderClass && null typeNames
    then return []
    else do
      saturatedType <- lift $ foldl1' appT (conT tn : map varT typeNames)
      instanceType  <- if isCnHighOrderClass && (not . null) typeNames
        then
          let pns = init typeNames
          in  if null pns
                then lift $ conT tn
                else lift $ foldl1' appT (conT tn : (map varT pns))
        else return saturatedType
      isMember <- lift $ isInstance' cn [instanceType]
      table    <- get
      if isMember || elem instanceType table || elem tn breaks
         -- normally empty instance will not be used to derive Generic
         -- so I do not check Generic and Generic1
        then return []
        else do
          classContext <- if isCnHighOrderClass
            then return []
            else lift $ cg cn tn
          let decl =
                [InstanceD mo classContext (AppT (ConT cn) instanceType) []]
          modify (instanceType :)
          names  <- lift $ fmap concat $ mapM getCompositeTypeNames cons
          names' <- lift
            $ filterM (\x -> isTypeFamily x >>= \b -> return $ not b) names
          xs <- mapM (\n -> gen_instance_decl cn n breaks mo cg) names'
          return $ concat xs ++ decl

instance_
  :: Name -- ^ class name
  -> Name -- ^ type name
  -> Q [Dec]
instance_ cn tn =
  evalStateT (gen_instance_decl cn tn [] Nothing genInferredContext) []

instance_with_breaks
  :: Name -- ^ class name
  -> Name -- ^ type name
  -> [Name] -- ^ type names that stop the deriving process
  -> Q [Dec]
instance_with_breaks cn tn bs =
  evalStateT (gen_instance_decl cn tn bs Nothing genInferredContext) []

instances
  :: [Name] -- ^ class names
  -> Name   -- ^ type name
  -> Q [Dec]
instances cns tn = fmap concat (mapM (\x -> instance_ x tn) cns)

instancess
  :: [Name] -- ^ class names
  -> [Name] -- ^ type names
  -> Q [Dec]
instancess cns tns = fmap concat (mapM (\x -> instances cns x) tns)

instance_with
  :: ClassName
  -> TypeName
  -> [TypeName]        -- ^ a list of types that breaks the generation process
  -> Maybe Overlap
  -> ContextGenerator -- ^ a context generator
  -> Q [Dec]
instance_with cn tn bs mo cg = evalStateT (gen_instance_decl cn tn bs mo cg) []
