{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.Standalone
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.Standalone (
     deriving_
   , deriving_with_breaks
   , derivings
   , derivingss
   , deriving_with
#if __GLASGOW_HASKELL__ >= 802
   , strategy_deriving
   , strategy_derivings
   , strategy_derivingss
#endif
) 

where

import Data.Derive.TopDown.Lib
import Data.Derive.TopDown.CxtGen (genInferredContext)
import Language.Haskell.TH
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.Derive.TopDown.IsInstance
import Data.Derive.TopDown.Types
import Data.List (foldl1')
import Data.Primitive.Types
import GHC.Generics


reset_strategy :: TypeName -> Maybe DerivStrategy -> Q (Maybe DerivStrategy)
reset_strategy tn st = do
        declareType <- decType tn
        case (declareType, st) of
          (_, Nothing) -> return Nothing
          (Data, Just NewtypeStrategy) -> return Nothing
          _ -> return st

gen_standalone_deriving_decl :: ClassName
                          -> TypeName
                          -> Maybe DerivStrategy  
                          -> [TypeName]  -- ^ a list of types that breaks the generation process
                          -> ContextGenerator -- ^ a context generator
                          -> StateT [Type] Q [Dec]
gen_standalone_deriving_decl cn tn st breaks cg = do
                       (tvbs, cons) <- lift $ getTyVarCons tn
                       let typeNames = map getTVBName tvbs
                       isCnHighOrderClass <- lift $ isHigherOrderClass cn
                       -- prevent calling isInstance class with * -> * and type with *
                       if isCnHighOrderClass && null typeNames
                        then return []
                        else do
                           saturatedType <- lift $ foldl1' appT (conT tn : map varT typeNames)
                           instanceType <- if isCnHighOrderClass && (not . null) typeNames
                                                then let pns = init typeNames
                                                      in  if null pns
                                                            then lift $ conT tn
                                                            else lift $ foldl1' appT (conT tn : (map varT pns))
                                                else return saturatedType
                           -- Stop generating further instances
                           -- 1. it is already a member of that type class
                           -- 2. we have already generated it, which is kind of same with case 1
                           -- 3. for GHC.Generic, if it is a primitive type like Int, Double
                           -- 4. It will stop on the types in breaks
                           -- 5. It will stop on primitive types and Integer when deriving Typeable
                           isMember <- lift $ isInstance' cn [instanceType]
                           isPrimitive <-lift $ isInstance' ''Prim [saturatedType]
                           let isGeneric = ''Generic == cn
                           let isGeneric1 = ''Generic1 == cn
                           table <- get
                           if isMember || elem instanceType table || elem tn breaks ||
                              (isPrimitive && (isGeneric || isGeneric1)) || 
                              (tn == ''Integer && (isGeneric || isGeneric1))
                              then return []
                              else do
                                 classContext <- if isCnHighOrderClass 
                                                   then return []
                                                   else lift $ cg  cn tn
                                 s <- lift $ reset_strategy tn st
                                 let decl = [StandaloneDerivD s classContext (AppT (ConT cn) instanceType)]
                                 modify (instanceType:)
                                 names <- lift $ fmap concat $ mapM getCompositeTypeNames cons
                                 names' <- lift $ filterM (\x -> isTypeFamily x >>= \b -> return $ not b) names
                                 xs <- mapM (\n -> gen_standalone_deriving_decl cn n st breaks cg) names'
                                 return $ concat xs ++ decl

deriving_ :: Name -- ^ class name
          -> Name -- ^ type name
          -> Q [Dec]
deriving_ cn tn = evalStateT (gen_standalone_deriving_decl cn tn Nothing [] genInferredContext) []

{- | This is particularly useful with 'Generic' class.

For the types like 'Int', 'Char','Ratio' or other types which are not 'Generic', there must be a way to stop the generation process on those types.

However, the deriving topdown function will only stop generating 'Generic' instances on primitive types and 'Integer' by default, so you do not need to break on them manually.

Another circumtances might be deriving for 'Typeable' class. Since there is a bug in GHC, isInstance function in TH library is not working on 'Typeable', you can manually give the types which are already instances of 'Typeable' to stop the generation process.

For others cases, there is no need to use this function, bacause for a data type @A@ which is composited by another type, when you manually write an instance declaration for @A@, the process will stop on @A@ automatically since it is already an instance of the type class.
-}
deriving_with_breaks :: Name -- ^ class name
          -> Name -- ^ type name
          -> [Name] -- ^ type names that stop the deriving process
          -> Q [Dec]
deriving_with_breaks cn tn bs = evalStateT (gen_standalone_deriving_decl cn tn Nothing bs genInferredContext) []

derivings :: [Name] -- ^ class names
          -> Name   -- ^ type name
          -> Q [Dec]
derivings cns tn = fmap concat (mapM (\x -> deriving_ x tn) cns)

derivingss :: [Name] -- ^ class names
           -> [Name] -- ^ type names
           -> Q [Dec]
derivingss cns tns = fmap concat (mapM (\x -> derivings cns x) tns)


#if __GLASGOW_HASKELL__ >= 802
strategy_deriving :: DerivStrategy
                  -> Name
                  -> Name
                  -> Q [Dec]

strategy_deriving st cn tn = evalStateT (gen_standalone_deriving_decl cn tn (Just st) [] genInferredContext) []

strategy_derivings :: DerivStrategy
                   -> [Name]
                   -> Name
                   -> Q [Dec]

strategy_derivings st cns tn = fmap concat $ (mapM (\x -> strategy_deriving st x tn) cns)

strategy_derivingss :: DerivStrategy
                    -> [Name]
                    -> [Name]
                    -> Q [Dec]
strategy_derivingss st cns tns = fmap concat $ (mapM (\x -> strategy_derivings st cns x) tns)
#endif

{-| Context generator be the following 3 functions

 1. @genHoleContext@: It requires PartialTypeSignatures to make the context of deriving
    context a `hole' e.g. @_ => Cls (D a b)@. This case cannot handle type family
    since GHC cannot handle it

 2. @genInferredContext@: It will try to infer the context including cases with type families.

 3. @genAllFieldsContext@: It will put all fields into the context. It may generate like the followings

 @
 data List a = Nil | Cons a (List a)
 deriving instance (Show a, Show (List a)) => Show (List a)
 @
-}
deriving_with :: ClassName
              -> TypeName
              -> Maybe DerivStrategy -- ^ deriving strategy
              -> [TypeName]        -- ^ a list of types that breaks the generation process
              -> ContextGenerator -- ^ a context generator,  @genInferredContext@, @genHoleContext@ or @genAllFieldsContext@
              -> Q [Dec]
deriving_with cn tn st bs cg = evalStateT (gen_standalone_deriving_decl cn tn st bs cg) []
