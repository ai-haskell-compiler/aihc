{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.TH
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.TH
  ( deriving_th
  , deriving_ths
  , deriving_thss
  , deriving_th_with
  ) where
import           Control.Monad.State
import           Data.Derive.TopDown.IsInstance ( isInstance' )
import           Data.Derive.TopDown.Lib
import           Data.Derive.TopDown.Types
import           Data.List                      ( foldl1' )
import           Language.Haskell.TH

genTH
  :: (ClassName, Name -> Q [Dec])
  -> TypeName
  -> [TypeName]
  -> StateT [Type] Q [Dec]
genTH (className, deriveFunction) typeName bs = do
  (tvbs, cons) <- lift $ getTyVarCons typeName
  let typeNames = map getTVBName tvbs
  isCnHighOrderClass <- lift $ isHigherOrderClass className
  saturatedType      <- lift $ foldl1' appT (conT typeName : map varT typeNames)
  instanceType       <- if isCnHighOrderClass && (not . null) typeNames
    then
      let pns = init typeNames
      in  if null pns
            then lift $ conT typeName
            else lift $ foldl1' appT (conT typeName : (map varT pns))
    else return saturatedType
  isMember <- lift $ isInstance' className [instanceType]
  table    <- get
  if isMember || elem instanceType table || elem typeName bs
    then return []
    else do
      decl <- lift $ deriveFunction typeName
      modify (instanceType :)
      subTypeNames <- lift $ fmap concat $ mapM getCompositeTypeNames cons
      decls <- mapM (\n -> genTH (className, deriveFunction) n bs) subTypeNames
      return $ concat decls ++ decl

deriving_th
  :: (Name, Name -> Q [Dec]) -- ^ class name and corresponding instance generation function
  -> Name -- ^ type name
  -> Q [Dec]
deriving_th cd tname = evalStateT (genTH cd tname []) []

deriving_ths
  :: [(Name, Name -> Q [Dec])] -- ^ class names and corresponding instance generation functions
  -> Name -- ^ type name
  -> Q [Dec]
deriving_ths cds typeName =
  fmap concat (mapM (\c -> deriving_th c typeName) cds)

deriving_thss
  :: [(Name, Name -> Q [Dec])] -- ^ class names and corresponding instance generation functions
  -> [Name] -- ^ type names
  -> Q [Dec]
deriving_thss cds typeNames =
  fmap concat (mapM (\t -> deriving_ths cds t) typeNames)

deriving_th_with
  :: (ClassName, Name -> Q [Dec]) -- ^ class name and corresponding instance generation function
  -> TypeName    -- ^ type name
  -> [TypeName]  -- ^ type name list that breaks deriving process
  -> Q [Dec]
deriving_th_with cd tname bs = evalStateT (genTH cd tname bs) []
