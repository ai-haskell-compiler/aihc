{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines a utility function for deriving 'Quasi' instances for monad
-- transformer data types.
module Language.Haskell.TH.Instances.Internal (deriveQuasiTrans) where

import qualified Control.Monad.Trans as MTL (lift)
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr (pprint)
import Language.Haskell.TH.Syntax

deriveQuasiTrans ::
     Q Type  -- ^ The instance head. For example, this might be of the form:
             --
             --   > [t| forall r m. Quasi m => Quasi (ReaderT r m) |]
  -> Q Exp   -- ^ The implementation of 'qRecover'
  -> Q [Dec] -- ^ The 'Quasi' instance declaration
deriveQuasiTrans qInstHead qRecoverExpr = do
  instHead    <- qInstHead
  let (instCxt, mangledInstTy) = decomposeType instHead
      qInstCxt = return instCxt
      qInstTy  = case mangledInstTy of
                   ConT proxy2 `AppT` instTy
                     |  proxy2 == ''Quasi
                     -> conT ''Quasi `appT` return instTy
                   _ -> fail $ "Unexpected type " ++ pprint mangledInstTy
  instDec <- instanceD qInstCxt qInstTy qInstMethDecs
  return [instDec]
  where
    decomposeType :: Type -> (Cxt, Type)
    decomposeType (ForallT _tvbs ctxt ty) = (ctxt, ty)
    decomposeType ty                      = ([],   ty)

    qInstMethDecs :: [Q Dec]
    qInstMethDecs =
      let instMeths :: [(Name, Q Exp)]
          instMeths =
            [ -- qRecover is different for each instance
              ('qRecover,            qRecoverExpr)

              -- The remaining methods are straightforward
            , ('qNewName,            [| MTL.lift . qNewName |])
            , ('qReport,             [| \a b -> MTL.lift $ qReport a b |])
            , ('qReify,              [| MTL.lift . qReify |])
            , ('qLocation,           [| MTL.lift qLocation |])
            , ('qRunIO,              [| MTL.lift . qRunIO |])
            , ('qReifyInstances,     [| \a b -> MTL.lift $ qReifyInstances a b |])
            , ('qLookupName,         [| \a b -> MTL.lift $ qLookupName a b |])
            , ('qAddDependentFile,   [| MTL.lift . qAddDependentFile |])
            , ('qReifyRoles,         [| MTL.lift . qReifyRoles |])
            , ('qReifyAnnotations,   [| MTL.lift . qReifyAnnotations |])
            , ('qReifyModule,        [| MTL.lift . qReifyModule |])
            , ('qAddTopDecls,        [| MTL.lift . qAddTopDecls |])
            , ('qAddModFinalizer,    [| MTL.lift . qAddModFinalizer |])
            , ('qGetQ,               [| MTL.lift qGetQ |])
            , ('qPutQ,               [| MTL.lift . qPutQ |])
            , ('qReifyFixity,        [| MTL.lift . qReifyFixity |])
            , ('qReifyConStrictness, [| MTL.lift . qReifyConStrictness |])
            , ('qIsExtEnabled,       [| MTL.lift . qIsExtEnabled |])
            , ('qExtsEnabled,        [| MTL.lift qExtsEnabled |])
#if MIN_VERSION_template_haskell(2,14,0)
            , ('qAddForeignFilePath, [| \a b -> MTL.lift $ qAddForeignFilePath a b |])
            , ('qAddTempFile,        [| MTL.lift . qAddTempFile |])
#elif MIN_VERSION_template_haskell(2,12,0)
            , ('qAddForeignFile,     [| \a b -> MTL.lift $ qAddForeignFile a b |])
#endif
#if MIN_VERSION_template_haskell(2,13,0)
            , ('qAddCorePlugin,      [| MTL.lift . qAddCorePlugin |])
#endif
#if MIN_VERSION_template_haskell(2,16,0)
            , ('qReifyType,          [| MTL.lift . qReifyType |])
#endif
#if MIN_VERSION_template_haskell(2,18,0)
            , ('qGetDoc,             [| MTL.lift . qGetDoc |])
            , ('qPutDoc,             [| \a b -> MTL.lift $ qPutDoc a b |])
#endif
#if MIN_VERSION_template_haskell(2,19,0)
            , ('qGetPackageRoot,     [| MTL.lift qGetPackageRoot |])
#endif
            ]

          mkDec :: Name -> Q Exp -> Q Dec
          mkDec methName methRhs = valD (varP methName) (normalB methRhs) []

      in map (uncurry mkDec) instMeths
