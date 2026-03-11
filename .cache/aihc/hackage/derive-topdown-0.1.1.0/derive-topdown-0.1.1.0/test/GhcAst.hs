{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module GhcAst where
import qualified Data.Maybe as Maybe
import qualified "ghc" Language.Haskell.Syntax as G
import           Data.Derive.TopDown
#if __GLASGOW_HASKELL__ >= 906
import Language.Haskell.Syntax.Module.Name
import Language.Haskell.Syntax.ImpExp
import GHC.Types.SrcLoc
import GHC.Hs.Doc
import Language.Haskell.Syntax.Decls
import Language.Haskell.Syntax.Type
import GHC.Types.Var
import Language.Haskell.Syntax.Concrete
import GHC.Data.FastString
import Language.Haskell.Syntax.Expr
import GHC.Types.Name.Reader
import GHC.Unit.Types
import GHC.Types.Name
import GHC.Core.TyCo.Rep
import qualified GHC.Core.TyCon as GCT
import GHC.Types.Unique
import Language.Haskell.Syntax.Basic
import GHC.Types.ForeignCall
import GHC.Types.SourceText
import GHC.Types.Basic
import GHC.Real
import GHC.Core.Coercion.Axiom
import GHC.Arr
import GHC.Data.Pair
import GHC.IORef
import GHC.Data.BooleanFormula
import GHC.Parser.Annotation
import GHC.Data.Strict
import Language.Haskell.Syntax.Lit
import Language.Haskell.Syntax.Binds
import Language.Haskell.Syntax.Pat
import GHC.Types.Fixity
import GHC.Data.Bag
import GHC.Types.Unique.DSet
import GHC.STRef
import GHC.Generics
import Data.ByteString.Short.Internal
import GHC.ForeignPtr
import Data.ByteString.Internal


$(deriving_with ''Generic ''G.HsModule Maybe.Nothing [
    ''ShortByteString, 
    ''ForeignPtr,
    ''Array,
    ''Var,
    ''GCT.TyCon,
    ''IORef,
    ''STRef,
    ''Bag,
    ''STRef, 
    ''FastZString,
    ''RealSrcSpan,
    ''ByteString,
    ''Name,
    ''UniqDSet,
    ''Unique,
    ''OccName] genInferredContext)
#endif