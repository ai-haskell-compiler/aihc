{-# LANGUAGE CPP, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module Debian.TH
    ( here
    , Loc
    ) where

import Data.List (intersperse)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Distribution.Pretty (Pretty(..))
import Language.Haskell.TH (ExpQ, Loc(..), location)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
--import Text.PrettyPrint (Doc, text)
import Text.PrettyPrint.HughesPJClass (Doc, hcat, text)

here :: ExpQ
here = lift =<< location

instance Pretty Loc where
    pretty = prettyLoc

prettyLoc :: Loc -> Doc
prettyLoc (Loc _filename _package modul (line, col) _) = text (modul <> ":" ++ show line ++ ":" ++ show col)

instance Pretty [Loc] where
    pretty locs = text "[" <> hcat (intersperse (text " → ") (fmap prettyLoc locs)) <> text "]"
