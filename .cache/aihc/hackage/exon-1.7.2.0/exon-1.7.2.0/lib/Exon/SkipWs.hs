-- | Whitespace skipping quoter, internal
module Exon.SkipWs where

import Language.Haskell.TH.Quote (QuasiQuoter)

import Exon.Class.Exon (ExonString (exonWhitespace))
import Exon.Data.Result (Result (Empty))
import Exon.Quote (exonWith)

-- | Wrapping a quote type with this causes whitespace to be ignored.
--
-- @since 1.0.0.0
newtype SkipWs a =
  SkipWs a
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

-- | Defined separately because TH chokes on the selector.
--
-- @since 1.0.0.0
skipWs :: SkipWs a -> a
skipWs (SkipWs a) =
  a

-- | The instance used when the result type is wrapped in 'SkipWs', which is done by 'Exon.intron'.
--
-- It returns 'Empty' for any whitespace.
instance (
    IsString builder
  ) => ExonString (SkipWs result) builder where
  exonWhitespace _ =
    Empty
  {-# inline exonWhitespace #-}

-- | A variant of 'Exon.exon' that ignores all literal whitespace in the quote (not in interpolated expressions).
--
-- > [intron|x|] === skipWs [exonws|x|]
--
-- @since 1.0.0.0
intron :: QuasiQuoter
intron =
  exonWith (Just ([e|SkipWs|], [e|skipWs|])) True False
