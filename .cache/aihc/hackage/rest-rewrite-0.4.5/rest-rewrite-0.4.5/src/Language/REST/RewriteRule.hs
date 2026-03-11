{-# LANGUAGE MultiParamTypeClasses #-}

module Language.REST.RewriteRule where

import qualified Data.HashSet as S

-- | A class for datatypes that can be used as rewrite rules
class RewriteRule m rule term where
  -- | @apply term rule@ returns the set of resulting terms that can be generated
  --   from @term@ using @rule@. Multiple terms are possible if the rule applies to
  --   multiple subterms. The result is embedded in a computation context @m@;
  --   this enables support for SMT-based conditional rewriting, for example.
  apply :: term -> rule -> m (S.HashSet term)
