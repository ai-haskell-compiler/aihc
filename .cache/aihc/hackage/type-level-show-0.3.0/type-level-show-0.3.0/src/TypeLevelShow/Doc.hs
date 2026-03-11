{-# LANGUAGE AllowAmbiguousTypes #-} -- for reifying

module TypeLevelShow.Doc
  (
  -- * Term level
    Doc(..)
  , renderDoc

  -- * Type level
  , PDoc
  , RenderPDoc
  , ErrorPDoc

  -- * Singleton
  , SDoc(..)
  , SingDoc(singDoc)
  , demoteDoc
  , reifyDoc
  , errorPDoc
  ) where

import GHC.TypeLits qualified as TE -- TE = TypeError
import GHC.TypeLits hiding ( ErrorMessage(..) )
import Singleraeh.Demote
import Singleraeh.SingI

-- | Simple pretty document ADT.
--
-- Designed to work on both type level (as a limited 'TE.ErrorMessage') and term
-- level (as a boring ADT).
--
-- Note that 'TE.ShowType' is magical (see
-- @compiler/GHC/Core/Type.hs#L1309@), so we need to remove it for term level.
--
-- singletons-base defines a version of this, but retains the 'TE.ShowType'
-- constructor and is in the singletons ecosystem.
data Doc s
  = Text s
  -- ^ plain ol' text
  | Doc s :<>: Doc s
  -- ^ append docs next to each other
  | Doc s :$$: Doc s
  -- ^ stack docs on top of each other (newline)
    deriving stock Show

-- | Render a 'Doc' as a 'String', formatted how 'ErrorMessage's get displayed.
renderDoc :: Doc String -> String
renderDoc doc = "    • " <> go doc
  where
    go = \case
      Text s   -> s
      l :<>: r -> go l <>               go r
      l :$$: r -> go l <> "\n      " <> go r

-- | Promoted 'Doc'.
type PDoc = Doc Symbol

-- | Render a 'PDoc' as an 'ErrorMessage', for type-level error messages.
--
-- 'PDoc' is a subset of 'ErrorMessage', so this is very boring.
type RenderPDoc :: PDoc -> TE.ErrorMessage
type family RenderPDoc doc where
    RenderPDoc (Text s)   = TE.Text s
    RenderPDoc (l :<>: r) = RenderPDoc l TE.:<>: RenderPDoc r
    RenderPDoc (l :$$: r) = RenderPDoc l TE.:$$: RenderPDoc r

-- | Render a 'PDoc' as an 'ErrorMessage', and wrap it in a 'TypeError'.
--
-- Note that this must be a type family, or the 'PDoc' won't actually get
-- rendered.
type ErrorPDoc :: PDoc -> a
type family ErrorPDoc doc where ErrorPDoc doc = TypeError (RenderPDoc doc)

-- | Singleton 'Doc'.
data SDoc (doc :: PDoc) where
    SText   :: SSymbol s -> SDoc (Text s)
    (:$<>:) :: SDoc docl -> SDoc docr -> SDoc (docl :<>: docr)
    (:$$$:) :: SDoc docl -> SDoc docr -> SDoc (docl :$$: docr)

-- | Demote an 'SDoc' singleton to the corresponding base 'Doc'.
demoteDoc :: SDoc doc -> Doc String
demoteDoc = \case
  SText s   -> Text $ fromSSymbol s
  l :$<>: r -> demoteDoc l :<>: demoteDoc r
  l :$$$: r -> demoteDoc l :$$: demoteDoc r

instance Demotable SDoc where
    type Demote SDoc = Doc String
    demote = demoteDoc

-- | Produce the singleton for the given promoted 'Doc'.
class SingDoc (doc :: PDoc) where
    singDoc :: SDoc doc

instance KnownSymbol s => SingDoc (Text s) where
    singDoc = SText (SSymbol @s)

instance (SingDoc l, SingDoc r) => SingDoc (l :<>: r) where
    singDoc = singDoc @l :$<>: singDoc @r

instance (SingDoc l, SingDoc r) => SingDoc (l :$$: r) where
    singDoc = singDoc @l :$$$: singDoc @r

instance SingDoc doc => SingI doc where
    type Sing = SDoc
    sing' = singDoc

-- | Reify a promoted 'Doc' to the corresponding term-level one.
reifyDoc :: forall (doc :: PDoc). SingDoc doc => Doc String
reifyDoc = demoteDoc (singDoc @doc)

-- | Render a 'PDoc' as a 'String', and call 'error' on it.
--
-- This enables using the same code for type- and term- "runtime" errors.
-- This can't be typechecked, naturally, but it's still nice.
errorPDoc :: forall (doc :: PDoc) a. SingDoc doc => a
-- add extra newline because runtime errors print like @*** Exception: _@
errorPDoc = error $ ("\n" <>) $ renderDoc $ reifyDoc @doc
