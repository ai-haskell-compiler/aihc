-- |
-- These are the beginnings of a Lens API.
-- It is compatible with the general Van Laarhoven lens libraries, such as \"lens\".
--
-- Many more definitions can be implemented, so do PRs if you miss any!
module Iri.Optics
  ( module Basics,
    module Defs,
  )
where

import Iri.Optics.Basics as Basics (Lens, Lens', Prism, Prism', Traversal, Traversal')
import Iri.Optics.Defs as Defs
