module System.Path.Internal.Component where

import qualified System.Path.Internal.Separator as Sep
import System.Path.Internal.System (System, canonicalize)

import Control.DeepSeq (NFData(rnf))
import Control.Applicative ((<$>))

import qualified Data.List.HT as ListHT
import Data.Tagged (Tagged(Tagged))
import Data.List (isPrefixOf)
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (mapFst)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)

import Prelude hiding (map)


newtype Component os = Component String

empty :: Component os
empty = Component ""

instance NFData (Component os) where
    rnf (Component pc) = rnf pc

instance (System os) => Eq (Component os) where
    (==)  =  equating (applyComp canonicalize)

instance (System os) => Ord (Component os) where
    compare  =  comparing (applyComp canonicalize)

applyComp :: Tagged os (String -> String) -> Component os -> String
applyComp (Tagged canon) (Component pc) = canon pc

retag :: GenComponent -> Component os
retag (Component pc) = Component pc

untag :: Component os -> GenComponent
untag (Component pc) = Component pc


map :: (String -> String) -> Component os -> Component os
map f (Component s) = Component $ f s

mapF ::
    (Functor f) =>
    (String -> f String) -> Component os -> f (Component os)
mapF f (Component s) = Component <$> f s



addExtension :: Component os -> String -> Component os
addExtension p "" = p
addExtension (Component pc) ext =
    Component $ pc ++
        if [Sep.extension] `isPrefixOf` ext
          then ext
          else Sep.extension : ext

splitExtension :: Component os -> (Component os, String)
splitExtension (Component s) =
    mapFst Component $
    maybe (s, "") (mapFst concat) $
    ((\p@(pcs,_) -> toMaybe (not (null pcs)) p) =<<) $ ListHT.viewR $
    ListHT.segmentBefore Sep.isExtension s

_splitExtension :: Component os -> (Component os, String)
_splitExtension (Component s) =
    mapFst Component $
    case break Sep.isExtension $ reverse s of
        (_, "") -> (s, "")
        (rext, dot:rstem) -> (reverse rstem, dot : reverse rext)

splitExtensions :: Component os -> (Component os, String)
splitExtensions (Component s) =
    mapFst Component $ break Sep.isExtension s



data Generic = Generic

{- |
We cannot have a Component without phantom types plus a Tagged wrapper,
because we need specialised Eq and Ord instances.
-}
type GenComponent = Component Generic
