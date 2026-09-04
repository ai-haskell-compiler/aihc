{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generic traversals over the parser syntax tree.
--
-- The type checker walks a whole module with "Data.Data". A new syntax
-- constructor then cannot hide an annotation from a pass. A plain generic
-- walk also enters each 'Text', 'String', and 'SourceSpan' value, and the
-- 'Data' instances of those types present each character as one node. No
-- annotation lives inside them, so the traversals here stop at them.
module Aihc.Tc.Generic
  ( everywhereM,
    everything,
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Data.Data (Data, gmapM, gmapQr)
import Data.Text (Text)
import Data.Typeable (Proxy (..), TypeRep, typeOf, typeRep)

-- | Apply a monadic rewrite to every node, bottom-up. The rewrite does not
-- see the leaf values that hold no annotations.
everywhereM :: (Monad m, Data a) => (forall b. (Data b) => b -> m b) -> a -> m a
everywhereM rewrite value
  | isLeaf value = pure value
  | otherwise = gmapM (everywhereM rewrite) value >>= rewrite

-- | Collect the results of a query from every node, in traversal order. The
-- query does not see the leaf values that hold no annotations.
everything :: forall a r. (Data a) => (forall b. (Data b) => b -> [r]) -> a -> [r]
everything query value = collect value []
  where
    -- Prepend the results of one subtree to the results that follow it.
    collect :: forall b. (Data b) => b -> [r] -> [r]
    collect node rest
      | isLeaf node = rest
      | otherwise = query node ++ gmapQr ($) rest collect node

isLeaf :: (Data a) => a -> Bool
isLeaf value =
  rep == textTypeRep || rep == stringTypeRep || rep == sourceSpanTypeRep
  where
    rep = typeOf value

textTypeRep, stringTypeRep, sourceSpanTypeRep :: TypeRep
textTypeRep = typeRep (Proxy :: Proxy Text)
stringTypeRep = typeRep (Proxy :: Proxy String)
sourceSpanTypeRep = typeRep (Proxy :: Proxy SourceSpan)
