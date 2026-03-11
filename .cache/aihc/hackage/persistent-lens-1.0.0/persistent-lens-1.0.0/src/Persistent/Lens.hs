{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Lenses for persistent.
module Persistent.Lens(entityTupleIso, valueLens, keyLens) where

import Control.Lens
import Database.Persist.Types
import Prelude

-- | an isomorphism from an entity to a tuple
entityTupleIso :: Iso' (Entity a) (Key a, a)
entityTupleIso = iso (\(Entity a b) -> (a, b)) $ uncurry Entity

-- | A lens from an entity to it's value
valueLens :: Lens' (Entity a) a
valueLens = entityTupleIso . _2

-- | A lens from an entity to it's key
keyLens :: Lens' (Entity a) (Key a)
keyLens = entityTupleIso . _1
