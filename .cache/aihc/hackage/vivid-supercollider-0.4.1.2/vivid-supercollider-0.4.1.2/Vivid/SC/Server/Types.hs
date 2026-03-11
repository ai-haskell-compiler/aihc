module Vivid.SC.Server.Types (
     NodeId(..)
   , BufferId(..)
   , SyncId(..)

   , Group(..)
   , ParGroup(..)
   ) where

import Data.Int

newtype NodeId = NodeId { _unNodeId :: Int32 }
   deriving (Show, Eq, Ord, Read)

newtype BufferId = BufferId { _unBufferId :: Int32 }
   deriving (Show, Eq, Ord, Read)

newtype SyncId = SyncId { _unSyncId :: Int32 }
   deriving (Show, Read, Eq, Ord)

-- Not sure if these be here or in 'vivid'. Depends on what the OSC messages need:
newtype Group = Group { _unGroup :: NodeId }
 deriving (Show, Read, Eq, Ord)

newtype ParGroup = ParGroup { _unParGroup :: NodeId }
 deriving (Show, Read, Eq, Ord)


