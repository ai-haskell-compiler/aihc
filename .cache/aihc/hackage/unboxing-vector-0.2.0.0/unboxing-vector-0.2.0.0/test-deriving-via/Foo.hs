{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Foo
  (Foo -- the constructor is not exported!
  ,mkFoo
  ) where
import Data.Vector.Unboxing (Unboxable(..))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Control.DeepSeq (NFData)
import GHC.Generics
import Data.Coerce

newtype Foo = Foo Int deriving (Eq,Show,Generic)

instance NFData Foo

mkFoo :: Foo
mkFoo = Foo 42

-- Comparison of Data.Vector.Unboxing and Data.Vector.Unboxed:

-- The number of lines needed to enable 'Data.Vector.Unboxing.Vector Foo' is ...

instance Unboxable Foo where
  type Rep Foo = Int -- needs TypeFamilies here

-- ... only 2 lines!
-- Also, you can use GeneralizedNewtypeDeriving + UndecidableInstances if you want to write less.

-- On the other hand, the number of lines needed to enable 'Data.Vector.Unboxed.Vector Foo' is ...

newtype instance UM.MVector s Foo = MV_Foo (UM.MVector s Int)
newtype instance U.Vector Foo = V_Foo (U.Vector Int)

instance GM.MVector UM.MVector Foo where -- needs MultiParamTypeClasses here
  basicLength (MV_Foo mv) = GM.basicLength mv
  basicUnsafeSlice i l (MV_Foo mv) = MV_Foo (GM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_Foo mv) (MV_Foo mv') = GM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_Foo <$> GM.basicUnsafeNew l
  basicInitialize (MV_Foo mv) = GM.basicInitialize mv
  basicUnsafeReplicate i x = MV_Foo <$> GM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_Foo mv) i = coerce <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_Foo mv) i x = GM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_Foo mv) = GM.basicClear mv
  basicSet (MV_Foo mv) x = GM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_Foo mv) (MV_Foo mv') = GM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_Foo mv) (MV_Foo mv') = GM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_Foo mv) n = MV_Foo <$> GM.basicUnsafeGrow mv n

instance G.Vector U.Vector Foo where -- needs MultiParamTypeClasses here
  basicUnsafeFreeze (MV_Foo mv) = V_Foo <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (V_Foo v) = MV_Foo <$> G.basicUnsafeThaw v
  basicLength (V_Foo v) = G.basicLength v
  basicUnsafeSlice i l (V_Foo v) = V_Foo (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_Foo v) i = coerce <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Foo mv) (V_Foo v) = G.basicUnsafeCopy mv v
  elemseq (V_Foo v) x y = G.elemseq v (coerce x) y

instance U.Unbox Foo

-- ... enormous!
-- Unfortunately, you cannot use GeneralizedNewtypeDeriving to MVector/Vector classes.
