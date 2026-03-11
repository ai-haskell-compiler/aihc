module PureSAT.LCG (
    LCG,
    newLCG,
    nextLCG,
) where

import Control.Monad.ST       (ST)
import Data.Primitive.PrimVar
import Data.Word              (Word64)

-- $setup
-- >>> import Control.Monad.ST (runST)

-- | Park-Miller LCG
--
-- >>> runST $ do { lcg <- newLCG 42; traverse (\_ -> nextLCG lcg) [1..10] }
-- [2027382,1226992407,551494037,961371815,1404753842,2076553157,1350734175,1538354858,90320905,488601845]
--
newtype LCG s = LCG (PrimVar s Word64)

newLCG :: Word64 -> ST s (LCG s)
newLCG seed = LCG <$> newPrimVar (max 1 (min 0x7fffffe seed))

nextLCG :: LCG s -> ST s Word64
nextLCG (LCG state) = do
    s <- readPrimVar state
    let !s' = mod (s * 48271) 0x7fffffff
    writePrimVar state s'
    return s'
