module Sound.MED.Raw.SampleInstr where

import Sound.MED.Basic.Amiga
import Sound.MED.Basic.Utility

data SampleInstr = SampleInstr
  { octaves :: WORD
  , chans :: Either [[BYTE]] [[WORD]]
  }
  deriving Show

{-# SPECIALISE peek :: ULONG -> WORD -> Bool -> Bool -> PTR -> StorableReader SampleInstr #-}
{-# SPECIALISE peek :: ULONG -> WORD -> Bool -> Bool -> PTR -> ByteStringReader SampleInstr #-}
peek :: (Reader m) => ULONG -> WORD -> Bool -> Bool -> PTR -> m SampleInstr
peek len' stype' s16' stereo' p = do
  let octaves' = [1,5,3,2,4,6,7,9]!!fromIntegral stype'
  case (s16',stereo') of
    (False, False) -> do { dat'' <- mapM peekBYTE $ pointerRangeGen (p+6) 1 len'             ; return $ SampleInstr octaves' $ Left [dat''] }
    (True,  False) -> do { dat'' <- mapM peekWORD $ pointerRangeGen (p+6) 2 (len'`div`2)     ; return $ SampleInstr octaves' $ Right [dat''] }
    (False, True ) -> do { dat'' <- mapM peekBYTE $ pointerRangeGen (p+6) 1 (2*len')         ; return $ SampleInstr octaves' $ Left (chunk len' dat'') }
    (True,  True ) -> do { dat'' <- mapM peekWORD $ pointerRangeGen (p+6) 2 (2*(len'`div`2)) ; return $ SampleInstr octaves' $ Right (chunk (len'`div`2) dat'') }
