module Answer (answer) where

import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "aihc_asm_answer" c_answer :: CInt

-- | The number that the assembly function returns.
answer :: Int
answer = fromIntegral c_answer
