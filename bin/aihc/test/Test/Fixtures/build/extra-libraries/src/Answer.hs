module Answer (answer) where

import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "aihc_extra_libraries_answer" c_answer :: CInt

-- | The number that the C archive @libanswer.a@ gives.
answer :: Int
answer = fromIntegral c_answer
