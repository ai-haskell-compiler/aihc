module Sound.ALSA.Sequencer.Utility where

import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Foreign.C.Error (Errno(Errno), eNOENT, )

import Data.Maybe.HT (toMaybe, )


showsField :: Show a => a -> ShowS
showsField = showsPrec 11

showsRecord :: Int -> String -> [ShowS] -> ShowS
showsRecord prec name fields =
   showParen (prec >= 10) $
   showString name . showString ".Cons" .
   foldr (.) id (map (\f -> showChar ' ' . f) fields)

-- might be moved to Sound.ALSA.Exception
checkResultQuery :: String -> C.CInt -> IO Bool
checkResultQuery name =
   Exc.checkResultMaybe name
      (const True)
      (\e -> toMaybe (Errno (negate e) == eNOENT) False)
