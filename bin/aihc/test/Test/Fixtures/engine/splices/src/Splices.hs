module Splices (application, text, unique, recovered, dependentFile, bytes) where

import Aihc.TH.Splice (runSplice)
import Language.Haskell.TH
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Array (pokeArray)
import Language.Haskell.TH.Syntax (Bytes (..), addDependentFile)

application :: IO ()
application = runSplice (appE (varE (mkName "negate")) (litE (integerL (-7))))

text :: IO ()
text = runSplice (tupE [litE (stringL "h\233llo"), litE (charL 'x'), conE (mkName "Data.Maybe.Nothing")])

unique :: IO ()
unique =
  runSplice $ do
    reportWarning "a warning"
    x <- newName "x"
    lamE [varP x] (varE x)

recovered :: IO ()
recovered = runSplice (recover (litE (integerL 1)) (reportError "an error" >> fail "stop"))

-- | A splice names a file that the module depends on.
dependentFile :: IO ()
dependentFile = runSplice (addDependentFile "data/table.txt" >> litE (integerL 2))

-- | A splice gives a primitive bytes literal, as the Lift instance of Text
-- does. The bytes are in memory of a foreign pointer.
bytes :: IO ()
bytes =
  runSplice $ do
    pointer <- runIO $ do
      memory <- mallocForeignPtrBytes 5
      withForeignPtr memory (\start -> pokeArray start [0x78, 0x61, 0x62, 0xE9, 0x79 :: Word8])
      pure memory
    litE (bytesPrimL (Bytes pointer 1 3))
