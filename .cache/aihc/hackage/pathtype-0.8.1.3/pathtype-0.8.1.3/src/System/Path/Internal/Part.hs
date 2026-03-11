module System.Path.Internal.Part where

import qualified System.Path.Internal.Component as PC
import System.Path.Internal.Component
        (Component(Component), GenComponent)

import Control.DeepSeq (NFData(rnf))


newtype Abs = Abs GenComponent
data Rel = Rel
data AbsRel = AbsO GenComponent | RelO

absPC :: String -> Abs
absPC = Abs . Component

newtype File = File GenComponent
data Dir = Dir
data FileDir = FileDir


instance NFData Abs where
    rnf (Abs drive) = rnf drive

instance NFData Rel where
    rnf Rel = ()

instance NFData AbsRel where
    rnf (AbsO drive) = rnf drive
    rnf RelO = ()

instance NFData File where
    rnf (File pc) = rnf pc

instance NFData Dir where
    rnf Dir = ()

instance NFData FileDir where
    rnf FileDir = ()


fileMap :: (String -> String) -> File -> File
fileMap f (File pc) = File $ PC.map f pc
