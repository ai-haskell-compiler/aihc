import Data.GI.CodeGen.CabalHooks (setupCompatWrapper)
import qualified GI.Gdk.Config as Cfg

main :: IO ()
main = setupCompatWrapper "gi-gdk4" Cfg.modules
