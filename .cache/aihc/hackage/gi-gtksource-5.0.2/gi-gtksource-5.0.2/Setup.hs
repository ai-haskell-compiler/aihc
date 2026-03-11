import Data.GI.CodeGen.CabalHooks (setupCompatWrapper)
import qualified GI.GtkSource.Config as Cfg

main :: IO ()
main = setupCompatWrapper "gi-gtksource5" Cfg.modules
