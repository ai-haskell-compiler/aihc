import Data.GI.CodeGen.CabalHooks (setupCompatWrapper)
import qualified GI.Gtk.Config as Cfg

main :: IO ()
main = setupCompatWrapper "gi-gtk4" Cfg.modules
