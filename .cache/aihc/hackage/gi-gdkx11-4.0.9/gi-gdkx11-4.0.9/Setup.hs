import Data.GI.CodeGen.CabalHooks (setupCompatWrapper)
import qualified GI.GdkX11.Config as Cfg

main :: IO ()
main = setupCompatWrapper "gi-gdkx114" Cfg.modules
