import Data.GI.CodeGen.CabalHooks (setupCompatWrapper)
import qualified GI.JavaScriptCore.Config as Cfg

main :: IO ()
main = setupCompatWrapper "gi-javascriptcore6" Cfg.modules
