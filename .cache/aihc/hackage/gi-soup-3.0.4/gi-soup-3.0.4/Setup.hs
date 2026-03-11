import Data.GI.CodeGen.CabalHooks (setupCompatWrapper)
import qualified GI.Soup.Config as Cfg

main :: IO ()
main = setupCompatWrapper "gi-soup3" Cfg.modules
