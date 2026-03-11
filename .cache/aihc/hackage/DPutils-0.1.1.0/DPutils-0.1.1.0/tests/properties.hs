
module Main where

--import           Control.Lens
--import           Control.Monad.Identity
--import           Data.ByteString (ByteString)
--import qualified Data.ByteString.Char8 as BS
--import qualified Data.ByteString.Lazy as BSL
--import qualified Pipes as P
--import qualified Pipes.ByteString as PB
--import qualified Pipes.Parse as PP
--import qualified Pipes.Prelude as P
import           Test.Tasty
--import           Test.Tasty.SmallCheck as SC
--
--import           Pipes.Split.ByteString

import QuickCheck
import SmallCheck



main :: IO ()
main = do
  defaultMain $ testGroup "all tests" [testQuickCheck, testSmallCheck]

