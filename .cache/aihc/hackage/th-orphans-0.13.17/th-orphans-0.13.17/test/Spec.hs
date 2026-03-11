{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Exception (evaluate)
import Language.Haskell.TH
import Language.Haskell.TH.Instances
import Language.Haskell.TH.Lift
import System.Timeout
import System.Mem
import Test.Hspec hiding (runIO)
import TestUtil
import qualified Data.ByteString as BS

main :: IO ()
main = hspec $ do
    -- See https://github.com/mgsloan/th-orphans/issues/13
    it "Doesn't infinite loop when comparing types" $ do
        -- This can either yield LT or GT because different TH
        -- versions define the constructors in different orders.
        result <- timeout (1000 * 100) $
            compare (AppT (ConT ''Maybe) (ConT ''Int)) (ConT ''Char)
                `shouldSatisfy` (/= EQ)
        result `shouldBe` Just ()
    -- See https://github.com/mgsloan/th-orphans/issues/14
    it "Compares types correctly" $
        compare (AppT (ConT ''Maybe) (ConT ''Int)) (AppT (ConT ''Maybe) (ConT ''Char))
            `shouldBe` GT
#if MIN_VERSION_template_haskell(2,16,0)
    it "Lifts bytes" $ do
        let addr = $(do
                let result = $(do
                        ast <- lift (LitE (BytesPrimL (bsToBytes testBytes)))
                        runIO performMajorGC
                        return ast)
                runIO $ evaluate result
                runIO performMajorGC
                return result)
        bs <- addrToBs addr (BS.length testBytes)
        bs `shouldBe` testBytes
#endif
