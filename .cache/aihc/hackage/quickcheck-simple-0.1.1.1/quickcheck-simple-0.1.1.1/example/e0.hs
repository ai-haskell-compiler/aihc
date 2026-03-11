
import Test.QuickCheck.Simple
import System.IO.Error

boolTQ :: Test
boolTQ = boolTest "true" True

boolTV :: Test
boolTV = boolTest' "true-verbose" "verbose true" True

boolFQ :: Test
boolFQ = boolTest "false" False

boolFV :: Test
boolFV = boolTest' "false-verbose" "verbose false error message" False

eqTQ :: Test
eqTQ = eqTest "eq" 1 (1 :: Int)

eqTV :: Test
eqTV = eqTest' (==) show "eq-verbose" 1 (1 :: Int)

eqFQ :: Test
eqFQ = eqTest "neq" 2 (1 :: Int)

eqFV :: Test
eqFV = eqTest' (==) show "neq-verbose" 2 (1 :: Int)

qcT :: Test
qcT = qcTest "qc-true" (\x -> (x :: Int) == x)

qcF :: Test
qcF = qcTest "qc-false" (\x -> (x :: Int) == x + 1)

successTests :: [Test]
successTests =
  [ boolTQ
  , boolTV
  , eqTQ
  , eqTV
  , qcT
  ]

allTests :: [Test]
allTests =
  [ boolTQ
  , boolTV
  , boolFQ
  , boolFV
  , eqTQ
  , eqTV
  , eqFQ
  , eqFV
  , qcT
  , qcF
  ]

putLine :: IO ()
putLine = putStrLn "\n------------------------------\n"

main :: IO ()
main = do
  verboseMain successTests
  putLine
  _ <- tryIOError $ defaultMain allTests
  putLine
  _ <- tryIOError $ verboseMain allTests
  return ()
