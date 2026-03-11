{-# Language TypeApplications, DataKinds, TypeInType, KindSignatures #-}

import Data.IndexedListLiterals
import GHC.TypeLits
import Data.Kind
import Test.Hspec

data Matrix (x :: Nat) (y :: Nat) (ty :: Type)

data Vector (length :: Nat) (ty :: Type)

vector :: ILL input length output => input -> Vector length output
vector = undefined

v :: Vector 3 Int
v = vector (1,2,3)

x :: Vector 0 Double
x = vector $ ZeroTuple @Double

y :: Vector 1 Double
y = vector (Only 1)

z :: Vector 2 String
z = vector ("Hello", "World")

matrix :: (ILL row width ty, ILL matrix height row) => matrix -> Matrix height width ty
matrix = undefined

a :: Matrix 0 0 Bool
a = matrix $ ZeroTuple @(ZeroTuple Bool)

b :: Matrix 1 2 String
b = matrix $ Only ("Hello","World")

c :: Matrix 4 5 Double
c = matrix ((1,2,3,0,0)
           ,(4,5,6,0,0)
           ,(7,8,9,0,0)
           ,(0,0,0,0,0))

-- | just making sure the docs type check
main :: IO ()
main = hspec $ do
  describe "fromListP" $
    it "should follow it's documentation" $ do
      fromListP (len @3) [1,2,3] `shouldBe` Just (1,2,3)
      fromListP (len @3) ["word","up"] `shouldBe`  Nothing
      fromListP (len @1) ['z'] `shouldBe` Just (Only 'z')

  describe "fromList" $
    it "should follow it's documentation" $ do
      (fromList [1,2,3] :: Maybe (Int, Int, Int)) `shouldBe` Just (1,2,3)
      (fromList ["word","up"] :: Maybe (String, String, String)) `shouldBe`  Nothing
      (fromList ['z'] :: Maybe (Only Char)) `shouldBe` Just (Only 'z')


