{-|
Module: Numeric.Morpheus.Activation
Description: Activation Functions
Copyright: (c) Alexander Ignatyev, 2017
License: BSD-3
Stability: experimental
Portability: POSIX
-}

module Numeric.Morpheus.Activation
(
  sigmoid
  , sigmoidGradient
  , relu
  , reluGradient
  , tanh_
  , tanhGradient
)

where


import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel
import System.IO.Unsafe(unsafePerformIO)
import Foreign.C.Types
import Foreign.Ptr(Ptr)
import Foreign.Storable


createMatrixOfShape :: Storable a => Matrix a -> IO (Matrix a)
createMatrixOfShape m = createMatrix (orderOf m) (rows m) (cols m)


{- morpheus_sigmoid -}
foreign import ccall unsafe "morpheus_sigmoid"
  c_morpheus_sigmoid :: CInt -> Ptr Double -> Ptr Double -> IO ()


call_morpheus_sigmoid :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                      -> CInt -> CInt -> CInt -> CInt -> Ptr Double
                      -> IO ()
call_morpheus_sigmoid rows cols _ _ xPtr _ _ _ _ yPtr = do
  c_morpheus_sigmoid (rows*cols) xPtr yPtr


-- | Calculates sigmoid.
sigmoid :: Matrix R -> Matrix R
sigmoid m = unsafePerformIO $ do
    y <- createMatrixOfShape m
    apply m (apply y id) call_morpheus_sigmoid
    return y


{- morpheus_sigmoid_gradient -}
foreign import ccall unsafe "morpheus_sigmoid_gradient"
  c_morpheus_sigmoid_gradient :: CInt -> Ptr Double -> Ptr Double -> IO ()


call_morpheus_sigmoid_gradient :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                               -> CInt -> CInt -> CInt -> CInt -> Ptr Double
                               -> IO ()
call_morpheus_sigmoid_gradient rows cols _ _ xPtr _ _ _ _ yPtr = do
  c_morpheus_sigmoid_gradient (rows*cols) xPtr yPtr


-- | Calculates derivates of sigmoid.
sigmoidGradient :: Matrix R -> Matrix R
sigmoidGradient m = unsafePerformIO $ do
    y <- createMatrixOfShape m
    apply m (apply y id) call_morpheus_sigmoid_gradient
    return y


{- morpheus_relu -}
foreign import ccall unsafe "morpheus_relu"
  c_morpheus_relu :: CInt -> Ptr Double -> Ptr Double -> IO ()


call_morpheus_relu :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                      -> CInt -> CInt -> CInt -> CInt -> Ptr Double
                      -> IO ()
call_morpheus_relu rows cols _ _ xPtr _ _ _ _ yPtr = do
  c_morpheus_relu (rows*cols) xPtr yPtr


-- | Calculates ReLu.
relu :: Matrix R -> Matrix R
relu m = unsafePerformIO $ do
    y <- createMatrixOfShape m
    apply m (apply y id) call_morpheus_relu
    return y


{- morpheus_relu_gradient -}
foreign import ccall unsafe "morpheus_relu_gradient"
  c_morpheus_relu_gradient :: CInt -> Ptr Double -> Ptr Double -> IO ()


call_morpheus_relu_gradient :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                               -> CInt -> CInt -> CInt -> CInt -> Ptr Double
                               -> IO ()
call_morpheus_relu_gradient rows cols _ _ xPtr _ _ _ _ yPtr = do
  c_morpheus_relu_gradient (rows*cols) xPtr yPtr


-- | Calculates derivates of ReLu.
reluGradient :: Matrix R -> Matrix R
reluGradient m = unsafePerformIO $ do
    y <- createMatrixOfShape m
    apply m (apply y id) call_morpheus_relu_gradient
    return y

{- morpheus_tanh -}
foreign import ccall unsafe "morpheus_tanh"
  c_morpheus_tanh :: CInt -> Ptr Double -> Ptr Double -> IO ()


call_morpheus_tanh :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                      -> CInt -> CInt -> CInt -> CInt -> Ptr Double
                      -> IO ()
call_morpheus_tanh rows cols _ _ xPtr _ _ _ _ yPtr = do
  c_morpheus_tanh (rows*cols) xPtr yPtr


-- | Calculates tanh.
tanh_ :: Matrix R -> Matrix R
tanh_ m = unsafePerformIO $ do
    y <- createMatrixOfShape m
    apply m (apply y id) call_morpheus_tanh
    return y


{- morpheus_tanh_gradient -}
foreign import ccall unsafe "morpheus_tanh_gradient"
  c_morpheus_tanh_gradient :: CInt -> Ptr Double -> Ptr Double -> IO ()


call_morpheus_tanh_gradient :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                               -> CInt -> CInt -> CInt -> CInt -> Ptr Double
                               -> IO ()
call_morpheus_tanh_gradient rows cols _ _ xPtr _ _ _ _ yPtr = do
  c_morpheus_tanh_gradient (rows*cols) xPtr yPtr


-- | Calculates derivates of tanh.
tanhGradient :: Matrix R -> Matrix R
tanhGradient m = unsafePerformIO $ do
    y <- createMatrixOfShape m
    apply m (apply y id) call_morpheus_tanh_gradient
    return y
