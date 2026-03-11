module Numeric.Morpheus.ActivationTest
(
  tests
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Plus

import Numeric.LinearAlgebra
import Numeric.Morpheus.Activation


x = row [10e10, 0, -10e10]
x_sigmoid = row [1, 0.5, 0]
x_sigmoid_gradient = row [0, 0.25, 0]
x_relu = row [10e10, 0, 0]
x_relu_gradient = row [1, 0, 0]
x_tanh = row [1, 0, -1]
x_tanh_gradient = row [0, 1, 0]


tests = [ testGroup "Sigmoid" [
            testCase "Sigmoid" $ assertMatrix "" 1e-10 x_sigmoid (sigmoid x)
            , testCase "Sigmoid Gradient" $ assertMatrix "" 1e-10 x_sigmoid_gradient (sigmoidGradient x)
            ]
        , testGroup "ReLu" [
            testCase "ReLu" $ assertMatrix "" 1e-10 x_relu (relu x)
            , testCase "ReLu Gradient" $ assertMatrix "" 1e-10 x_relu_gradient (reluGradient x)
            ]
        , testGroup "Tanh" [
            testCase "Tanh" $ assertMatrix "" 1e-10 x_tanh (tanh_ x)
            , testCase "Tanh Gradient" $ assertMatrix "" 1e-10 x_tanh_gradient (tanhGradient x)
            ]
        ]
