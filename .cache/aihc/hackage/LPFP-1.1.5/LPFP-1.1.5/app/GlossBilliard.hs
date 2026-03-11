{-# OPTIONS -Wall #-}

import LPFP.Mechanics3D ( simulateGloss )
import LPFP.MultipleObjects (eulerCromerMPS )

import LPFP.MOExamples ( billiardInitial, billiardPicture, billiardUpdate )

main :: IO ()
main = simulateGloss 1 100 billiardInitial billiardPicture
       (billiardUpdate eulerCromerMPS 30)
