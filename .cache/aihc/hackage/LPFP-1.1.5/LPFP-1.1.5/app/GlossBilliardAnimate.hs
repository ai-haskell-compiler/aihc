{-# OPTIONS -Wall #-}

import LPFP.MultipleObjects ( eulerCromerMPS )
import LPFP.MOExamples
    ( animateGloss, billiardPicture, billiardStates )

main :: IO ()
main = animateGloss 1 billiardPicture (billiardStates eulerCromerMPS 30 0.01)
