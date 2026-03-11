module Graphics.Gnuplot.Private.LineSpecification where

import qualified Graphics.Gnuplot.Private.ColorSpecification as Color
import Data.Maybe (catMaybes, )
import Graphics.Gnuplot.Utility (quote, )


{-
The Int types would be better enumerations
but their interpretations depend on the gnuplot output type. :-(
-}

data T =
   Cons
      { lineStyle_ :: Maybe Int
      , lineType_  :: Maybe Int
      , lineWidth_ :: Maybe Double
      , lineColor_ :: Maybe Color.T
      , pointType_ :: Maybe Int
      , pointSize_ :: Maybe Double
      , title_     :: Maybe String
      }

deflt :: T
deflt =
   Cons
      { lineStyle_ = Nothing
      , lineType_  = Nothing
      , lineWidth_ = Nothing
      , lineColor_ = Nothing
      , pointType_ = Nothing
      , pointSize_ = Nothing
      , title_     = Nothing
      }

lineStyle :: Int -> T -> T
lineStyle x ls = ls{lineStyle_ = Just x}

lineType :: Int -> T -> T
lineType x ls = ls{lineType_ = Just x}

lineWidth :: Double -> T -> T
lineWidth x ls = ls{lineWidth_ = Just x}

lineColor :: Color.T -> T -> T
lineColor x ls = ls{lineColor_ = Just x}

pointType :: Int -> T -> T
pointType x ls = ls{pointType_ = Just x}

pointSize :: Double -> T -> T
pointSize x ls = ls{pointSize_ = Just x}

title :: String -> T -> T
title x ls = ls{title_ = Just x}


toString :: T -> String
toString linespec =
   let showField :: String -> (a -> String) -> (T -> Maybe a) -> Maybe [String]
       showField s f access = fmap (\a -> [s, f a]) $ access linespec
   in  unwords $ concat $ catMaybes $
       showField "linestyle" show  lineStyle_ :
       showField "linetype"  show  lineType_  :
       showField "linewidth" show  lineWidth_ :
       showField "linecolor" Color.toString lineColor_ :
       showField "pointtype" show  pointType_ :
       showField "pointsize" show  pointSize_ :
       showField "title"     quote title_     :
       []
