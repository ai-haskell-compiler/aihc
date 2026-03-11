{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  CLI.Arguments.Parsing
-- Copyright   :  (c) OleksandrZhabenko 2022-2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A library to process command line arguments in some more convenient way.

module CLI.Arguments.Parsing where

import GHC.Base
import GHC.List
import Data.Monoid (mappend)
import Data.Maybe (fromJust)
import GHC.Arr
import qualified Data.Foldable as F
import CLI.Arguments

args2ArgsR
  :: CLSpecifications
  -> (Args,[String])
  -> (Args,[String])
args2ArgsR ((t@(xs,n)):ms) q@(ls,xss@(js:jss))
  | n < 0 = case w0ss of
             [] -> args2ArgsR ms q
             w00ss -> case qss of
                       [] -> args2ArgsR ms q
                       q0ss -> args2ArgsR ms (C xs q0ss:ls,kss `mappend` rss)
  | n == 0 = case w0ss of
              [] -> args2ArgsR ms q
              w00ss -> args2ArgsR ms (A xs:ls,kss `mappend` wss)
  | otherwise = case w0ss of
                 [] -> args2ArgsR ms q
                 w00ss -> if length vss == n then args2ArgsR ms (B n xs vss:ls,kss `mappend` zss)
                          else args2ArgsR ms q
      where (kss,uss) = break (== xs) xss
            (w0ss,wss) = splitAt 1 uss
            (qss,pss) = break (== xs) wss
            (r0ss,rss) = splitAt 1 pss
            (vss,zss) = splitAt n wss
args2ArgsR _ q = q

args2ArgsR0
  :: CLSpecifications
  -> [String]
  -> (Args,[String])
args2ArgsR0 xs yss = args2ArgsR xs ([],yss)
{-# INLINABLE args2ArgsR0 #-}

args2Args3'R
  :: CLSpecifications
  -> (Args,Args,Args,[String])
  -> (Args,Args,Args,[String])
args2Args3'R (t@(xs,n):ts) q@(w1,w2,w3,xss@(js:jss))
  | n < 0 = case w0ss of
             [] -> args2Args3'R ts q
             w00ss -> case qss of
                       [] -> args2Args3'R ts q
                       q0ss -> args2Args3'R ts (w1,w2,C xs q0ss:w3,kss `mappend` rss)
  | n == 0 = case w0ss of
              [] -> args2Args3'R ts q
              w00ss -> args2Args3'R ts (A xs:w1,w2,w3,kss `mappend` wss)
  | otherwise = case w0ss of
                 [] -> args2Args3'R ts q
                 w00ss -> if length vss == n then args2Args3'R ts (w1,B n xs vss:w2,w3,kss `mappend` zss)
                          else args2Args3'R ts q
      where (kss,uss) = break (== xs) xss
            (w0ss,wss) = splitAt 1 uss
            (qss,pss) = break (== xs) wss
            (r0ss,rss) = splitAt 1 pss
            (vss,zss) = splitAt n wss
args2Args3'R _ q = q

args2Args3R
  :: CLSpecifications
  -> [String]
  -> (Args,Args,Args,[String])
args2Args3R xs yss = args2Args3'R xs ([],[],[],yss)
{-# INLINABLE args2Args3R #-}

------------------------------------------------------------------------------

args2Args1R
  :: FirstChars -- ^ A pair of the first characters of the starting group delimiter (the same for all 'String's in the all 'CLSpecifications') and the probable its modification (the first character of the last delimiter).
  -> CLSpecifications
  -> (Args,[String])
  ->  (Args,[String])
args2Args1R (x1,x2) (t@(xs@(k:ks),n):ts) q@(ls,xss@(js:jss))
  | n < 0 = case w0ss of
             [] -> args2Args1R (x1,x2) ts q
             w00ss -> case qss of
                       [] -> args2Args1R (x1,x2) ts q
                       q0ss -> args2Args1R (x1,x2) ts (C xs qss:ls,kss `mappend` rss)
  | n == 0 = case w0ss of
              [] -> args2Args1R (x1,x2) ts q
              w00ss -> args2Args1R (x1,x2) ts (A xs:ls,kss `mappend` wss)
  | otherwise = case w0ss of
                 [] -> args2Args1R (x1,x2) ts q
                 w00ss -> if length vss == n then args2Args1R (x1,x2) ts (B n xs vss:ls,kss `mappend` zss)
                          else args2Args1R (x1,x2) ts q
      where (kss,uss) = break (== xs) xss
            (w0ss,wss) = splitAt 1 uss
            (qss,pss) = break (\rs -> rs == xs || (k == x1 && rs == (x2:ks))) wss
            (r0ss,rss) = splitAt 1 pss
            (vss,zss) = splitAt n wss
args2Args1R (x1,x2) (t@([],n):ts) q = args2Args1R (x1,x2) ts q
args2Args1R _ _ q = q

args2Args3'1R
  :: FirstChars -- ^ A pair of the first characters of the starting group delimiter (the same for all 'String's in the all 'CLSpecifications') and the probable its modification (the first character of the last delimiter).
  -> CLSpecifications
  -> (Args,Args,Args,[String])
  -> (Args,Args,Args,[String])
args2Args3'1R (x1,x2) (t@(xs@(k:ks),n):ts) q@(w1,w2,w3,xss@(js:jss))
  | n < 0 = case w0ss of
             [] -> args2Args3'1R (x1,x2) ts q
             w00ss -> case qss of
                       [] -> args2Args3'1R (x1,x2) ts q
                       q0ss -> args2Args3'1R (x1,x2) ts (w1,w2,C xs q0ss:w3,kss `mappend` rss)
  | n == 0 = case w0ss of
              [] -> args2Args3'1R (x1,x2) ts q
              w00ss -> args2Args3'1R (x1,x2) ts (A xs:w1,w2,w3,kss `mappend` wss)
  | otherwise = case w0ss of
                 [] -> args2Args3'1R (x1,x2) ts q
                 w00ss -> if length vss == n then args2Args3'1R (x1,x2) ts (w1,B n xs vss:w2,w3,kss `mappend` zss)
                          else args2Args3'1R (x1,x2) ts q
      where (kss,uss) = break (== xs) xss
            (w0ss,wss) = splitAt 1 uss
            (qss,pss) = break (\rs -> rs == xs || (k == x1 && rs == (x2:ks))) wss
            (r0ss,rss) = splitAt 1 pss
            (vss,zss) = splitAt n wss
args2Args3'1R (x1,x2) _ q = q

args2Args31R
  :: FirstChars -- ^ A pair of the first characters of the starting group delimiter (the same for all 'String's in the all 'CLSpecifications') and the probable its modification (the first character of the last delimiter).
  -> CLSpecifications
  -> [String]
  -> (Args,Args,Args,[String])
args2Args31R (x1,x2) xs yss = args2Args3'1R (x1,x2) xs ([],[],[],yss)
{-# INLINABLE args2Args31R #-}

----------------------------------------------------------------------

-- | This function can actually parse the command line arguments being the ['String'].
args2ArgsFilteredGR
  :: (Arguments -> Bool) -- ^ A predicate to check which 'Arguments' must be kept in the result.
  -> CLSpecifications
  -> (Args,[String])
  -> (Args,[String])
args2ArgsFilteredGR f ts r = (filter f . map b1Args2AArgs $ qs, yss)
  where (qs,yss) = args2ArgsR ts r
{-# INLINABLE args2ArgsFilteredGR #-}

-- | This function can actually parse the command line arguments being the ['String'].
args2ArgsFilteredG1R
  :: FirstChars -- ^ A pair of the first characters of the starting group delimiter (the same for all 'String's in the all 'CLSpecifications') and the probable its modification (the first character of the last delimiter).
  -> (Arguments -> Bool) -- ^ A predicate to check which 'Arguments' must be kept in the result.
  -> CLSpecifications
  -> (Args,[String])
  -> (Args,[String])
args2ArgsFilteredG1R (x1,x2) f ts r = (filter f . map b1Args2AArgs $ qs, yss)
  where (qs,yss) = args2Args1R (x1,x2) ts r
{-# INLINABLE args2ArgsFilteredG1R #-}

----------------------------------------------------------------------

takeCsR
  :: CLSpecifications
  -> [String]
  -> (Args,[String])
takeCsR xs yss = args2ArgsFilteredGR (\x -> notNullArguments x && isC x) xs ([],yss)
{-# INLINABLE takeCsR #-}

takeCs1R
  :: FirstChars -- ^ A pair of the first characters of the starting group delimiter (the same for all 'String's in the all 'CLSpecifications') and the probable its modification (the first character of the last delimiter).
  -> CLSpecifications
  -> [String]
  -> (Args,[String])
takeCs1R (x1,x2) xs yss = args2ArgsFilteredG1R (x1,x2) (\x -> notNullArguments x && isC x) xs ([],yss)
{-# INLINABLE takeCs1R #-}

takeBsR
  :: CLSpecifications
  -> [String]
  -> (Args,[String])
takeBsR xs yss = args2ArgsFilteredGR (\x -> notNullArguments x && isB x) xs ([],yss)
{-# INLINABLE takeBsR #-}

takeAsR
  :: CLSpecifications
  -> [String]
  -> (Args,[String])
takeAsR xs yss = args2ArgsFilteredGR (\x -> notNullArguments x && isA x) xs ([],yss)
{-# INLINABLE takeAsR #-}

