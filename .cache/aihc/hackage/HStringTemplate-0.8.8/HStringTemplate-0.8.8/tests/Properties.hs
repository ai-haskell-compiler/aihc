{-# OPTIONS -O2 -fglasgow-exts #-}

module Properties where
import Text.Printf
import Control.Monad
import Control.Arrow
import Control.Applicative hiding ((<|>),many)
import Data.Maybe
import Data.Monoid
import Data.List
import System.IO
import System.Random hiding (next)
import qualified Data.Map as M

import Text.StringTemplate
import Text.StringTemplate.Classes
import Text.StringTemplate.Base
import Test.QuickCheck
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    let n = if null args then 100 else read (head args)
    results <- mapM (\ (s, a) -> printf "%-25s: " s >> fmap (\x->(s,x)) a) tests
    mapM print results
    when (not $ all (isSuccess  . snd) results) $ fail "Not all tests passed!"
 where
    isSuccess (Success _ _ _ _ _ _) = True
    isSuccess _ = False
    tests =
        [("prop_paddedTrans" , mytest prop_paddedTrans),
         ("prop_constStr" , mytest prop_constStr),
         ("prop_emptyNulls" , mytest prop_emptyNulls),
         ("prop_fullNulls" , mytest prop_fullNulls),
         ("prop_substitution" , mytest prop_substitution),
         ("prop_separator" , mytest prop_separator),
         ("prop_attribs" , mytest prop_attribs),
         ("prop_comment" , mytest prop_comment),
         ("prop_ifelse" , mytest prop_ifelse),
         ("prop_simpleGroup" , mytest prop_simpleGroup)
        ]
mytest x = quickCheckResult x
{-----------------------------------------------------------------------
  Limited tests for now: just for list juggling and some basic parsing.
-----------------------------------------------------------------------}

prop_paddedTrans (x::[Int]) (y::[Int]) (z::[Int]) n =
   (length pt == length npt) &&
   all (3 ==) (map length pt) &&
   all (all (==n)) (zipWith unmerge (paddedTrans n pt) [x,y,z])
          where pt   = paddedTrans n [x,y,z]
                npt  = transpose [x,y,z]
                unmerge xl@(x:xs) (y:ys)
                    | x == y = unmerge xs ys
                    | otherwise = xl
                unmerge x y = x

prop_constStr (LitString x) = x == (toString . newSTMP $ x)

prop_emptyNulls (LitString x) (LitString y) i =
    (concat . replicate i' $ x) ==
      (toString . newSTMP . concat . replicate i' $ tmpl)
    where tmpl = x++"$"++y++"$"
          i' = min (abs i) 10

prop_fullNulls (LitString x) (LitString y) i =
    length y > 0 ==>
               (concat . replicate i' $ x++y) ==
               (toString . newSTMP . concat . replicate i' $ tmpl)
    where tmpl = x++"$"++y++";null='"++y++"'$"
          i' = min (abs i) 10

prop_substitution (LitString x) (LitString y) (LitString z) i =
    length y > 0 ==>
               (concat . replicate i' $ x++z) ==
               (toString . setAttribute y z .
                newSTMP . concat . replicate i' $ tmpl)
    where tmpl = x++"$"++y++"$"
          i' = min (abs i) 10

prop_separator (LitString x) (LitString y) (LitString z) i =
    length x > 0 ==>
               (concat . intersperse z . replicate i' $ y) ==
               (toString . setAttribute x (replicate i' y)
                . newSTMP $ tmpl)
    where tmpl = "$"++x++";separator='"++z++"'$"
          i' = min (abs i) 10

prop_comment (LitString x) (LitString y) (LitString z) =
    toString (newSTMP (x ++ "$!" ++ y ++ "!$" ++ z)) == x ++ z

prop_attribs (LitString x) i =
    toString (setManyAttrib (replicate i' ("f",x)) $ newSTMP "$f$")
                 == (concat . replicate i' $ x)
        where
          i' = min (abs i) 10

prop_ifelse a b c d =
    toString (setManyAttrib alist . newSTMP $ "$if(a)$a$elseif(b)$b$elseif(c)$c$else$$if(d)$d$else$e$endif$$endif$") == (fst . head . filter snd) alist
        where alist = [("a",a),("b",b),("c",c),("d",d),("e",True)]

prop_simpleGroup (LitString x) (LitString y) (LitString z) (LitString t) =
    length x > 0 && length y > 0 && length z > 0 && length t > 0
               && length (nub [x,y,z,t]) == 4 ==>
                  x == (toString . fromJust . getStringTemplate x $ grp)
    where tm   = newSTMP x
          tm'  = newSTMP $ "$"++y++"()$"
          tmIt = newSTMP "$it$"
          tm'' = newSTMP $ "$"++z++"():"++t++"()$"
          grp = groupStringTemplates [(y,tm),(z,tm'),(t,tmIt),(x,tm'')]

newtype LitChar = LitChar {unLitChar :: Char} deriving Show
instance Arbitrary LitChar where
  arbitrary = LitChar <$> choose ('a','z')

newtype LitString = LitString String deriving Show
instance Arbitrary LitString where
  arbitrary = LitString . map unLitChar <$> sized (\n -> choose (0,n) >>= vector)
