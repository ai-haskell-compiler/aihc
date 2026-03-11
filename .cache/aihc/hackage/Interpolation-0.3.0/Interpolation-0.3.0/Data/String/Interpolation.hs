{-# LANGUAGE CPP,QuasiQuotes,TemplateHaskell,DeriveDataTypeable,PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-} 

-- | This module defines a quasiquoter for interpolated strings. For example:
--
--  @
--  import qualified Data.Text.Lazy as LT
--  let fb x | x \`mod\` 15 == 0 = \"FizzBuzz\" 
--           | x \`mod\` 5 == 0 = \"Buzz\" 
--           | x \`mod\` 3 == 0 = \"Fizz\" 
--           | otherwise = LT.pack (show x)
-- @
--
-- >>> LT.take 85 [str|#x in [1..]:$fb x$|, #|] <> ".. "
-- "1, 2, Fizz, 4, Buzz, Fizz, 7, 8, Fizz, Buzz, 11, Fizz, 13, 14, FizzBuzz, 16, 17, Fizz.. "
--  
--
module Data.String.Interpolation(str,prnt,endline,tab) where
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta
import Data.Data
import Data.Maybe
import Data.Monoid
import Data.Char
import Data.String
-- import Data.List(intercalate)



quoteExprExp :: String -> TH.ExpQ
-- | Quasiquote 'str' implements multiline strings with interpolation.
--   Interpolating a value into the string is done by 
--   $\<String expression\>$
--   and interpolating anything with instance Show is 
--   $:\<Show expression\>$. Due to pretty deep limitations, the parser
--   is not able to properly deduce associtivity of infix operators,
--   so use lots and lots of parenthesis.
--
--   Repetitive patterns can be made with # symbol: 
--
--  @
--  \#\<pattern\> in \<list\>: \<interpolated string\> (|\<interpolated string\>)\#
--  @
--   
--   Where (|\<interpolated string\>) denotes optional separator for the 
--   elements.
--   
--   Multiline indentation is handled by aligning on smallest un-empty
--   line after the first. Neither pattern matching nor nested #-patterns
--   are supported, however, see example below.
--
--   Normal '\\n' style escaping of special characters
--   is intentionally not supported. Please use $endline$ or $"\n"$ 
--   style instead. 
--
--   As an example, let's plot set of vectors with gnuplot:
--    
--   @
--   plotVecs :: [(String,[Double])] -> String
--   plotVecs vs =  
--       [$str| \#\# Plot multiple vectors
--              plot \#(n,_) in vs:'-' with lines lw 5 title $:n$ |, \#
--              \#d in map snd vs:$singleVec d$$endline$e$endline$\# |]
--   where
--    singleVec n = [$str|\#(e,i) in zip n [1..]: $:i$ $:e$|$endline$\#|]
--   @
--   
--   @
--  *Gnuplotter> plotVecs [(\"A\",[1..5]),(\"B\",[2..6])]
--  \# Plot multiple vectors
--   plot '-' with lines lw 5 title \"A\" , '-' with lines lw 5 title \"B\" 
--    1 1.0
--    2 2.0
--    3 3.0
--    4 4.0
--    5 5.0
--   
--   e
--    1 2.0
--    2 3.0
--    3 4.0
--    4 5.0
--    5 6.0
--   
--   e
--   @
--

-- | Quasiquoter for interpolating strings. Produces values of `(Monoid m, IsString m) => m`
str  :: QuasiQuoter
str  = QuasiQuoter {quoteExp = quoteExprExp}

-- | Quasiquoter for printing an interpolated String
prnt  :: QuasiQuoter
prnt  = QuasiQuoter {quoteExp = quoteExprPExp}
-- ** Predefined strings

-- | End of the line  
endline :: IsString a => a
endline = fromString "\n"

-- | Tab
tab :: IsString a => a
tab = fromString "\t"


--
quoteExprExp s = psToStringE (parParse $ norming s)

quoteExprPExp :: String -> ExpQ
quoteExprPExp = TH.appE [|putStr|] . quoteExprExp  

--

psToStringE :: PieceString -> TH.Q TH.Exp
psToStringE [] = [|mempty|] --TH.stringE ""
psToStringE (x:xs) =  ([| mappend |]) `TH.appE`
                      (sbitToExp x)   `TH.appE`   
                      (psToStringE xs)

runEither :: (Monad m) => [Char] -> Either [Char] t -> m t
runEither _ (Right x) = return x
runEither s (Left e)  = error $ s ++" : "++ e

appM :: (Monad m) => String -> m Exp
appM expr = runEither ("Parse error in antiquote <"++expr++">") (parseExp expr)

sbitToExp :: StringBits -> ExpQ
sbitToExp (Str x) =  [| fromString |] `appE` TH.stringE x 
sbitToExp (Var x)  =  appM x 
sbitToExp (SVar x) = TH.appE  [| fromString . show |]--(TH.varE (TH.mkName "show"))
                        (sbitToExp (Var x))

sbitToExp (RepVar varName lstName rep sep) 
    = [|intercalate $(sepr) (map $(lam) $(lstN))|]
    where
     sepr = psToStringE (fromMaybe [Str ""] sep)
     lam  = TH.lamE [varN] (psToStringE rep)
     varN = runEither ("Parse error in repetition pattern <"++varName++">")
             (parsePat varName)--TH.varP $ TH.mkName varName
     lstN = runEither ("Parse error in repetition binding <"++lstName++">")
             (parseExp lstName) -- TH.varE $ TH.mkName lstName



type PieceString = [StringBits]
data StringBits = Str String | Var String | SVar String
                  | RepVar String String 
                    PieceString (Maybe PieceString)
                deriving (Eq,Ord,Show,Typeable,Data)

-- Split to string into pieces. This needs proper error messages.
-- Perhaps parsec?
parParse :: [Char] -> [StringBits]
parParse [] = []

parParse ('$':'$':s) = Str "$":parParse s -- Parse escapes
parParse ('#':'#':s) = Str "#":parParse s

parParse ('$':':':s) = SVar (takeWhile (/='$') s) -- Parse antiquotes
                        :parParse (drop 1 (dropWhile (/='$') s))

parParse ('$':s) = Var (takeWhile (/='$') s)
                    :parParse (drop 1 (dropWhile (/='$') s))

parParse ('#':s) = let (bind,exprS) = escapingBreak (':') 
                                       ("Repetition <"++s++"> missing body") 
                                        $ s
                       (hasSep,expr,sepS) = takeRep 
                                             ("Repetition <"++s++"> missing #")$ exprS
                       (sep,restS) = escapingBreak ('#')
                                       ("Repetition <"++s++"> missing #")
                                         $ sepS
                       rest = restS
                       (varNameS,listNameS) = break (=="in") . words
                                            $ bind
                       listName = unwords . drop 1 $ listNameS
                       varName  = unwords varNameS
                    in if hasSep 
                        then RepVar varName listName 
                             (parParse expr) (Just $ parParse sep) 
                             : parParse rest
                        else RepVar varName listName 
                             (parParse expr) (Nothing) 
                             : parParse sepS

parParse s = Str (takeWhile (notIn "$#") s)
                 : parParse (dropWhile (notIn "$#") s) 

notIn :: (Eq a) => [a] -> a -> Bool
notIn s x =not $ elem x s 

takeRep :: String -> String -> (Bool,String,String)
takeRep e x = takeRep' x []
 where
    takeRep' :: String -> String -> (Bool,String,String)
    takeRep' [] _ = error $ e
    takeRep' ('|':'|':s) acc = takeRep' s ('|':acc)
    takeRep' ('|':s) acc = (True,reverse acc,s)
    takeRep' ('#':s) acc = (False,reverse acc,s)
    takeRep' (r:s) acc = takeRep' s (r:acc)

escapingBreak :: (Eq t) => t -> [Char] -> [t] -> ([t], [t])
escapingBreak s e st = eBreak [] st 
  where 
    eBreak acc (a:b:c) | a==s&&b==s = eBreak (s:acc) c
                       | a==s = (reverse acc,b:c)
                       | otherwise = eBreak (a:acc) (b:c)
    eBreak acc (a:c) | a==s   = (reverse acc,c) 
                     | otherwise = eBreak (a:acc) c
    eBreak _ [] = error e

-- Normalize the indentation to match second line

norming :: String -> String
norming = intercalate "\n" . norming' . lines . dropWhile isSpace

norming':: [String] -> [String]
norming' [] = []
norming' (l:lst) = l:map (drop (n)) lst
    where 
     n = minimumD 0 $ map (length . takeWhile (==' ') ) 
         $ (filter (not.isEmpty) lst)
     isEmpty = all (isSpace)


minimumD :: (Ord a) => a -> [a] -> a
minimumD d [] = d
minimumD _ x  = minimum x


-- Re-implementation of intercalate and intersperse using monoid instead of lists

intersperse             :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs


prependToAll            :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

intercalate :: Monoid a => a -> [a] -> a
intercalate xs xss = mconcat (intersperse xs xss)


