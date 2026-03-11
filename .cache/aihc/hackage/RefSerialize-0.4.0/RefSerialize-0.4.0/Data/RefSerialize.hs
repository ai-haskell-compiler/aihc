{-# OPTIONS -XOverlappingInstances
            -XTypeSynonymInstances
            -XFlexibleInstances
            -XUndecidableInstances
            -XOverloadedStrings
            -XIncoherentInstances
              #-}

-----------------------------------------------------------------------------
--
-- Module      :  Data.RefSerialize
-- Copyright   :  Alberto Gómez Corona
-- License     :  see LICENSE
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental


{- | Read, Show and Data.Binary do not check for repeated references to the same address.
     As a result, the data is duplicated when serialized. This is a waste of space in the filesystem
     and  also a waste of serialization time. but the worst consequence is that, when the serialized data is read,
     it allocates multiple copies for the same object when referenced multiple times. Because multiple referenced
     data is very typical in a pure language such is Haskell, this means that the resulting data loose the beatiful
     economy of space and processing time that referential transparency permits.

     This package leverages Show, Read and Data.Binary instances while it permits textual as well as binary serialization
      keeping internal  references.

     NOTE: to avoid long lists of variables with only one reference,
     now variables not referenced two or more times are inlined
     so rshowp serializes the same result than showp in these cases.
     However, showp is faster.
     In correspondence, rreadp call readp when there is no variable serialized.



     This is an example of a showp parser for a simple data structure.

>     data S= S Int Int deriving ( Show, Eq)
>
>     instance  Serialize S  where
>        showp (S x y)= do
>                        insertString "S"
>                        rshowp x       -- rshowp parsers can be inside showp parser
>                        rshowp y
>
>
>        readp =  do
>                        symbol "S"     -- I included a (almost) complete Parsec for deserialization
>                        x <- rreadp
>                        y <- rreadp
>                        return $ S x y

     there is a mix between referencing and no referencing parser here:

>    Data.RefSerialize>putStrLn $ runW $ showp $ S x x
>    S  v23 v23 where {v23= 5; }

-}





module Data.RefSerialize
(
     module Data.RefSerialize.Parser
    ,Serialize(
        showp
       ,readp
     )
    ,rshowp
    ,rreadp
    ,showps
    ,rshowps
    ,runR
    ,runW
    ,showpText
    ,readpText

    ,showpBinary
    ,readpBinary

    ,insertString
    ,insertChar
    ,rShow
    ,rRead
    ,insertVar
    ,addrHash
    ,readVar

    ,takep
    ,readHexp
    ,showHexp
-- * Context handling
    ,Context
    ,getRContext
    ,getWContext
    ,newContext
    ,showContext
    ,runRC
    ,runWC
)

 where

import Data.RefSerialize.Serialize
import Data.RefSerialize.Parser
import Unsafe.Coerce
import Data.Char(isAlpha, isSpace, isAlphaNum)
import Numeric(readHex,showHex)
import Data.ByteString.Lazy.Char8 as B
import Debug.Trace
import Data.Binary
import System.IO.Unsafe
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe

--import Debug.Trace
--(!>) = flip  trace

newContext :: IO Context
newContext  = Data.RefSerialize.Serialize.empty

class Serialize c where
   showp :: c -> STW ()     -- ^ shows the content of a expression, must be  defined bu the user
   readp ::  STR c          -- ^ read the content of a expression, must be user defined

-- | insert a reference (a variable in the where section).

-- @rshowp  = insertVar  showp @
rshowp :: Serialize c => c -> STW ()
rshowp  = insertVar  showp

 --  | read a variable in the where section (to use for deserializing rshowp output).

 --   @rreadp  = readVar  readp@
rreadp ::  Serialize c => STR c
rreadp = readVar  readp

{-
#ifdef Axioms

   serializeAxioms: Axioms c
   serializeAxioms= axioms{
         unary=   [Axiom "reverse"
                            (\x ->  let str= rShow x
                                        y = rRead xtr
                                    in  y== x)

                   AxioM "pointer equality"
                           (\x ->   let str= rShow[x,x]
                                        [y,z] = rRead str
                                    in varName y== varName z)
                  ]
        }
#endif
-}



-- | return the serialized list of variable values
-- useful for delayed deserialzation of expresions, in case of dynamic variables were deserialization
-- is done when needed, once the type is known with `runRC`

getRContext :: STR (Context, ByteString)
getRContext = STR(\(StatR(c,s,v)) -> Right (StatR (c,s,v), (c,v)))

getWContext :: STW (Context, ByteString)
getWContext = STW(\(StatW(c,s,v)) ->  (StatW (c,s,v), (c,"")))


-- | use the rshowp parser to serialize the object
-- @ rShow c= runW  $  rshowp c@
rShow :: Serialize c => c -> ByteString
rShow c= runW  $  showp c

-- | deserialize  trough the rreadp parser
-- @ rRead str= runR rreadp $ str@
rRead :: Serialize c => ByteString -> c
rRead str= runR readp $ str

readHexp :: (Num a, Integral a) => STR a
readHexp = STR(\(StatR(c,s,v)) ->
   let us= unpack s
       l=  readHex  us
   in if Prelude.null l then Left . Error $  "readHexp: not readable: " ++ us
         else let ((x,str2):_)= l
              in Right(StatR(c, pack $ Prelude.dropWhile isSpace str2,v),x) )
   <?> "readHexp "



showHexp :: (Num a,Integral a,Show a) => a -> STW ()
showHexp var= STW(\(StatW(c,s,v)) ->  (StatW(c, mappend s [Expr (pack $ showHex var "")],v),()))

-- |if a is an instance of Show, showpText can be used as the showp method
-- the drawback is that the data inside is not inspected for common references
-- so it is recommended to create your own readp method for your complex data structures
showpText :: Show a => a -> STW ()
showpText var= STW(\(StatW(c,s,v)) ->  (StatW(c, s `mappend` [Expr $ snoc (pack $ show var) ' '] ,v),()))

-- |if a is an instance of Read, readpText can be used as the readp method
-- the drawback is that the data inside is not inspected for common references
-- so it is recommended to create your own readp method for your complex data structures
readpText :: Read a => STR a
readpText = STR(\(StatR(c,s,v)) ->
   let us= unpack s
       l=  readsPrec 1 us
   in if Prelude.null l then Left . Error $  "not readable: " ++ us
         else let ((x,str2):_)= l
              in Right(StatR(c, pack $ Prelude.dropWhile isSpace str2,v),x) )
   <?> "readpText: readsPrec "



-- |  deserialize the string with the parser
runR:: STR a -> ByteString ->  a
runR p str=unsafePerformIO $ do
    c <- newContext
    let (struct, vars)= readContext whereSep str
    return $ runRC (c, vars) p struct

-- | read an expression with the variables definedd in a context passed as parameter.
runRC :: (Context, ByteString) -> STR a -> ByteString ->  a
runRC (c,vars) (STR f) struct=
  case   f (StatR(c,struct,vars) ) of
      Right (StatR _, a) -> a
      Left (Error s) -> error s

whereSep= "\r\nwhere{\r\n "


-- |   serialize x with the parser
runW :: STW () -> ByteString
runW  f = unsafePerformIO $ do
      c  <- newContext
      return $ runWC (c,"") f `append` showContext c True

-- | serialize x witn a given context and the parser
runWC :: (Context, ByteString) -> STW () -> ByteString
runWC (c,vars) (STW f) =
      let
          (StatW(c',str,_), _) = f (StatW(c,[],vars))
      in  showExpr  str  c'



--  let STW f= insertVar (const $ return ()) x >> return (varName x)
--       (StatW(c',str,_), v) = f (StatW(c,[],""))
--   in v

-- | serialize the variables. if the Bool flag is true, it prepend the text with the string "where"
showContext :: Context -> Bool -> ByteString
showContext c False=
     let  scontext= assocs c
     in   B.concat $ Prelude.map (\(n,(_,_,v,_))->"v" `append`  (pack $ show n)  `append`  "= "  `append`  showExpr v c  `append`  ";\r\n ")  scontext

showContext c True=
          let vars= showContext c False
          in if B.null vars  then "" else  whereSep `append` vars  `append`  "\r\n}"

showExpr :: [ShowF] -> Context -> ByteString
showExpr [] _ = B.empty
showExpr (Expr s:xs) c = s `mappend`  (cons ' ' $ showExpr  xs c)
showExpr ex@(Var v:xs) c=
   case Data.RefSerialize.Serialize.lookup  v  c  of
           Nothing -> error $ "showp: not found first variable in "++ show ex
           Just (_,_,exp,1)  -> delete v c `seq` showExpr exp c `mappend` (cons ' ' $ showExpr xs c)
           Just (_,_,exp,n)  ->  pack ('v':show v)  `mappend` (cons ' ' $ showExpr xs c)



 -- Prelude.concat $ runRC (context,"") proc exp
--     where
--     proc = many $ choice[stringLiteral, isvar, other]
--     other= manyTill anyChar whiteSpace
--     isvar= do
--        char 'v'
--        n <- integer
--        let var= 'v': show n
--        mvar <- findVar n
--        case mvar of
--          Nothing  -> return var
--          Just varExpr -> return $ showExpr varExpr context
--
--     findVar x = ST $ \(Stat(c,s,v1))->
--       case Data.RefSerialize.Serialize.lookup  x  c  of
--         Nothing -> Right(Stat(c,s,v1), Nothing)
--         Just (_,_,v,n)  ->
--           if n==1 then Right(Stat(delete c x,s,v1), Just v)
--                   else Right(Stat(c,s,v1), Nothing)



-- | return the  serialization instead of updating the writer
showps :: Serialize a =>  a -> STW ByteString
showps x= STW(\(StatW(c,s,v))->
 let
    STW f= showp x
    (StatW (c',str,_), _) = f (StatW(c,[],v))

 in (StatW(c',s ,v), showExpr str c'))

-- | return the variable name of the serialized data, which is put in the context
-- and does not update the writer
rshowps x= STW(\(StatW(c,s,v))->
 let
    STW f= rshowp x
    (StatW (c',str,_), _) = f (StatW(c,[],v))

 in (StatW(c',s ,v), showExpr str c'))

-- | insert a variable at this position. The expression value is inserted in the "where" section if it is not already
-- created. If the address of this object being parsed correspond with an address already parsed and
-- it is in the where section, then the same variable name is used
--   @runW showp (1::Int)                                -> "1"
--   runW (insertVar showp) (1::Int)                ->  v1 where { v1=1}
--   runW (insertVar showp) [(1::Int) ,1]        -> [v1.v1] where { v1=1}@
--   This is useful when the object is referenced many times

insertVar :: (a -> STW ()) -> a -> STW ()
insertVar parser x= STW(\(StatW(c,s,v))->
 let mf = x `seq`findVar x c in
 case mf of
   True ->  (StatW(c,s `mappend` [Var hash],v),())
   False ->
         let
            STW f= parser x
            (StatW (c',str,_), _) = f  (StatW(c,[],v))

         in (StatW(addc str c',s `mappend` [Var hash] ,v), ()))
 where
  addc str c=  insert ( hash) (st,unsafeCoerce x,  str,1) c

  (hash,st) = hasht x

  findVar x c=
         case  Data.RefSerialize.Serialize.lookup  hash  c  of
           Nothing -> False
           Just (x,y,z,n)  ->  insert hash (x,y,z,n+1) c  `seq`  True


-- | inform if the expression iwas already referenced and return @Right varname@
--  otherwise, add the expresion to the context and giive it a name and return  @Left varname@
-- The varname is not added to the serialized expression. The user must serialize it
-- This is usefu for expressions that admit different syntax depending or recursiviity, such are lists

isInVars :: (a -> STW ()) -> a -> STW (Either ByteString ByteString)
isInVars parser x= STW(\(StatW(c,s,v))->
 let mf = trytofindEntireObject x c in
 case mf of
   Just  var ->  (StatW(c,s,v),Right var)
   Nothing ->
         let
            STW f= parser x
            (StatW (c',str,_), _) = f  (StatW(c,[],v))

         in (StatW(addc str c',s ,v), Left varname))
 where
  addc str c= insert ( hash) (st,unsafeCoerce x,  str,1) c
  (hash,st) = hasht x
  varname=  pack$ "v" ++ show hash

  trytofindEntireObject x c=
         case Data.RefSerialize.Serialize.lookup  hash  c  of
           Nothing -> Nothing
           Just(x,y,z,n)  -> insert hash (x,y,z,n+1) c `seq` Just varname



-- | deserialize a variable serialized with insertVar. Memory references are restored
readVar :: Serialize c => STR c -> STR c
readVar (STR f)=  STR(\stat@(StatR(c,s,v))->
     let
       s1= B.dropWhile isSpace s
       (var, str2) = B.span isAlphaNum s1
       str3= B.dropWhile isSpace str2
       mnvar= numVar $ unpack var
       nvar= fromJust mnvar

     in  if isNothing mnvar then f stat
         else
          case  trytofindEntireObject nvar c of

           Just  (_,x,_,_) ->  Right(StatR(c,str3,v),unsafeCoerce x)
           Nothing ->
            let
               (_, rest)= readContext (var `append` "= ") v

            in if B.null rest then Left (Error ( "RedSerialize: readVar: " ++ unpack var ++ "value not found" ))
               else  case f  (StatR(c,rest,v)) of

                 Right (StatR(c',s',v'),x) ->
                   let c''= insert nvar ( undefined, unsafeCoerce x,  [],0) c'
                   in  Right (StatR(c'', str3,v),x)

                 err -> err)
  where
  trytofindEntireObject x c=
         case Data.RefSerialize.Serialize.lookup   x  c  of
           Nothing -> Nothing
           justx   -> justx


-- |  Write a String in the serialized output with an added whitespace. Deserializable with `symbol`
insertString :: ByteString -> STW ()
insertString s1= STW(\(StatW(c,s,v)) ->  (StatW(c, s  `mappend` [ Expr  s1 ],v),()))

-- | Write a char in the serialized output (no spaces)
insertChar :: Char -> STW()
insertChar car= STW(\(StatW(c, s,v)) -> (StatW(c, s `mappend` [Expr $ pack [car]],v),()))

--

-- -------------Instances


instance  Serialize a => Serialize [a] where
   showp []= insertString "[]"
   showp (x:xs)= do
           insertChar '['
           rshowp x
           mapM f xs
           insertString "]"
           where
           f :: Serialize a => a -> STW ()
           f x= do
              insertChar ','
              rshowp x


   readp = (brackets . commaSep $ rreadp)   <?> "readp:: [] "

{-
instance Serialize a => Serialize [a] where
 showp xs= showpl [] xs
   where
   showpl res []= bracketdisp $ Prelude.reverse res
   showpl res xs= do
        is <- isInVars showp xs
        case is of
            Right v -> parensdisp  (Prelude.reverse res) v
            Left  v -> showpl (v:res) xs

   parensdisp xs t= do
           insertChar '('
           disp ':' xs
           insertChar ':'
           insertString t
           insertString ")"

   bracketdisp []= insertString "[]"
   bracketdisp xs= do
           insertChar '['
           disp ',' xs
           insertString "]"

   disp sep (x:xs)= do
           insertString x
           mapM f xs
           where

           f x= do
              insertChar sep
              insertString x

 readp= choice [bracketsscan, parensscan]  <?> "readp:: [] "
   where
   bracketsscan= (brackets . commaSep $ rreadp)   <?> "readp:: [] "
   parensscan=parens $ do
       xs <- many(rreadp >>= \x -> symbol ":" >> return x)
       end <- rreadp
       return $ xs ++ end


-}


instance Serialize String where
    showp = showpText
    readp = readpText


instance (Serialize a, Serialize b) => Serialize (a, b) where
    showp (x, y)= do
            insertString  "("
            rshowp x
            insertString ","
            rshowp y
            insertString ")"


    readp =  parens (do
            x <- rreadp
            comma
            y <- rreadp
            return (x,y))
            <?> "rreadp:: (,) "

instance (Serialize a, Serialize b, Serialize c) => Serialize (a, b,c) where
    showp (x, y, z)= do
            insertString "("
            rshowp x
            insertString ","
            rshowp y
            insertString ","
            rshowp z
            insertString ")"


    readp =  parens (do
            x <- rreadp
            comma
            y <- rreadp
            comma
            z <- rreadp
            return (x,y,z))
            <?> "rreadp:: (,,) "

instance (Serialize a, Serialize b, Serialize c, Serialize d) => Serialize (a, b,c, d) where
    showp (x, y, z, t)= do
            insertString "("
            rshowp x
            insertString ","
            rshowp y
            insertString ","
            rshowp z
            insertString ","
            rshowp t
            insertString ")"


    readp =  parens (do
            x <- rreadp
            comma
            y <- rreadp
            comma
            z <- rreadp
            comma
            t <- rreadp
            return (x,y,z,t))
            <?> "rreadp:: (,,,) "

instance (Serialize a, Ord a, Serialize b) => Serialize (M.Map a b) where
    showp m= showp $ M.toList m
    readp= do
           list <- readp  -- :: ST [(a,b)]
           return $ M.fromList list




instance Serialize a => Serialize (Maybe a) where
    showp Nothing = insertString "Nothing"
    showp (Just x) =do
          insertString "Just"
          showp x

    readp =  choice [rNothing, rJust] where
      rNothing = symbol "Nothing" >> return Nothing
      rJust =  do
         symbol "Just"
         x <- readp
         return $ Just x

instance (Serialize a, Serialize b) => Serialize (Either a b) where
    showp (Left x) = do
       insertString "Left"
       rshowp x

    showp (Right x) = do
       insertString "Right"
       rshowp x

    readp =  choice [rLeft, rRight] where
      rLeft = symbol "Left" >> rreadp >>= \x -> return $ Left x
      rRight = symbol "Right" >> rreadp >>= \x -> return $ Right x




-- binary serialization


binPrefix=   "Bin "
binPrefixSp= append (pack binPrefix) " "

-- | serialize a variable which has a Binary instance
showpBinary :: Binary a => a -> STW ()
showpBinary x = do
    let s = encode x
    let n = pack . show $ B.length s
    insertString $  binPrefixSp `append` n `append` " " `append` s

-- | deserialize a variable serialized by `showpBinary`
readpBinary :: Binary a => STR a
readpBinary = do
      symbol binPrefix
      n     <- integer
      str   <- takep (fromIntegral n)
      let x = decode str
      return x

-- return n chars form the serialized data
--takep :: Int -> STR ByteString
--takep n=   take1 "" n
--  where
--  take1 s 0= return  s
--  take1 s n=  anyChar >>= \x -> take1 (snoc s x ) (n-1)

takep n=STR(\(StatR(c,s,v)) ->
   let (x,r)= B.splitAt n s
   in Right(StatR(c, r,v),x) )
   <?> "takep "

-- | default instances

--instance (Show a, Read a )=> Serialize a where
--  showp= showpText
--  readp= readpText



