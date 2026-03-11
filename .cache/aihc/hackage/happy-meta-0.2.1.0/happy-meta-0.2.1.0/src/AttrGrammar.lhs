> module AttrGrammar 
> ( AgToken (..)
> , AgRule (..)
> , agLexAll
> , agLexer
> , subRefVal
> , selfRefVal
> , rightRefVal
> ) where


> import Data.Char
> import ParseMonad


> data AgToken 
>   = AgTok_LBrace
>   | AgTok_RBrace
>   | AgTok_Where
>   | AgTok_Semicolon
>   | AgTok_Eq
>   | AgTok_SelfRef String
>   | AgTok_SubRef (Int, String)
>   | AgTok_RightmostRef String
>   | AgTok_Unknown String
>   | AgTok_EOF
>  deriving (Show,Eq,Ord)


> subRefVal :: AgToken -> (Int, String)
> subRefVal   (AgTok_SubRef x)       = x
> subRefVal   _ = error "subRefVal: Bad value"
> selfRefVal :: AgToken -> String
> selfRefVal  (AgTok_SelfRef x)      = x
> selfRefVal  _ = error "selfRefVal: Bad value"
> rightRefVal :: AgToken -> String
> rightRefVal (AgTok_RightmostRef x) = x
> rightRefVal _ = error "rightRefVal: Bad value"


> data AgRule
>   = SelfAssign String [AgToken]
>   | SubAssign (Int,String) [AgToken]
>   | RightmostAssign String [AgToken]
>   | Conditional [AgToken]
>  deriving (Show,Eq,Ord)














--       or $> (for the rightmost symbol) followed by an optional










--            $>
















> type Pfunc a = String -> Int -> ParseResult a


> agLexAll :: P [AgToken]
> agLexAll = P $ aux []
>  where aux toks [] _ = OkP (reverse toks)
>        aux toks s l  = agLexer' (\t -> aux (t:toks)) s l


> agLexer :: (AgToken -> P a) -> P a
> agLexer m = P $ agLexer' (\x -> runP (m x))


> agLexer' :: (AgToken -> Pfunc a) -> Pfunc a
> agLexer' cont []         = cont AgTok_EOF []
> agLexer' cont ('{':rest) = cont AgTok_LBrace rest
> agLexer' cont ('}':rest) = cont AgTok_RBrace rest
> agLexer' cont (';':rest) = cont AgTok_Semicolon rest
> agLexer' cont ('=':rest) = cont AgTok_Eq rest
> agLexer' cont ('w':'h':'e':'r':'e':rest) = cont AgTok_Where rest
> agLexer' cont ('$':'$':rest) = agLexAttribute cont (\a -> AgTok_SelfRef a) rest
> agLexer' cont ('$':'>':rest) = agLexAttribute cont (\a -> AgTok_RightmostRef a) rest
> agLexer' cont s@('$':rest) =
>     let (n,rest') = span isDigit rest
>     in if null n 
>           then agLexUnknown cont s
>	    else agLexAttribute cont (\a -> AgTok_SubRef (read n,a)) rest'
> agLexer' cont s@(c:rest)
>     | isSpace c = agLexer' cont (dropWhile isSpace rest)
>     | otherwise = agLexUnknown cont s


> agLexUnknown :: (AgToken -> Pfunc a) -> Pfunc a
> agLexUnknown cont s = let (u,rest) = aux [] s in cont (AgTok_Unknown u) rest
>   where aux t [] = (reverse t,[])
>         aux t ('$':c:cs)
>            | c /= '$' && not (isDigit c)  = aux ('$':t) (c:cs)
>            | otherwise                    = (reverse t,'$':c:cs)
>         aux t (c:cs)
>            | isSpace c || c `elem` "{};=" = (reverse t,c:cs)
>	     | otherwise                    = aux (c:t) cs


> agLexAttribute :: (AgToken -> Pfunc a) -> (String -> AgToken) -> Pfunc a
> agLexAttribute cont k ('.':x:xs) 
>	 | isLower x = let (ident,rest) = span (\c -> isAlphaNum c || c == '\'') xs in cont (k (x:ident)) rest
>	 | otherwise = \_ -> FailP "bad attribute identifier"
> agLexAttribute cont k rest = cont (k "") rest
