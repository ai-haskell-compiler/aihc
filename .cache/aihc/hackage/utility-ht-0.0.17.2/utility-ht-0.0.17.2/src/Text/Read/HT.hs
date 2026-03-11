module Text.Read.HT where

{-| Parse a string containing an infix operator. -}
{-# INLINE readsInfixPrec #-}
readsInfixPrec :: (Read a, Read b) =>
   String -> Int -> Int -> (a -> b -> c) -> ReadS c
readsInfixPrec opStr opPrec prec cons =
   readParen
     (prec >= opPrec)
     ((\s -> [(const . cons, s)]) .>
      readsPrec opPrec .>
      (filter ((opStr==).fst) . lex) .>
      readsPrec opPrec)

{-| Compose two parsers sequentially. -}
infixl 9 .>
(.>) :: ReadS (b -> c) -> ReadS b -> ReadS c
(.>) ra rb =
   concatMap (\(f,rest) -> map (\(b, rest') -> (f b, rest')) (rb rest)) . ra


readMany :: (Read a) => String -> [a]
readMany x =
   let contReadList []     = []
       contReadList (y:[]) = fst y : readMany (snd y)
       contReadList _      = error "readMany: ambiguous parses"
   in  contReadList (reads x)

maybeRead :: Read a => String -> Maybe a
maybeRead str =
   case reads str of
      [(x,"")] -> Just x
      _ -> Nothing
