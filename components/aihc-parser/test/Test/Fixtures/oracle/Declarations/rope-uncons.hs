{- ORACLE_TEST xfail Rope with Unicode quote -}
unconsRope :: Rope -> Maybe (Char, Rope)
unconsRope text =
    let x = unRope text
    in  case F.viewl x of
            F.EmptyL -> Nothing
            (F.:<) piece x' ->
                case S.uncons piece of
                    Nothing -> Nothing
                    Just (c, piece') -> Just (c, Rope ((F.<|) piece' x'))
