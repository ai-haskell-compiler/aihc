{-# LANGUAGE TemplateHaskell #-}
module EasyLogger.Util
    ( liftLoc
    ) where

import           Language.Haskell.TH.Syntax as TH


liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(TH.lift a)
    $(TH.lift b)
    $(TH.lift c)
    ($(TH.lift d1), $(TH.lift d2))
    ($(TH.lift e1), $(TH.lift e2))
    |]


