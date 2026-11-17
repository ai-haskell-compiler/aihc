{-# LANGUAGE TemplateHaskell #-}

-- | A module of the program that the splices of "Main" use.
module Helper (Describe (..), deriveDescribe, helperValue, isMode, plusHelper, twiceHelper) where

import Language.Haskell.TH
import Lower (Mode, lowerValue)

helperValue :: Int
helperValue = lowerValue + 3

-- | A quote that names a value of this module.
plusHelper :: Int -> Q Exp
plusHelper n = [|helperValue + n|]

-- | A function that this module does not export. A quote of this module
-- names it, so the code from the quote in a different module calls it.
privateTwice :: Int -> Int
privateTwice n = n * 2

-- | A quote that names a function that this module does not export.
twiceHelper :: Q Exp
twiceHelper = [|privateTwice|]

class Describe a where
  describe :: a -> String

-- | An instance of 'Describe' for a data type. The instance head names the
-- class of this module and the type that the caller gives.
deriveDescribe :: Name -> Q [Dec]
deriveDescribe name = do
  TyConI (DataD _ _ _ _ constructors _) <- reify name
  let names = unwords [nameBase constructor | NormalC constructor _ <- constructors]
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''Describe) (ConT name))
        [FunD 'describe [Clause [WildP] (NormalB (LitE (StringL names))) []]]
    ]

-- | Whether a name is the name of 'Mode'. The name quote of this module
-- and the name quote of the caller must give the same name.
isMode :: Name -> Q Exp
isMode name = if name == ''Mode then [|True|] else [|False|]
