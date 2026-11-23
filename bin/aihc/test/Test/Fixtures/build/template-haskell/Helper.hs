{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A module of the program that the splices of "Main" use.
module Helper (Describe (..), Size (..), deriveDescribe, deriveLabel, fraction, helperValue, isMode, labelText, pathVariable, plusHelper, twiceHelper, typedOffset, typedSteps, warmType) where

import Data.Proxy (Proxy)
import Data.Ratio ((%))
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)
import System.Environment (lookupEnv)
import Lower (Mode, lowerValue)
import Tone (Label (..), Tone (..))

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

-- | A record type of a different module than the quotes that use it.
data Size = Size {width :: Int, height :: Int}

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

-- | A typed quote with sections, a variable of the function, and a typed
-- splice of a different typed quote.
typedSteps :: Int -> Code Q (Int -> Int)
typedSteps 0 = [||id||]
typedSteps k = [||(`subtract` k) . (* 2) . $$(typedSteps (k - 1))||]

-- | A typed quote with a typed splice of a value that has no signature.
-- Only the type of the quote fixes the monad of that value.
typedOffset :: Int -> Code Q Int
typedOffset k = [||(1 +) $$(unsafeCodeCoerce offset)||]
  where
    offset = litE (integerL (fromIntegral k))

-- | The value of the environment variable PATH in a splice. A library
-- can look for a program in PATH, and the variable is always in the
-- environment of a process. The splice cannot start a program, thus the
-- value is empty.
pathVariable :: Q Exp
pathVariable = runIO (maybe "no PATH" show <$> lookupEnv "PATH") >>= lift

-- | Whether a name is the name of 'Mode'. The name quote of this module
-- and the name quote of the caller must give the same name.
isMode :: Name -> Q Exp
isMode name = if name == ''Mode then [|True|] else [|False|]

-- | A type with a promoted constructor of "Tone". The module of the splice
-- does not import "Tone".
warmType :: Q Type
warmType = [t|Proxy 'Warm|]

-- | An instance of the class of "Tone" for a data type. The module of the
-- splice does not import "Tone".
deriveLabel :: Name -> Q [Dec]
deriveLabel name =
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''Label) (ConT name))
        [FunD 'label [Clause [WildP] (NormalB (LitE (StringL (nameBase name)))) []]]
    ]

labelText :: (Label a) => a -> String
labelText = label

-- | A quasi-quoter for the four positions of a quasi-quote. The text of an
-- expression or a pattern is a fraction, such as @1/4@. The text of a
-- declaration is a name and a fraction. Each type is 'Double'.
fraction :: QuasiQuoter
fraction =
  QuasiQuoter
    { quoteExp = litE . rationalL . parseFraction,
      quotePat = litP . rationalL . parseFraction,
      quoteType = const [t|Double|],
      quoteDec = \text -> case words text of
        [name, value] ->
          sequence
            [ sigD (mkName name) [t|Double|],
              valD (varP (mkName name)) (normalB (litE (rationalL (parseFraction value)))) []
            ]
        _ -> fail "fraction: a declaration needs a name and a fraction"
    }
  where
    parseFraction text =
      case break (== '/') text of
        (top, '/' : bottom) -> read top % read bottom
        _ -> fromInteger (read text)
