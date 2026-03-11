module Text.Format.Error
  ( ArgError(..)
  , isArgKeyError
  , catchArgError
  , vferror
  , errorNoParse
  , errorCloseTag
  , errorArgKey
  , errorArgFmt
  ) where


import           Control.Exception


-- | A data type indicates an arg error
data ArgError = ArgKeyError -- ^ Can not find argument for the given key
              | ArgFmtError -- ^ The field format descriptor
                            -- does not match the argument
              deriving (Show, Eq)

instance Exception ArgError

isArgKeyError :: SomeException -> Bool
isArgKeyError = maybe False (== ArgKeyError) . fromException

catchArgError :: a -> a -> SomeException -> a
catchArgError key fmt e = maybe (throw e) handle (fromException e)
  where
    handle ArgKeyError = key
    handle ArgFmtError = fmt


--------------------------------------------------------------------------------
-- | Raises an error with a vformat-specific prefix on the message string.
vferror :: String -> a
vferror = errorWithoutStackTrace . ("vformat: " ++)


--------------------------------------------------------------------------------
-- *** Exception: vformat: no parse "xxx"
errorNoParse :: String -> a
errorNoParse = vferror . ("no parse " ++) . show

-- *** Exception: vformat: "xxx" close tag '}' missing
errorCloseTag :: String -> a
errorCloseTag = vferror . (++ " close tag '}' missing") . show

-- | Calls 'vferror' to indicate an arg key error for a given type.
errorArgKey :: String -> a
errorArgKey = vferror . ("bad arg key " ++) . show

-- | Calls 'vferror' to indicate an arg format error for a given type.
errorArgFmt :: String -> a
errorArgFmt = vferror . ("bad arg format " ++) . show
