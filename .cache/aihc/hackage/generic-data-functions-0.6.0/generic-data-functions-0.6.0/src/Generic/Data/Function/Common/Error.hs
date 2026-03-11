-- | Runtime error messages for common generic data representation errors.

module Generic.Data.Function.Common.Error where

wrapE :: String -> String -> String
wrapE msgGot msgWhyBad =
       "Generic.Data.Function.Common.Error:\n"
    <> "Attempted to use an invalid generic instance: \n"
    <> "  got: "<>msgGot<>"\n"
    <> "  but: "<>msgWhyBad<>"\n"
    <> "If you like, you can catch such errors during compilation.\n"
    <> "See the generic-type-asserts package on Hackage."

eNoEmpty :: String
eNoEmpty = wrapE "empty data type" "disallowed"

eNoSum :: String
eNoSum = wrapE "sum data type" "cannot use non-sum generics"
