module Graphics.X11.Xauth
    (Xauth(..), familyLocal, familyWild, familyNetname,
    familyKrb5Principal, familyLocalHost, getAuthByAddr) where

#include <X11/Xauth.h>

import Foreign
import Foreign.C
import Control.Monad (liftM2, zipWithM_)

data Xauth = Xauth { xauthName, xauthData :: [CChar] } deriving (Show, Read)

familyLocal, familyWild, familyNetname, familyKrb5Principal, familyLocalHost :: CUShort
familyLocal         = #{const FamilyLocal}
familyWild          = #{const FamilyWild}
familyNetname       = #{const FamilyNetname}
familyKrb5Principal = #{const FamilyKrb5Principal}
familyLocalHost     = #{const FamilyLocalHost}

foreign import ccall "X11/Xauth.h XauGetAuthByAddr"
    xauGetAuthByAddr :: CUShort -> CUShort -> Ptr CChar -> CUShort -> Ptr CChar
                         -> CInt -> Ptr CChar -> IO (Ptr Xauth)

foreign import ccall "X11/Xauth.h XauDisposeAuth"
    xauDisposeAuth :: Ptr Xauth -> IO ()

getAuthByAddr :: CUShort -> [CChar] -> [CChar] -> [CChar] -> IO (Maybe Xauth)
getAuthByAddr family address number atype
 = withArray address $ \addr_p -> withArray number $ \num_p ->
   withArray atype $ \atype_p -> do
        res <- xauGetAuthByAddr family (slength address) addr_p (slength number)
                             num_p (slength atype) atype_p
        if res == nullPtr
            then return Nothing
            else do
                name_p   <- #{peek Xauth, name}        res
                name_len <- #{peek Xauth, name_length} res :: IO CUShort
                data_p   <- #{peek Xauth, data}        res
                data_len <- #{peek Xauth, data_length} res :: IO CUShort
                x <- if or [nullPtr == name_p, nullPtr == data_p, data_len <= 0, name_len <= 0]
                    then return $ Nothing
                    else
                        liftM2 ((Just .) . Xauth)
                            (peekArray (fromIntegral name_len) name_p)
                            (peekArray (fromIntegral data_len) data_p)
                xauDisposeAuth res
                return x
 where slength x = fromIntegral $ length x
