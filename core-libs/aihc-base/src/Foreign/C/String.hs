module Foreign.C.String
  ( CString,
    CStringLen,
    CWString,
    CWStringLen,
  )
where

import Foreign.C.Types (CChar, CWchar)
import GHC.Int (Int)
import GHC.Ptr (Ptr)

type CString = Ptr CChar

type CStringLen = (Ptr CChar, Int)

type CWString = Ptr CWchar

type CWStringLen = (Ptr CWchar, Int)
