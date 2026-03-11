module Foreign.C.Struct.TypeSynonyms where

import Foreign.Ptr
import Foreign.C.String

-- * PTR

type PtrVoid = Ptr ()
type PtrFloat = Ptr #{type float}
type PtrCString = Ptr CString

-- * LIST

type ListFloat = [#{type float}]
