module Demo where

import GHC.Prim (Int32#)
import Generated ()

#include <Provider.h>
#include "ProviderConfig.h"
#if PROVIDER_STATIC != 17 || PROVIDER_CONFIGURED != 23
#error dependency header values are incorrect
#endif

data Token = Token
data Int32 = I32# Int32#
foreign import capi unsafe "Provider.h value PROVIDER_STATIC" answer :: Int32
