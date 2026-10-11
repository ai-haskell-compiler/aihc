-- | Encodings that carry a failure mode in their name. This module is
-- above 'GHC.IO.Encoding.Failure' and 'GHC.Internal.IO.Encoding.Codec',
-- because the failure module depends on the codec module.
module GHC.Internal.IO.Encoding.FailureMode
  ( failureModeEncoding,
  )
where

import GHC.Base (String, (++))
import GHC.IO.Encoding.Failure (CodingFailureMode (..), codingFailureModeSuffix)
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.Internal.IO.Encoding.Codec (nameOnlyEncoding)

-- | An encoding that is only its name and the suffix of a failure mode.
-- 'ErrorOnCodingFailure' has no suffix, thus its name is the base name
-- with no copy.
failureModeEncoding :: String -> CodingFailureMode -> TextEncoding
failureModeEncoding name ErrorOnCodingFailure = nameOnlyEncoding name
failureModeEncoding name failureMode = nameOnlyEncoding (name ++ codingFailureModeSuffix failureMode)
