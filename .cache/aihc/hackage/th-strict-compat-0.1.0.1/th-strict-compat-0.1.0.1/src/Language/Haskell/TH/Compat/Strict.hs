module Language.Haskell.TH.Compat.Strict
  ( isStrict
  , notStrict
  , unpacked
  ) where


import           Language.Haskell.TH.Syntax


isStrict, notStrict, unpacked :: Strict
#if MIN_VERSION_template_haskell(2,11,0)
isStrict = Bang NoSourceUnpackedness SourceStrict
notStrict = Bang NoSourceUnpackedness NoSourceStrictness
unpacked = Bang SourceUnpack NoSourceStrictness
#else
isStrict = IsStrict
notStrict = NotStrict
unpacked = Unpacked
#endif
