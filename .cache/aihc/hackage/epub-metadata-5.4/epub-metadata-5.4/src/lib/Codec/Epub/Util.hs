{- | Utility functions shared by modules that need to read the
   contents of XML documents.
-}
module Codec.Epub.Util
   where

import Text.Regex ( mkRegexWithOpts, subRegex )


{- | An evil hack to remove *ILLEGAL* characters before the XML
   declaration. Why do people write software that does this?
   Can't they follow directions?
-}
removeIllegalStartChars :: String -> String
removeIllegalStartChars = dropWhile (/= '<')


-- | An evil hack to remove encoding from the document
removeEncoding :: String -> String
removeEncoding = flip (subRegex 
   (mkRegexWithOpts " +encoding=\"UTF-8\"" False False)) ""


-- | An evil hack to remove any \<!DOCTYPE ...\> from the document
removeDoctype :: String -> String
removeDoctype = flip (subRegex 
   (mkRegexWithOpts "<!DOCTYPE [^>]*>" False True)) ""
