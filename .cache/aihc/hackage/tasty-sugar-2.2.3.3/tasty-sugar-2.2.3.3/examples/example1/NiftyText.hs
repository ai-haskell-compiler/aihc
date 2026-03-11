-- | Fancy text processor, surely destined to replace pandoc some day!

module NiftyText ( processText ) where

import Data.List

processText :: String -> String -> String -> String
processText transform outfmt inp =
  let wls = words <$> lines inp
      xf = case transform of
             "passthru" -> id
             "upper" -> upper
             "lower" -> lower
      outf = case outfmt of
               "ascii" -> unwords
               "octets" -> octets
               "sizes" -> sizes
  in unlines $ map (outf . map xf) wls

upper w =
  let toUpper c = if c `elem` ['a'..'z']
                  then toEnum $ fromEnum c - fromEnum 'a' + fromEnum 'A'
                  else c
  in map toUpper w

lower w =
  let toLower c = if c `elem` ['A'..'Z']
                  then toEnum $ fromEnum c - fromEnum 'A' + fromEnum 'a'
                  else c
  in map toLower w

octets = intercalate "  " . map (intercalate " " . map (show . fromEnum))

sizes l = show (length l) <> ": " <> intercalate " " (map (show . length) l)
