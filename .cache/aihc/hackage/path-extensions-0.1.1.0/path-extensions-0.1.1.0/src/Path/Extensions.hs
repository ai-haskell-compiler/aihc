{- |
   Module     : Path.Extensions
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : provisional

Enumeration of file extensions for use with the Path library.
-}

module Path.Extensions (
  -- * Raw Extension Strings
  cExtension
, cppExtension
, cssExtension
, docxExtension
, gifExtension
, hsExtension
, htmlExtension
, jpgExtension
, jsExtension
, mdExtension
, mp3Extension
, mp4Extension
, oExtension
, odtExtension
, pdfExtension
, phpExtension
, pngExtension
, pyExtension
, rtfExtension
, shExtension
, svgExtension
, texExtension
, txtExtension
, xmlExtension
, wikiExtension

  -- * Add Extensions
, addCExtension
, addCppExtension
, addCssExtension
, addDocxExtension
, addGifExtension
, addHsExtension
, addHtmlExtension
, addJpgExtension
, addJsExtension
, addMdExtension
, addMp3Extension
, addMp4Extension
, addOExtension
, addOdtExtension
, addPdfExtension
, addPhpExtension
, addPngExtension
, addPyExtension
, addRtfExtension
, addShExtension
, addSvgExtension
, addTexExtension
, addTxtExtension
, addXmlExtension
, addWikiExtension

  -- * Replace Extensions
, withCExtension
, withCppExtension
, withCssExtension
, withDocxExtension
, withGifExtension
, withHsExtension
, withHtmlExtension
, withJpgExtension
, withJsExtension
, withMdExtension
, withMp3Extension
, withMp4Extension
, withOExtension
, withOdtExtension
, withPdfExtension
, withPhpExtension
, withPngExtension
, withPyExtension
, withRtfExtension
, withShExtension
, withSvgExtension
, withTexExtension
, withTxtExtension
, withXmlExtension
, withWikiExtension
) where

import           Control.Monad.Catch
import           Path

-- | The string ".c".
cExtension :: String
cExtension = ".c"

-- | The string ".cpp".
cppExtension :: String
cppExtension = ".cpp"

-- | The string ".css".
cssExtension :: String
cssExtension  = ".css"

-- | The string ".docx"
docxExtension :: String
docxExtension = ".docx"

-- | The string ".gif".
gifExtension :: String
gifExtension  = ".gif"

-- | The string ".hs".
hsExtension :: String
hsExtension   = ".hs"

-- | The string ".html".
htmlExtension :: String
htmlExtension = ".html"

-- | The string ".jpg".
jpgExtension :: String
jpgExtension  = ".jpg"

-- | The string ".js".
jsExtension :: String
jsExtension   = ".js"

-- | The string ".md".
mdExtension :: String
mdExtension   = ".md"

-- | The string ".mp3".
mp3Extension :: String
mp3Extension  = ".mp3"

-- | The string ".mp4".
mp4Extension :: String
mp4Extension  = ".mp4"

-- | The string ".o".
oExtension :: String
oExtension    = ".o"

-- | The string ".odt"
odtExtension :: String
odtExtension = ".odt"

-- | The string ".pdf".
pdfExtension :: String
pdfExtension  = ".pdf"

-- | The string ".php".
phpExtension :: String
phpExtension  = ".php"

-- | The string ".png".
pngExtension :: String
pngExtension  = ".png"

-- | The string ".py".
pyExtension :: String
pyExtension = ".py"

-- | The string ".rtf".
rtfExtension :: String
rtfExtension = ".rtf"

-- | The string ".sh"
shExtension :: String
shExtension = ".sh"

-- | The string ".svg"
svgExtension :: String
svgExtension = ".svg"

-- | The string ".tex"
texExtension :: String
texExtension = ".tex"

-- | The string ".txt"
txtExtension :: String
txtExtension = ".txt"

-- | The string ".xml".
xmlExtension :: String
xmlExtension  = ".xml"

-- | The string ".wiki"
wikiExtension :: String
wikiExtension = ".wiki"

-- | Add a ".c" extension to the end of a `File`.
addCExtension :: MonadThrow m => Path b File -> m (Path b File)
addCExtension = addExtension cExtension

-- | Add a ".cpp" extension to the end of a `File`.
addCppExtension :: MonadThrow m => Path b File -> m (Path b File)
addCppExtension = addExtension cppExtension

-- | Add a ".css" extension to the end of a `File`.
addCssExtension :: MonadThrow m => Path b File -> m (Path b File)
addCssExtension = addExtension cssExtension

-- | Add a ".docx" extension to the end of a `File`.
addDocxExtension :: MonadThrow m => Path b File -> m (Path b File)
addDocxExtension = addExtension docxExtension

-- | Add a ".gif" extension to the end of a `File`.
addGifExtension :: MonadThrow m => Path b File -> m (Path b File)
addGifExtension = addExtension gifExtension

-- | Add a ".hs" extension to the end of a `File`.
addHsExtension :: MonadThrow m => Path b File -> m (Path b File)
addHsExtension  = addExtension hsExtension

-- | Add a ".html" extension to the end of a `File`.
addHtmlExtension :: MonadThrow m => Path b File -> m (Path b File)
addHtmlExtension = addExtension htmlExtension

-- | Add a ".js" extension to the end of a `File`.
addJsExtension :: MonadThrow m => Path b File -> m (Path b File)
addJsExtension = addExtension jsExtension

-- | Add a ".jpg" extension to the end of a `File`.
addJpgExtension :: MonadThrow m => Path b File -> m (Path b File)
addJpgExtension = addExtension jpgExtension

-- | Add a ".md" extension to the end of a `File`.
addMdExtension :: MonadThrow m => Path b File -> m (Path b File)
addMdExtension  = addExtension mdExtension

-- | Add a ".mp3" extension to the end of a `File`.
addMp3Extension :: MonadThrow m => Path b File -> m (Path b File)
addMp3Extension = addExtension mp3Extension

-- | Add a ".mp4" extension to the end of a `File`.
addMp4Extension :: MonadThrow m => Path b File -> m (Path b File)
addMp4Extension = addExtension mp4Extension

-- | Add a ".o" extension to the end of a `File`.
addOExtension :: MonadThrow m => Path b File -> m (Path b File)
addOExtension = addExtension oExtension

-- | Add a ".odt" extension to the end of a `File`.
addOdtExtension :: MonadThrow m => Path b File -> m (Path b File)
addOdtExtension = addExtension odtExtension

-- | Add a ".pdf" extension to the end of a `File`.
addPdfExtension :: MonadThrow m => Path b File -> m (Path b File)
addPdfExtension = addExtension pdfExtension

-- | Add a ".php" extension to the end of a `File`.
addPhpExtension :: MonadThrow m => Path b File -> m (Path b File)
addPhpExtension = addExtension phpExtension

-- | Add a ".png" extension to the end of a `File`.
addPngExtension :: MonadThrow m => Path b File -> m (Path b File)
addPngExtension = addExtension pngExtension

-- | Add a ".py" extension to the end of a `File`.
addPyExtension :: MonadThrow m => Path b File -> m (Path b File)
addPyExtension = addExtension pyExtension

-- | Add a ".rtf" extension to the end of a `File`.
addRtfExtension :: MonadThrow m => Path b File -> m (Path b File)
addRtfExtension = addExtension rtfExtension

-- | Add a ".sh" extension to the end of a `File`.
addShExtension :: MonadThrow m => Path b File -> m (Path b File)
addShExtension = addExtension shExtension

-- | Add a ".svg" extension to the end of a `File`.
addSvgExtension :: MonadThrow m => Path b File -> m (Path b File)
addSvgExtension = addExtension svgExtension

-- | Add a ".tex" extension to the end of a `File`.
addTexExtension :: MonadThrow m => Path b File -> m (Path b File)
addTexExtension = addExtension texExtension

-- | Add a ".txt" extension to the end of a `File`.
addTxtExtension :: MonadThrow m => Path b File -> m (Path b File)
addTxtExtension = addExtension txtExtension

-- | Add a ".xml" extension to the end of a `File`.
addXmlExtension :: MonadThrow m => Path b File -> m (Path b File)
addXmlExtension = addExtension xmlExtension

-- | Add a ".wiki" extension to the end of a `File`.
addWikiExtension :: MonadThrow m => Path b File -> m (Path b File)
addWikiExtension = addExtension wikiExtension

-- | Replace the current extension of a `File` with a ".c" extension.
withCExtension :: MonadThrow m => Path b File -> m (Path b File)
withCExtension = replaceExtension cExtension

-- | Replace the current extension of a `File` with a ".cpp" extension.
withCppExtension :: MonadThrow m => Path b File -> m (Path b File)
withCppExtension = replaceExtension cppExtension

-- | Replace the current extension of a `File` with a ".css" extension.
withCssExtension :: MonadThrow m => Path b File -> m (Path b File)
withCssExtension = replaceExtension cssExtension

-- | Replace the current extension of a `File` with a ".docx" extension.
withDocxExtension :: MonadThrow m => Path b File -> m (Path b File)
withDocxExtension = replaceExtension docxExtension

-- | Replace the current extension of a `File` with a ".gif" extension.
withGifExtension :: MonadThrow m => Path b File -> m (Path b File)
withGifExtension = replaceExtension gifExtension

-- | Replace the current extension of a `File` with a ".hs" extension.
withHsExtension :: MonadThrow m => Path b File -> m (Path b File)
withHsExtension = replaceExtension hsExtension

-- | Replace the current extension of a `File` with a ".html" extension.
withHtmlExtension :: MonadThrow m => Path b File -> m (Path b File)
withHtmlExtension = replaceExtension htmlExtension

-- | Replace the current extension of a `File` with a ".jpg" extension.
withJpgExtension :: MonadThrow m => Path b File -> m (Path b File)
withJpgExtension = replaceExtension jpgExtension

-- | Replace the current extension of a `File` with a ".js" extension.
withJsExtension :: MonadThrow m => Path b File -> m (Path b File)
withJsExtension = replaceExtension jsExtension

-- | Replace the current extension of a `File` with a ".md" extension.
withMdExtension :: MonadThrow m => Path b File -> m (Path b File)
withMdExtension = replaceExtension mdExtension

-- | Replace the current extension of a `File` with a ".mp3" extension.
withMp3Extension :: MonadThrow m => Path b File -> m (Path b File)
withMp3Extension = replaceExtension mp3Extension

-- | Replace the current extension of a `File` with a ".mp4" extension.
withMp4Extension :: MonadThrow m => Path b File -> m (Path b File)
withMp4Extension = replaceExtension mp4Extension

-- | Replace the current extension of a `File` with a ".md" extension.
withOExtension :: MonadThrow m => Path b File -> m (Path b File)
withOExtension = replaceExtension oExtension

-- | Replace the current extension of a `File` with a ".odt" extension.
withOdtExtension :: MonadThrow m => Path b File -> m (Path b File)
withOdtExtension = replaceExtension odtExtension

-- | Replace the current extension of a `File` with a ".pdf" extension.
withPdfExtension :: MonadThrow m => Path b File -> m (Path b File)
withPdfExtension = replaceExtension pdfExtension

-- | Replace the current extension of a `File` with a ".php" extension.
withPhpExtension :: MonadThrow m => Path b File -> m (Path b File)
withPhpExtension = replaceExtension phpExtension

-- | Replace the current extension of a `File` with a ".png" extension.
withPngExtension :: MonadThrow m => Path b File -> m (Path b File)
withPngExtension = replaceExtension pngExtension

-- | Replace the current extension of a `File` with a ".py" extension.
withPyExtension :: MonadThrow m => Path b File -> m (Path b File)
withPyExtension = replaceExtension pyExtension

-- | Replace the current extension of a `File` with a ".rtf" extension.
withRtfExtension :: MonadThrow m => Path b File -> m (Path b File)
withRtfExtension = replaceExtension rtfExtension

-- | Replace the current extension of a `File` with a ".sh" extension.
withShExtension :: MonadThrow m => Path b File -> m (Path b File)
withShExtension = replaceExtension shExtension

-- | Replace the current extension of a `File` with a ".svg" extension.
withSvgExtension :: MonadThrow m => Path b File -> m (Path b File)
withSvgExtension = replaceExtension svgExtension

-- | Replace the current extension of a `File` with a ".tex" extension.
withTexExtension :: MonadThrow m => Path b File -> m (Path b File)
withTexExtension = replaceExtension texExtension

-- | Replace the current extension of a `File` with a ".txt" extension.
withTxtExtension :: MonadThrow m => Path b File -> m (Path b File)
withTxtExtension = replaceExtension txtExtension

-- | Replace the current extension of a `File` with a ".xml" extension.
withXmlExtension :: MonadThrow m => Path b File -> m (Path b File)
withXmlExtension = replaceExtension xmlExtension

-- | Replace the current extension of a `File` with a ".wiki" extension.
withWikiExtension :: MonadThrow m => Path b File -> m (Path b File)
withWikiExtension = replaceExtension wikiExtension
