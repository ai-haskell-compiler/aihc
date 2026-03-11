module System.ReadEditor
       ( -- * Main exports
         readEditor
       , readEditorWith
       , openEditor
       , openEditorWithPrompt
         -- * Utility
       , prompt
       , withSystemTempFile
       )
  where

import           Control.Exception  (finally)
import           System.Directory   (getTemporaryDirectory, removeFile)
import           System.Environment (getEnv)
import           System.IO          (Handle, hClose, hFlush, hPutStr,
                                     openTempFile, stdout)
import           System.IO.Error    (catchIOError)
import           System.Process     (system)

-- | Opens a file in the sytem's editor and returns it's contents after it's saved.
readEditor :: IO String
readEditor = withSystemTempFile "read-editor" readEditor'

-- | Opens a file, fills it some content and returns it's contents after it's saved.
readEditorWith :: String -> IO String
readEditorWith contents = withSystemTempFile "read-editor" $ \fp temph -> do
    hPutStr temph contents
    hFlush temph
    readEditor' fp temph

readEditor' :: FilePath -> Handle -> IO String
readEditor' fp temph = do
    openEditor fp
    hClose temph
    readFile fp

-- | Opens a file in the sytem's editor, waits until it's closed.
openEditor :: FilePath -> IO ()
openEditor = openEditorWithPrompt "What editor should I use? "

-- | A version of 'openEditor' which takes the fallback prompt question.
openEditorWithPrompt :: String -> FilePath -> IO ()
openEditorWithPrompt p fp = do
    editorCmd <- catchIOError (getEnv "EDITOR") (const promptForEditor)
    _ <- system $ editorCmd ++ " " ++ fp
    return ()
  where
    promptForEditor = prompt p

-- | Prompts for a line and returns it
prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine

-- | Executes a function with an open temporary file Handle.
withSystemTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withSystemTempFile templ fn = do
    tempdir <- catchIOError getTemporaryDirectory (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir templ
    finally
        (fn tempfile temph)
        (removeFile tempfile)
