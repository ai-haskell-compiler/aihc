module Text.HTML.Tagchup.Parser.Tag where

import Text.HTML.Tagchup.Parser.Combinator
   (allowFail, withDefault,
    voidChar, dropSpaces, getPos,
    many, many0toN, many1toN,
    many1Satisfy, readUntil,
    satisfy, voidString,
    emit, modifyEmission, )

import qualified Text.HTML.Tagchup.Parser.Combinator as Parser
import qualified Text.HTML.Tagchup.Parser.Status as Status
import qualified Text.HTML.Tagchup.Parser.Stream as Stream

import qualified Text.HTML.Tagchup.PositionTag as PosTag
import qualified Text.HTML.Tagchup.Tag         as Tag
import qualified Text.XML.Basic.Position    as Position
import qualified Text.HTML.Basic.Character    as HTMLChar
import qualified Text.XML.Basic.ProcessingInstruction as PI
import qualified Text.XML.Basic.Attribute   as Attr
import qualified Text.XML.Basic.Name        as Name
import qualified Text.XML.Basic.Tag         as TagName

import qualified Text.HTML.Tagchup.Character as Chr
import Text.HTML.Tagchup.Character (fromChar, )

import qualified Text.HTML.Basic.Entity as HTMLEntity

import qualified Control.Monad.Exception.Synchronous as Exc

import Control.Monad.Trans.Writer (runWriterT, )
import Control.Monad.Trans.State (StateT(..), )
import Control.Monad (mplus, msum, when, liftM, )

import Data.Monoid (Monoid, mempty, mconcat, )

import qualified Data.Map as Map

import Data.Tuple.HT (mapSnd, )
import Data.Char (isAlphaNum, chr, ord, )
import Data.Maybe (maybeToList, )

-- import qualified Numeric

type Warning = (Position.T, String)

type Parser     source a = Parser.Full     source Warning a
type ParserEmit source a = Parser.Emitting source Warning a


parsePosTagMergeWarnings ::
   (Stream.C source, StringType sink,
    Name.Attribute name, Name.Tag name) =>
   StateT (Status.T source) Maybe [PosTag.T name sink]
parsePosTagMergeWarnings =
   liftM (\((ot,ct),warns) ->
      ot :
      map (\(pos,warn) -> PosTag.cons pos $ Tag.Warning warn) warns ++
      maybeToList ct) $
   runWriterT parsePosTag

parsePosTag ::
   (Stream.C source, StringType sink,
    Name.Attribute name, Name.Tag name) =>
   Parser source (PosTag.T name sink, Maybe (PosTag.T name sink))
parsePosTag = do
   let omitClose :: Monad m => m t -> m (t, Maybe t)
       omitClose = liftM (\t -> (t, Nothing))
   pos <- getPos
   mplus
      (do voidChar '<'
          allowFail $ withDefault
             (msum $
                omitClose (parseSpecialTag pos) :
                omitClose (parseProcessingTag pos) :
                omitClose (parseCloseTag pos) :
                parseOpenTag pos :
                [])
             (do emitWarning pos "A '<', that is not part of a tag. Encode it as &lt; please."
                 omitClose (returnTag pos (Tag.Text $ stringFromChar '<'))))
      (omitClose (parseText pos))


{- |
Parsing an open tag may also emit a close tag
if the tag is self-closing, e.g. @\<br\/\>@.

For formatting self-closing tags correctly
it would be better to emit tags in the order @open tag, close tag, warnings@.
However, if there are infinitely many warnings,
we don't know whether a self-closing slash comes
and thus whether there is a close tag or not.
This implies, that we cannot even emit the warnings.

Thus we choose the order @open tag, warnings, close tag@.
-}
parseOpenTag ::
   (Stream.C source, StringType sink,
    Name.Attribute name, Name.Tag name) =>
   Position.T ->
   Parser source
      (PosTag.T name sink, Maybe (PosTag.T name sink))
parseOpenTag pos =
   do name <- parseName
      allowFail $
         do dropSpaces
            tag <- returningTag pos (Tag.Open name) $
               modifyEmission (restrictWarnings 10) $ many parseAttribute
            liftM ((,) tag) $ withDefault
               (do closePos <- getPos
                   voidString "/>"
                   allowFail $ liftM Just $ returnTag closePos (Tag.Close name))
               (do junkPos <- getPos
                   readUntilTerm
                      (\ junk ->
                         emitWarningWhen
                            (not $ null junk)
                            junkPos ("Junk in opening tag: \"" ++ junk ++ "\""))
                      ("Unterminated open tag \"" ++ Name.toString name ++ "\"") ">"
                   return Nothing)

parseCloseTag ::
   (Stream.C source, Name.Tag name) =>
   Position.T -> Parser source (PosTag.T name sink)
parseCloseTag pos =
   do voidChar '/'
      name <- parseName
      allowFail $
         do tag <- returnTag pos (Tag.Close name)
            dropSpaces
            junkPos <- getPos
            readUntilTerm
               (\ junk ->
                  emitWarningWhen
                     (not $ null junk)
                     junkPos ("Junk in closing tag: \"" ++ junk ++"\""))
               ("Unterminated closing tag \"" ++ Name.toString name ++"\"") ">"
            return tag

parseSpecialTag ::
   (Stream.C source, Name.Tag name) =>
   Position.T -> Parser source (PosTag.T name sink)
parseSpecialTag pos =
   do voidChar '!'
      msum $
       (do voidString "--"
           allowFail $ readUntilTerm
              (\ cmt -> returnTag pos (Tag.Comment cmt))
              "Unterminated comment" "-->") :
       (do voidString TagName.cdataString
           allowFail $ readUntilTerm
              (\ cdata -> returnTag pos (Tag.cdata cdata))
              "Unterminated cdata" "]]>") :
       (do name <- parseName
           allowFail $
              do dropSpaces
                 readUntilTerm
                    (\ info -> returnTag pos (Tag.Special name info))
                    ("Unterminated special tag \"" ++ Name.toString name ++ "\"") ">") :
       []

parseProcessingTag ::
   (Stream.C source, StringType sink,
    Name.Attribute name, Name.Tag name) =>
   Position.T -> Parser source (PosTag.T name sink)
parseProcessingTag pos =
   do voidChar '?'
      name <- parseName
      allowFail $
         do dropSpaces
            returningTag pos (Tag.Processing name) $
               if Name.matchAny ["xml", "xml-stylesheet"] name
                 then
                   do attrs <- many parseAttribute
                      junkPos <- getPos
                      readUntilTerm
                         (\ junk ->
                            emitWarningWhen (not $ null junk) junkPos
                               ("Junk in processing info tag: \"" ++ junk ++ "\""))
                         ("Unterminated processing info tag \"" ++ Name.toString name ++ "\"") "?>"
                      return $ PI.Known attrs
                 else readUntilTerm (return . PI.Unknown)
                         "Unterminated processing instruction" "?>"

parseText ::
   (Stream.C source, StringType sink) =>
   Position.T -> Parser source (PosTag.T name sink)
parseText pos =
   returningTag pos Tag.Text (parseCharAsString (const True))
--   returningTag pos Tag.Text (parseCharAsString ('<'/=))
--   returningTag pos Tag.Text (parseString1 ('<'/=))


parseAttribute ::
   (Stream.C source, StringType sink, Name.Attribute name) =>
   Parser source (Attr.T name sink)
parseAttribute =
   parseName >>= \name -> allowFail $
   do dropSpaces
      value <-
         withDefault
            (voidString "=" >> allowFail (dropSpaces >> parseValue))
            (return mempty)
      dropSpaces
      return $ Attr.Cons name value

parseName ::
   (Stream.C source, Name.C pname) =>
   Parser source pname
parseName =
   liftM Name.fromString $
   many1Satisfy (\c -> isAlphaNum c || c `elem` "_-.:")

parseValue ::
   (Stream.C source, StringType sink) =>
   ParserEmit source sink
parseValue =
   (msum $
      parseQuoted "Unterminated doubly quoted value string" '"' :
      parseQuoted "Unterminated singly quoted value string" '\'' :
      [])
   `withDefault`
   parseUnquotedValueAsString

parseUnquotedValueChar ::
   (Stream.C source) =>
   ParserEmit source String
parseUnquotedValueChar =
   let parseValueChar =
          do pos <- getPos
             str <- parseUnicodeChar (not . flip elem " >\"\'")
             let wrong = filter (not . isValidValueChar) str
             allowFail $
                emitWarningWhen (not (null wrong)) pos $
                "Illegal characters in unquoted value: " ++ wrong
             return str
   in  liftM concat $ many parseValueChar

parseUnquotedValueHTMLChar ::
   (Stream.C source) =>
   ParserEmit source [HTMLChar.T]
parseUnquotedValueHTMLChar =
   let parseValueChar =
          do pos <- getPos
             hc <- parseHTMLChar (not . flip elem " >\"\'")
             {- We do the check after each parseHTMLChar
                and not after (many parseValueChar)
                in order to correctly interleave warnings. -}
             allowFail $ mapM_ (checkUnquotedChar pos) hc
             return hc
   in  liftM concat $ many parseValueChar

checkUnquotedChar :: Position.T -> HTMLChar.T -> ParserEmit source ()
checkUnquotedChar pos x =
   case x of
      HTMLChar.Unicode c ->
         emitWarningWhen (not (isValidValueChar c)) pos $
            "Illegal characters in unquoted value: '" ++ c : "'"
      _ -> return ()


isValidValueChar :: Char -> Bool
isValidValueChar c  =  isAlphaNum c || c `elem` "_-:."

parseQuoted ::
   (Stream.C source, StringType sink) =>
   String -> Char -> Parser source sink
parseQuoted termMsg quote =
   voidChar quote >>
   (allowFail $
    do str <- parseString (quote/=)
       withDefault
          (voidChar quote)
          (do termPos <- getPos
              emitWarning termPos termMsg)
       return str)

{-
Instead of using 'generateTag' we could also wrap the call to 'readUntilTerm'
in 'mfix' in order to emit a tag, where some information is read later.
-}
readUntilTerm ::
   (Stream.C source) =>
   (String -> ParserEmit source a) -> String -> String -> ParserEmit source a
readUntilTerm generateTag termWarning termPat =
   do ~(termFound,str) <- readUntil termPat
      result <- generateTag str
      termPos <- getPos
      emitWarningWhen (not termFound) termPos termWarning
      return result


class Chr.C char => CharType char where
   parseChar :: (Stream.C source) => (Char -> Bool) -> Parser source [char]
   parseUnquotedValue :: (Stream.C source) => ParserEmit source [char]

instance CharType Char where
   parseChar = parseUnicodeChar
   parseUnquotedValue = parseUnquotedValueChar

instance CharType HTMLChar.T where
   parseChar = parseHTMLChar
   parseUnquotedValue = parseUnquotedValueHTMLChar


class Monoid sink => StringType sink where
   stringFromChar :: Char -> sink
   parseCharAsString ::
      (Stream.C source) =>
      (Char -> Bool) -> Parser source sink
   parseUnquotedValueAsString ::
      (Stream.C source) =>
      ParserEmit source sink

instance CharType char => StringType [char] where
   stringFromChar c = [fromChar c]
   parseCharAsString = parseChar
   parseUnquotedValueAsString = parseUnquotedValue


parseString  ::
   (Stream.C source, StringType sink) =>
   (Char -> Bool) -> ParserEmit source sink
parseString  p = liftM mconcat $ many  (parseCharAsString p)

{-
parseString1 ::
   (Stream.C source, StringType sink) =>
   (Char -> Bool) -> Parser     name source sink sink
parseString1 p = liftM mconcat $ many1 (parseCharAsString p)
-}



parseUnicodeChar ::
   (Stream.C source) =>
   (Char -> Bool) -> Parser source String
parseUnicodeChar p =
   do pos <- getPos
      x <- parseHTMLChar p
      allowFail $ liftM concat $
         mapM (htmlCharToString pos) x

htmlCharToString ::
   Position.T -> HTMLChar.T -> ParserEmit source String
htmlCharToString pos x =
   let returnChar c = return $ c:[]
   in  case x of
          HTMLChar.Unicode c -> returnChar c
          HTMLChar.CharRef num -> returnChar (chr num)
          HTMLChar.EntityRef name ->
             maybe
                (let refName = '&':name++";"
                 in  emitWarning pos ("Unknown HTML entity " ++ refName) >>
                     return refName)
                returnChar
                (Map.lookup name HTMLEntity.mapNameToChar)

{- |
Only well formed entity references are interpreted as single HTMLChars,
whereas ill-formed entity references are interpreted as sequence of unicode characters without special meaning.
E.g. "&amp ;" is considered as plain "&amp ;",
and only "&amp;" is considered an escaped ampersand.
It is a very common error in HTML documents to not escape an ampersand.
With the interpretation used here,
those ampersands are left as they are.

At most one warning can be emitted.
-}
parseHTMLChar ::
   (Stream.C source) =>
   (Char -> Bool) -> Parser source [HTMLChar.T]
parseHTMLChar p =
   do pos <- getPos
      c <- satisfy p
      allowFail $
        if c=='&'
          then
            withDefault
              (do ent <-
                     mplus
                        (do voidChar '#'
                            digits <- allowFail $ many0toN 10 (satisfy isAlphaNum)
                               -- exclude ';', '"', '<'
                               -- include 'x'
                            Exc.switch
                               (\e ->
                                  allowFail (emitWarning pos ("Error in numeric entity: " ++ e)) >>
                                  return (map HTMLChar.fromUnicode ('&':'#':digits)))
                               (return . (:[]) . HTMLChar.CharRef . ord)
                               (HTMLEntity.numberToChar digits))
                        (liftM ((:[]) . HTMLChar.EntityRef) $
                         many1toN 10 (satisfy isAlphaNum))
                  voidChar ';'
                  return ent)
              (emitWarning pos "Non-terminated entity reference" >>
               return [HTMLChar.Unicode '&'])
          else return [HTMLChar.Unicode c]

{-
readHex :: (Num a) => String -> a
readHex str =
   case Numeric.readHex str of
      [(n,"")] -> n
      _ -> error "readHex: no parse"

{-
We cannot emit specific warnings,
because the sub-parsers simply fail
and then throw away the warnings.
-}
parseHTMLCharGenericWarning ::
   (Stream.C source) =>
   (Char -> Bool) -> Parser source [HTMLChar.T]
parseHTMLCharGenericWarning p =
   do pos <- getPos
      c <- satisfy p
      allowFail $
        if c=='&'
          then
            withDefault
              (do ent <-
                     mplus
                        (voidChar '#' >>
                         liftM HTMLChar.CharRef
                            (mplus
                               (voidChar 'x' >> liftM readHex (many1toN 8 (satisfy isHexDigit)))
                               (liftM read (many1toN 10 (satisfy isDigit)))))
                        (liftM HTMLChar.EntityRef $ many1toN 10 (satisfy isAlphaNum))
                  voidChar ';'
                  return [ent])
              (emitWarning pos "Ill formed entity" >>
               return [HTMLChar.Unicode '&'])
          else return [HTMLChar.Unicode c]
-}


restrictWarnings :: Int -> [Warning] -> [Warning]
restrictWarnings n =
   uncurry (++) .
   mapSnd
      (\rest ->
          case rest of
             (pos, _) : _ ->
                [(pos, "further warnings suppressed")]
             _ -> []) .
   splitAt n


-- these functions have intentionally restricted types

emitWarningWhen :: Bool -> Position.T -> String -> ParserEmit source ()
emitWarningWhen cond pos msg =
   when cond $ emitWarning pos msg

emitWarning :: Position.T -> String -> ParserEmit source ()
emitWarning = curry emit

returnTag ::
   Position.T ->
   Tag.T name sink ->
   ParserEmit source (PosTag.T name sink)
returnTag p t = return (PosTag.cons p t)

returningTag ::
   (Monad m) =>
   Position.T ->
   (a -> Tag.T name sink) ->
   m a ->
   m (PosTag.T name sink)
returningTag pos f =
   liftM (PosTag.cons pos . f)
