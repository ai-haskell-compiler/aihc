-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.Parse.Source
-- Copyright   :  © 2015 Francesco Ariis
-- License     :  GPLv3 (see the LICENSE file)
--
-- Comments from source files
-----------------------------------------------------------------------------


module Lentil.Parse.Source where

import Lentil.Types
import Lentil.Helpers

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Char       as C
import qualified Data.Maybe      as M
import qualified Data.List       as L


-----------
-- TYPES --
-----------

-- from source to comment strings
type ParSource a = StateParser () a

data ParSyntax = -- 'standard' language (with single line and/or multiline
                 -- markers, etc.
                 StdSyntax { psLineComms   :: [String],
                             psBlockComms  :: [(String, String)],
                             psStringStyle :: EscapeStyle,
                             psStringLits  :: [Char],
                             psCharStyle   :: CharStyle,
                             psCharLits    :: [Char] }
               | OrgModeSyntax -- org-mode parsing
               | RstSyntax     -- reStructuredText/sphinx parsing
                 deriving (Show)

data EscapeStyle = ClangLike | SQLLike
                 deriving (Show, Eq)

data CharStyle = CommonChr | ErlangChr
               deriving (Show, Eq)


--------------
-- COMMENTS --
--------------

data CommentString = SingleLine Row String  -- sans newline
                   | MultiLine Row String
                   deriving (Show, Eq)

-- parses stuff like "-- comment"
lineComment :: String -> ParSource CommentString
lineComment s = SingleLine                 <$>
                (initId s *> getRow)       <*>
                manyTill anySingle eol
              <?> "line comment"
    where
          -- for 1-char line comments (#, ;, etc.), erases repeating
          initId :: String -> ParSource ()
          initId [a] = () <$ some (char a)
          initId as  = () <$ string as

blockComment :: (String, String) -> ParSource CommentString
blockComment (i, e) = MultiLine                         <$>
                      (string i *> getRow)              <*>
                      manyTill anySingle (try $ string e)
                    <?> "block comment"

getRow :: ParSource Row
getRow = fmap (unPos . sourceLine) getSourcePos


-------------
-- STRINGS --
-------------

litString :: EscapeStyle -> Char -> ParSource String
litString ClangLike ic = cString ic
litString SQLLike   ic = sqlString ic

-- string ancillaries --

-- quoted strings, escaped by \, by mauke^
cString :: Char -> ParSource String
cString ic = q *> many ((char '\\' *> anySingle) <|> noneOf no) <* q
           <?> "codestring"
    where q  = char ic
          no = C.showLitChar ic "\\"

-- sqllike string (no escape with \, '' to escape ')
sqlString :: Char -> ParSource String
sqlString ic = char ic *> manyTill anySingle (char ic)
    -- we treat 'cdscsad  cdscdsa '' csdcs' as two strings
    -- because the content is meaningless to our program


-----------
-- CHARS --
-----------

litChar :: CharStyle -> Char -> ParSource Char
litChar CommonChr ic = commonChar ic
litChar ErlangChr ic = erlangChar ic

-- quoted single character
commonChar :: Char -> ParSource Char
commonChar ic = q *> ((char '\\' *> anySingle) <|> anySingle) <* q
              <?> "char string sign"
    where q = char ic

-- $a for 'a' (where ic = '$')
erlangChar :: Char -> ParSource Char
erlangChar ic = char ic *> anySingle


------------------
-- OTHER BLOCKS --
------------------

-- a program is instructions to the computer. Ends when you meet
-- a well formed element from above (linecomm, blockcom, stringlit,
-- charLit)
program :: ParSyntax -> ParSource String
program ps = someTill anySingle (endp <|> ("" <$ eof))
             <?> "program"
    where
          -- endp :: [ParSource String]
          endp  = lookAhead . choice . map try $ map string posts ++ [lchars]

          -- every "post" symbol (init comments, listring char)
          posts  = psLineComms ps              ++ -- line
                   map fst (psBlockComms ps)   ++ -- block
                   map (:[]) (psStringLits ps)    -- lit string

          -- ambiguous symbols (pall'aaa'foo is valid haskell identifier,
          -- so it should stay here
          lchars = "" <$ choice (map (litChar (psCharStyle ps))
                                     (psCharLits ps))


-----------------------
-- RST+SPHINX SYNTAX --
-----------------------

rstDocumentPart :: ParSource (Maybe CommentString)
rstDocumentPart = (choice . map try $ [Just <$> rstTodo, Nothing <$ rstOther])
                <?> "rst document part"

-- `.. todo::` directive
rstTodo :: ParSource CommentString
rstTodo = MultiLine <$> getRow <*>
            (rstTodoIncipit *> fmap modText todoBody)
        <?> "rst todo directive"
  where
        startPara :: ParSource ()
        startPara = eol *> (() <$ satisfy (not . C.isSpace) <|> eof)

        todoBody :: ParSource String
        todoBody = someTill anySingle
                             (lookAhead . try $ startPara <|> eof)

        -- modText does 2 things
        --   1. injects a dummy todo keyword, having discarded `.. todo::`
        --   2. replaces '\n' with "\n ", so that issue is not broken
        --      by newline
        modText :: String -> String
        modText cs = "TODO " ++ addSpace cs

        addSpace :: String -> String
        addSpace [] = []
        addSpace ('\n':cs) = '\n' : ' ' : addSpace cs
        addSpace (c:cs) = c : addSpace cs

-- everything else
rstOther :: ParSource String
rstOther = someTill anySingle (endp <|> eof)
         <?> "rst other text"
    where
          endp = (lookAhead . try) (eol *> rstTodoIncipit) *>
                 eol *> pure () -- if you succeed, consume newline

rstTodoIncipit :: ParSource ()
rstTodoIncipit = () <$ string ".. todo::"
               <?> "rst todo incipit"

---------------------
-- ORG-MODE SYNTAX --
---------------------

orgModeDocument :: ParSource (Maybe CommentString)
orgModeDocument = choice [Just <$> try orgTodo,
                          Just <$> try orgListMulti,
                          Just <$> try orgListSingle,
                          Nothing <$ orgOther]
                <?> "org-mode document"

orgTodo :: ParSource CommentString
orgTodo = lineComment "*" <?> "org TODO"

orgListMulti :: ParSource CommentString
orgListMulti = MultiLine               <$>
               (orgInitList *> getRow) <*>
               body
               <?> "org-mode list item over multiple lines"
  where
    body :: ParSource String
    body = (++) <$> pure " TODO " <*>
           manyTill anySingle
           -- A list item over multiple lines terminates either:
           --   + when an empty lines follows the last
           --     line of the current item,
           --   + or when a new list item follows.  In
           --     this case, we must not consume the
           --     header of the list item.
           ( try (eol *> eol) <|>
             try (eol <* lookAhead (listMarker *> space)) <|>
             ("x" <$ eof)
           )

orgListSingle :: ParSource CommentString
orgListSingle = SingleLine              <$>
                (orgInitList *> getRow) <*>
                body
                <?> "org-mode list item in a single-line"
  where
    body :: ParSource String
    body = (++) <$> pure " TODO " <*> manyTill anySingle eol

-- - [ ] or + [ ] or 1. or 1)
orgInitList :: ParSource ()
orgInitList = listMarker *> space *>
              checkBox *> space
  where
    checkBox :: ParSource ()
    checkBox = () <$ string "[ ]"

listMarker :: ParSource Char
listMarker = choice
                [char '-',
                char '+',
                numMark]
  where
    numMark :: ParSource Char
    numMark = digitChar *> (char '.' <|> char ')')


orgOther :: ParSource ()
orgOther = choice
             [() <$ eol, -- an empty line
              () <$ someTill anySingle (endp <|> eof)] -- a non-empty one
         <?> "org-mode other text"
    where
          endp = () <$ eol


------------
-- SOURCE --
------------

-- given a set of lineparsers / blockparsers
sourcePart :: ParSyntax -> ParSource (Maybe CommentString)
sourcePart OrgModeSyntax                    = orgModeDocument
sourcePart RstSyntax                        = rstDocumentPart
sourcePart ps@(StdSyntax lc bc es sl cs cl) =
            choice [plc, pbc, psl, pcl, ppr]
            <?> "source file part"
    where
          ct = choice . map try

          plc = Just <$> (ct . map lineComment  $ lc)  -- line comm
          pbc = Just <$> (ct . map blockComment $ bc)  -- block comm
          psl = Nothing <$ (choice . map (try . litString es) $ sl)
                                                           -- str lit
          pcl = Nothing <$ (ct . map (litChar cs) $ cl)    -- char lit
          ppr = Nothing <$ program ps                      -- program

-- ps: syntax to sever comment from the rest
source :: ParSyntax -> ParSource [CommentString]
source ps = fmap M.catMaybes (some (sourcePart ps)) <|> ([] <$ eof)
         <?> "source file"


-------------
-- CONVERT --
-------------

type Comment = (Row, String)


comms2Tuple :: [CommentString] -> [Comment]
comms2Tuple [] = []
comms2Tuple (c:cs) | isSl c    = let (a,z) = span isSl (c:cs)
                                 in groupLineComms a ++ comms2Tuple z
                   | otherwise = comment2Tuple c : comms2Tuple cs
    where
          isSl (SingleLine _ _) = True
          isSl _                = False

-- use it on Line comments **only**!
groupLineComms :: [CommentString] -> [Comment]
groupLineComms cs = let cs'         = map comment2Tuple cs
                        f (r, s) i  = (r, s, r-i)

                        zipped      = zipWith f cs' (enumFrom 1)
                        grouped     = L.groupBy threeEq zipped

                    in map (flatComm . map back) grouped

    where
          threeEq (_, _, x) (_, _, y) = x == y

          back (a, b, _) = (a, b)
          flatComm cs'   = (fst . head $ cs',
                            unlines . map snd $ cs')

comment2Tuple :: CommentString -> Comment
comment2Tuple (SingleLine r t) = (r, t)
comment2Tuple (MultiLine  r t) = (r, t)
