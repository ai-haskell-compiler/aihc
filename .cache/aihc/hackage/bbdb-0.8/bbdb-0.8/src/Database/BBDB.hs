{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | 
-- This module can read and write BBDB files, and provides a few handy
-- functions for getting at fields inside of BBDB data.
-- 
-- BBDB (now version 3) (<http://savannah.nongnu.org/projects/bbdb/>)
-- is short for the Insidious Big Brother Database, which is a contact
-- management utility that can be integrated into Emacs (the one true
-- editor.)  Since bbdb.el is implemented in elisp, it can be
-- difficult to \"get at\" the data inside a .bbdb file with external
-- programs.  Many years ago, I wrote a BBDB interface for perl, but
-- having experience enlightenment at the hands of the category gods,
-- I\`m now dabbling with Haskell.  But having been a loyal Emacs user
-- for many years now, I wanted a way to spam my friends while still
-- using my favorite programming language.  Hence the module
-- Data.BBDB.
-- 
-- The following is the data layout for a BBDB record.  I have created a
-- sample record with my own data.  Each field is just separated by a
-- space.  I have added comments to the right
-- 
-- > ["Henry"                                   The first name - a string
-- > "Laxen"                                    The last name - a string
-- > nil                                        Affixes - a comma separated list
-- > ("Henry, Enrique")                         Also Known As - a comma separated list
-- > ("Elegant Solutions")                      Organizations- a comma separated list
-- > (
-- >  ["reno" 775 624 1851 0]                   Phone number field - US style
-- >  ["chapala" "011-52-376-765-3181"]         Phone number field - International style
-- > ) 
-- > (
-- >  ["mailing"                                The address location, then a list
-- >  ("10580 N. McCarran Blvd." "#115-396")    for the street address, then one each
-- >  "Reno" "Nevada" "89503" "USA"             for City, State, Zip Code, and country
-- > ] 
-- >  ["home"                                   another Address field
-- >  ("Villa Alta #6" "Gaviotas #10")            The street list
-- >  "Chapala" "Jalisco"                       City State
-- >  "45900" "Mexico"                          Zip and country
-- > ]) 
-- > (
-- >  "nadine.and.henry@pobox.com"              the net addresses - a list of strings
-- >  "nadinelaxen@pobox.com"
-- > ) 
-- > (
-- >  (notes . "Always split aces and eights")  The notes field - a list of alists
-- >  (birthday . "6/15")
-- > )
-- > "169cc701-8754-45f5-aba8-c89c2dc60a49"     The hash field based
-- >                                            on names, organizations, akas, and emails
-- > "2010-09-03"                               The creation date
-- > "2017-11-06 13:58:33 +0000"                The last modifcation time
-- > nil                                        The cache vector - always nil
-- > ]
-- 
-- Inside the .bbdb file, this looks like:
-- \[\"Henry\" \"Laxen\" nil (\"Henry, Enrique\") (\"Elegant Solutions\")
-- (\[\"reno\" 775 624 1851 0] \[\"chapala\" \"011-52-376-765-3181\"]) 
-- (\[\"mailing\" (\"10580 N. McCarran Blvd.\" 
-- \"#115-396\") \"Reno\" \"Nevada\" \"89503\" \"USA\"] 
-- \[\"home\" (\"Via Alta #6\" \"Gaviotas #10\") 
-- \"Chapala\" \"Jalisco\" \"45900\" \"Mexico\"]) 
-- (\"nadine.and.henry\@pobox.com\" \"nadinelaxen\@pobox.com\") 
-- ((notes . \"Always split aces and eights\") 
-- (birthday . \"6/15\")) "169cc701-8754-45f5-aba8-c89c2dc60a49"
-- "2010-09-03" "2017-11-06 13:58:33 +0000" nil]
-- 
-- When parsed, this is represented inside Haskell as:
--  
-- >      BBDBEntry
-- >        (BBDB{firstName = Just "Henry", lastName = Just "Laxen",
-- >              affix = Nothing
-- >              aka = Just ["Henry, Enrique"], company = Just ["Elegant Solutions"],
-- >              phone =
-- >                Just
-- >                  [USStyle "reno" ["775", "624", "1851", "0"],
-- >                   InternationalStyle "chapala" "011-52-376-765-3181"],
-- >              address =
-- >                Just
-- >                  [Address{location = "mailing",
-- >                           streets =
-- >                             Just ["10580 N. McCarran Blvd.", "#115-396"],
-- >                           city = Just "Reno", state = Just "Nevada",
-- >                           zipcode = Just "89503", country = Just "USA"},
-- >                   Address{location = "home",
-- >                           streets = Just ["Via Alta #6", "Gaviotas #10"],
-- >                           city = Just "Chapala", state = Just "Jalisco",
-- >                           zipcode = Just "45900", country = Just "Mexico"}],
-- >              net = Just ["nadine.and.henry@pobox.com", "nadinelaxen@pobox.com"],
-- >              notes =
-- >                Just
-- >                  (Note{unnote =
-- >                          [("notes", "Always split aces and eights"),
-- >                           ("birthday", "6/15")]})})]
-- >             hash = "169cc701-8754-45f5-aba8-c89c2dc60a49",
-- >             creation = "2010-09-03",  
-- >             modification = "2017-11-06 13:58:33 +0000" 
-- > 

module Database.BBDB 
  (
    Location,
    Street,
    Symbol,
    Address(..), 
    Alist, 
    Note(..), 
    Phone(..), 
    BBDB(..), 
    BBDBFile(..),
    LispAble(..),
    Hash,
    CreationDate,
    ModificationTime,
    bbdbDefault,
    key,value,
    parseBBDB,
    bbdbFileParse,
    justEntry,
    justEntries,
    readBBDB,
    wantNote,
    getNote,
    mapBBDB,
    filterBBDB
  ) where


import Text.Parsec.Char
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import Text.Parsec hiding ((<|>))
import Control.Applicative hiding (many)
import Data.Maybe

doubleQuoteChar :: Char
doubleQuoteChar = '"'

betweenParens :: Parser a -> Parser a
betweenParens   = between (char '(') (char ')')

quotedString :: Parser String 
--quotedString :: (Stream s m Char) => ParsecT s u Identity String -> ParsecT s u Identity String
quotedString = 
    between (char doubleQuoteChar) (char doubleQuoteChar)  $
    many quotedChar

quotedChar :: Parser Char  
quotedChar =
  noneOf "\\\"" <|>
  try (string "\\\"" >> return '"') <|>
  noneOf "\""

-- | A Location is just a synonym for String.  Each BBDB Address and
-- Phone field must be associated with a location, such as /home/ or
-- /work/
type Location = String
-- | A Street is also a synonym for String.  Each Address may have a
-- list of Streets associated with it.
type Street = String
-- | A Symbol is just a String, but Lisp only wants
-- alphanumerics and the characters _ (underscore) and - (dash)
type Symbol = String

-- | Since file-format 9, BBDB now includes there more fields, which
-- | are always present and used internally.
-- | Synonym for String
type Hash   = String
-- | Synonym for String
type CreationDate = String
-- | Synonym for String
type ModificationTime = String
-- | For some unknow reason, BBDB can have phones in two different
-- formats.  In /USStyle/, the phone is list of integers, in the form
-- of Area code, Prefix, Number, and Extension.  I don\'t bother to
-- convert the strings of digits to actual integers.  In
-- /InternationalStyle/, the phone number is just a String.
data Phone =
    USStyle Location [String] 
     |
    InternationalStyle Location String
                 deriving (Eq, Ord, Show)
-- | An Address must have a location, and may have associated streets,
-- a city, a state, a zipcode, and an country.
data Address = Address {
                 location :: Location,
                 streets  :: Maybe [String],
                 city     :: Maybe String,
                 state    :: Maybe String,
                 zipcode  :: Maybe String,
                 country  :: Maybe String
                 }
               deriving (Eq, Ord, Show)

-- | An Alist is an Association List.  Lisp writes these as (key
-- . value) We convert these to a tuple in haskell where fst is key
-- and snd is value.  
type Alist = (Symbol,String)

-- | Given an Alist, return the key
key :: (x,y) -> x
key   (x,_) = x
-- | Given an Alist, return the value
value :: (x,y) -> y
value (_,y) = y

-- | The Note field of a BBDB record is just a list of associations.
-- If you don\'t provide a your own key, the BBDB will use the word \"note\"

data Note = Note {
                   unnote :: [Alist]
                 }
            deriving (Eq, Ord, Show)

-- | The record fields of the BBDB data type 
data BBDB = BBDB {
-- | the first name.  Why is this a Maybe?  Because sometimes you just
-- have a company, and not a specific first name

                      firstName    :: Maybe String,
                      lastName     :: Maybe String,
-- | aka = Also Known As.  Sometimes the same email address can match
-- several users, so BBDB gives you the option of remembering
-- different names for the same address
                      affix        :: Maybe [String],
                      aka          :: Maybe [String],
-- | The company if any                      
                      company      :: Maybe [String],
-- | A list of phone numbers, either in US Style or International Style
                      phone        :: Maybe [Phone],
-- | A list of addresses, keyed by location
                      address      :: Maybe [Address],
-- | A list of email addresses.  
-- BBDB uses the first element of this field when you create a new email
                      net          :: Maybe [String],
-- | Any number of key, value pairs.  Great for random data.
                      notes        :: Maybe Note,
                      hash         :: Hash,
                      creation     :: CreationDate,
                      modification :: ModificationTime
                  }             
                    deriving (Eq, Ord, Show)

-- | A BBDB record containing no data
bbdbDefault :: BBDB
bbdbDefault = BBDB Nothing Nothing Nothing Nothing Nothing Nothing
              Nothing Nothing Nothing "" "" ""

-- | At the beginning of a BBDB file are a variable number of comments, which
-- specify the encoding type and the version.  We just ignore them.
-- Comments starts with a \; (semi-colon) and continue to end of line
data BBDBFile = 
  BBDBComment String 
   | 
  BBDBEntry BBDB
                    deriving (Eq, Ord, Show)

-- | return Nothing if parsing the string \"nil\"
nil :: Parser (Maybe a)
nil = string "nil" >> return Nothing

strings :: Parser [String]
strings = betweenParens (sepBy quotedString space)
  

stringOrNil :: Parser (Maybe String)
stringOrNil = 
    nil <|> Just <$> quotedString <?> "nil or string"


stringsOrNil :: Parser (Maybe [String])
stringsOrNil = 
    nil <|> Just <$> strings
        

listOfInts :: Parser [String]
listOfInts = sepBy1 (many1 digit) space

phoneParser :: Parser Phone
phoneParser = do
      _ <- char '[' 
      phoneType <- quotedString
      spaces
      n <- singlePhone phoneType
      _ <- char ']' 
      return n
  where 
    singlePhone phoneType = do
        n <- listOfInts
        return $ USStyle phoneType n
      <|> do
        n <- quotedString
        return $ InternationalStyle phoneType n

phonesParser :: Parser (Maybe [Phone])
phonesParser = 
        try nil
    <|> Just <$> betweenParens (sepBy phoneParser space)


singleAddress :: Parser Address
singleAddress = 
  char '[' *>
  (Address <$>
  quotedString <* space <*> 
  stringsOrNil <* space <*> 
  stringOrNil  <* space <*> 
  stringOrNil  <* space <*> 
  stringOrNil  <* space <*> 
  stringOrNil) <* char ']'

addressesParser :: Parser (Maybe [Address])
addressesParser = 
        nil
    <|> Just <$> betweenParens (sepBy singleAddress space)

    
lispSymbol :: Parser Symbol
lispSymbol = many1 (alphaNum <|> oneOf "-_")



alist :: Parser Alist
alist = betweenParens $
        (,) <$> lispSymbol <*> (string " . " *> quotedString)


notesParser :: Parser (Maybe Note)
notesParser = 
       nil
   <|> Just <$> betweenParens (Note <$> sepBy alist space)

bbdbEntry :: Parser BBDB              
bbdbEntry =
  char '[' *>
  (BBDB <$>
  stringOrNil      <* space <*> 
  stringOrNil      <* space <*> 
  stringsOrNil     <* space <*> 
  stringsOrNil     <* space <*> 
  stringsOrNil     <* space <*>
  phonesParser     <* space <*>
  addressesParser  <* space <*>
  stringsOrNil     <* space <*>
  notesParser      <* space <*>
  quotedString     <* space <*>
  quotedString     <* space <*>
  quotedString)
  <* space
  <* string "nil"
  <* char ']'


-- | The Parser for a BBDB file, as it is written on disk.  If you
-- read a .bbdb file with:
-- 
-- > testParse :: FilePath -> IO (Either ParseError [BBDBFile])
-- > testParse filename = do
-- >   b <- readFile filename
-- >   return $  parse bbdbFileParse "bbdb" b
-- 
-- You will get IO (Right [BBDBFile]) if the parse went ok
-- 
bbdbFileParse :: Parser [BBDBFile]
bbdbFileParse = do
  comments <-  many commentLine
  entries <- many (bbdbEntry <* newline)
  eof
  return $ map BBDBComment comments ++ map BBDBEntry entries
  where
    commentLine = (:) <$> char ';' <*> (many (noneOf "\n\r") <* newline)

-- | converts a BBDB comment to nothing, and a BBDB entry to just the entry
justEntry :: BBDBFile -> Maybe BBDB
justEntry (BBDBComment _) = Nothing
justEntry (BBDBEntry x) = Just x

-- | returns a list of  only the actual bbdb entries, removing the comments
justEntries :: [BBDBFile] -> [BBDB]
justEntries = mapMaybe justEntry

-- | surround a string with the given two characters  
surroundWith :: a -> a -> [a] -> [a]
surroundWith before after str = before : str ++ [after]

surroundWithQuotes, surroundWithBrackets, surroundWithParens :: String -> String
surroundWithQuotes   = surroundWith '"' '"'
surroundWithBrackets = surroundWith '[' ']'
surroundWithParens   = surroundWith '(' ')'

-- | convert a Haskell string to a string that Lisp likes
escapeLisp :: String -> String
escapeLisp [] = []
escapeLisp (c:cs) = 
  case c of
    '"' -> '\\' : '"' : escapeLisp cs
    _ -> c : escapeLisp cs

-- | LispAble is how we convert from our internal representation of a
-- BBDB record, to one that will make Lisp and Emacs happy.  (Sans bugs)
-- 
-- > testInverse = do
-- >   let inFile = "/home/henry/.bbdb"
-- >   actualBBDBFile <- readFile inFile
-- >   parsedBBDBdata <- readBBDB inFile
-- >   let bbdbDataOut = asLisp parsedBBDBdata
-- >   print $ actualBBDBFile == bbdbDataOut
-- >  
-- 
--  should print True
class LispAble s where
  asLisp :: s -> String

instance LispAble String where
  asLisp = escapeLisp  

instance LispAble (Maybe String) where
  asLisp   Nothing = "nil"
  asLisp   (Just x) = surroundWithQuotes . escapeLisp $ x

instance LispAble (Maybe [String]) where
  asLisp   Nothing = "nil"
  asLisp   (Just x) = surroundWithParens . unwords .
                        map (surroundWithQuotes . asLisp) $ x

instance LispAble Phone where
  asLisp (USStyle loc numbers) =
    surroundWithBrackets $ surroundWithQuotes loc ++ " " ++ 
    unwords numbers
  asLisp (InternationalStyle loc numbers) =  
    surroundWithBrackets $ surroundWithQuotes loc ++ " " ++ 
    surroundWithQuotes numbers

instance LispAble (Maybe [Phone]) where
  asLisp   Nothing = "nil"
  asLisp   (Just x) = surroundWithParens . unwords . map asLisp $ x

instance LispAble Address where
  asLisp x = surroundWithBrackets $ unwords 
    [asLisp $ Just (location x),
     asLisp (streets x),
     asLisp (city x),
     asLisp (state x),
     asLisp (zipcode x),
     asLisp (country x)]

instance LispAble (Maybe [Address]) where
  asLisp   Nothing = "nil"
  asLisp   (Just x) = surroundWithParens . unwords .
                        map asLisp $ x

instance LispAble Alist where
  asLisp x = surroundWithParens $
    key x ++ " . " ++ asLisp (Just (value x))

instance LispAble Note where
  asLisp (Note x)  = surroundWithParens . unwords .
                      map asLisp $ x
  
instance LispAble (Maybe Note) where
  asLisp   Nothing = "nil"
  asLisp   (Just x) = surroundWithParens . unwords . 
                        map asLisp $ unnote x
                        
instance LispAble BBDB where
  asLisp x = surroundWithBrackets $ unwords 
   [asLisp (firstName x),
    asLisp (lastName x),
    asLisp (affix x),
    asLisp (aka x),
    asLisp (company x),
    asLisp (phone x),
    asLisp (address x),
    asLisp (net x),
    asLisp (notes x),
    asLisp (Just (hash x)),
    asLisp (Just (creation x)),
    asLisp (Just (modification x)),
    "nil"
   ]

instance LispAble BBDBFile where
  asLisp (BBDBComment x) = x
  asLisp (BBDBEntry x) = asLisp x

-- | the inverse of bbdbFileParse
instance LispAble [BBDBFile] where
  asLisp = unlines . map asLisp

-- | parse the string as a BBDB File
parseBBDB :: String -> Either ParseError [BBDBFile]
parseBBDB  = parse bbdbFileParse "bbdb"

-- | read the given file and call error if the parse failed,
-- otherwise return the entire file as a list of BBDBFile records.
readBBDB :: String -> IO [BBDBFile]
readBBDB filename = do
  b <- readFile filename
  let ls = parseBBDB b
  return . either (error . show)  id $ ls

-- | Notes inside a BBDB record are awkward to get at.  This helper
-- function digs into the record and applies a function to each
-- Alist element of the record.  It returns true if it any of the
-- Alists in the note return true.  For example:
--  
-- > hasBirthday :: BBDB -> Bool
-- > hasBirthday = wantNote (\x -> key x == "birthday")
--  
-- will return True for any BBDB record that has a \"birthday\" key
-- in it\'s notes field
wantNote :: (Alist -> Bool) -> BBDB -> Bool
wantNote cond bbdb = maybe False alistTest (notes bbdb)
  where
    alistTest = any cond . unnote

-- | Lookup the value whose key is the given string.  If found returns 
-- Just the value, otherwise Nothing  For example:
--
-- > getBirthday :: BBDB -> Maybe String
-- > getBirthday = getNote "birthday"
--
getNote :: String -> BBDB -> Maybe String
getNote k b = lookup k  (maybe []  unnote (notes b))

-- | This and filterBBDB are the main functions you should use to
-- manipulate a set of BBDB entries.  You supply a function that
-- applies a transformation on a BBDB record, and this function will
-- apply that transformation to every BBDBEntry in a BBDB file.
-- Sample usage:
-- 
-- > starCompanies = do
-- >   b <- readBBDB "/home/henry/.bbdb"
-- >   writeFile "/home/henry/.bbdb-new" $ asLisp . mapBBDB starCompany $ b
-- >   where
-- >     starCompany x = case (company x) of
-- >       Nothing -> x
-- >       Just y -> x { company = Just ("*" ++ y) }
-- 
-- Prepend a star (\"*\") to each company 
-- field of a BBDB file and write the result
-- out as a new bbdb file.
mapBBDB :: (BBDB -> BBDB) -> [BBDBFile] -> [BBDBFile]
mapBBDB f = map g
  where
    g (BBDBComment x) = BBDBComment x
    g (BBDBEntry x) = BBDBEntry (f x)

-- | Just like mapBBDB except it filters.  You supply a function that
-- takes a BBDB record to a Bool, and filterBBDB will return a new
-- list of BBDBFile that satisfy that condition.  Sample usage:
-- 
-- > import Text.Regex.Posix
-- > -- do regex matching while ignoring case, so "reno" matches "Reno"
-- > matches x = match (makeRegexOpts compIgnoreCase defaultExecOpt x :: Regex)
-- 
-- > getReno = do
-- >   b <- readBBDB "/home/henry/.bbdb"
-- >   let c = justEntries . filterBBDB hasReno $ b
-- >   mapM_ print $ map (\a -> (firstName a, lastName a, address a)) c
-- >   where
-- >     isReno :: Maybe String -> Bool
-- >     isReno = maybe False (matches "reno")
-- >     anyAddressHasReno :: [Address] -> Bool
-- >     anyAddressHasReno = any id . map (isReno . city)
-- >     hasReno :: BBDB -> Bool
-- >     hasReno = maybe False anyAddressHasReno . address
-- 
-- print the name and all addresses of anyone in the BBDB file
-- who live in Reno.  
filterBBDB :: (BBDB -> Bool) -> [BBDBFile] -> [BBDBFile]
filterBBDB f = filter g
  where
    g (BBDBComment _) = False
    g (BBDBEntry x) = f x    

  
{-
usPhone :: Phone
usPhone = USStyle "home" ["+1", "775", "624", "9011"]
itPhone :: Phone
itPhone = InternationalStyle "work" "+52 (376) 765-3181"
ad1 :: Address
ad1 = Address "home" (Just ["Via Alta #6", "Gaviotas #10"])
                     (Just "Chapala")
                     (Just "Jalisco")
                     (Just "45900")
                     (Just "Mexico")
notes1 :: Note
notes1 = (Note [("notes", "Always split aces and eights"),
               ("birthday", "6/15")])
b1 :: BBDB
b1 = BBDB
     (Just "Henry")
     (Just "Laxen")
     Nothing
     Nothing
     (Just ["Elegant Solutions"])
     (Just [usPhone, itPhone])
     (Just [ad1])
     Nothing
     (Just notes1)
     "hash"
     "creation"
     "modification"
     
usPhone :: Phone
usPhone = USStyle "home" ["+1", "775", "624", "9011"]
itPhone :: Phone
itPhone = InternationalStyle "work" "+52 (376) 765-3181"
ad1 :: Address
ad1 = Address "home" (Just ["Via Alta #6", "Gaviotas #10"])
                     (Just "Chapala")
                     (Just "Jalisco")
                     (Just "45900")
                     (Just "Mexico")
notes1 :: Note
notes1 = (Note [("notes", "Always split aces and eights"),
               ("birthday", "6/15")])
b1 :: BBDB
b1 = BBDB
     (Just "Henry")
     (Just "Laxen")
     Nothing
     Nothing
     (Just ["Elegant Solutions"])
     (Just [usPhone, itPhone])
     (Just [ad1])
     Nothing
     (Just notes1)     "hash"
     "creation"
     "modification"
     

-}
