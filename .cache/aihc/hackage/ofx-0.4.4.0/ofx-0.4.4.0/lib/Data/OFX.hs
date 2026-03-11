{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Parser for downloaded OFX files.
--
-- This parser was written based on the OFX version 1.03
-- specification, which is available at
--
-- <http://www.ofx.net>
--
-- It will probably work on earlier versions of OFX without
-- incident. However, it may or may not not work on newer versions of
-- OFX, which are XML based (this version of OFX is SGML based.)
--
-- It will also parse QFX files, which are OFX files with minor
-- proprietary additions by Intuit, the maker of Quicken.
--
-- An OFX file consists of three parts: the HTTP headers (which this
-- parser does NOT handle because typically they will not show up in
-- files downloaded to disk), the OFX headers, and the OFX data. This
-- parser handles the OFX headers and the OFX data.
--
-- The parser in this module simply parses the tags and data into a
-- tree, which you can manipulate with other functions. Some functions
-- are provided to find the transactions in the tree and place them
-- into a 'Transaction' type, which is the data you are most likely
-- interested in. If you are interested in other data you can query
-- the 'Tag' tree for what you need.
--
-- The @ofx@ package includes two executable files that you can use at
-- the command line to test the library and see how it works.  The
-- @renderTransactions@ executable reads an OFX file on standard
-- input, runs it through the 'prettyRenderTransactions' function, and
-- prints the result to standard output.  The @renderOfx@ executable
-- reads an OFX file on standard input, runs it through the
-- 'prettyRenderOfxFile' function, and prints the result to standard
-- output.

module Data.OFX
  ( -- * Error handling
    Err
  
    -- * The OFX data tree
  , HeaderTag
  , HeaderValue
  , OFXHeader(..)
  , TagName
  , TagData
  , Tag(..)
  , OFXFile(..)

  -- * Manipulating the OFX tag tree
  , find
  , findPath
  , tagData
  , pathData
  , findData

  -- * Extracting specific data
  , fiName
  , creditCardNumber
  , bankAccountNumber
  , accountNumber

  -- * Types to represent specific OFX data
  , Transaction(..)
  , transaction
  , transactions
  , TrnType(..)
  , trnType
  , Payee(..)
  , payee
  , CorrectAction(..)
  , BankAcctTo(..)
  , bankAcctTo
  , CCAcctTo(..)
  , ccAcctTo
  , AcctType(..)
  , acctType
  , CurrencyData(..)
  , currencyData
  , Currency(..)
  , currency
  , OrigCurrency(..)
  , origCurrency

  -- * Running parsers
  , parseOfxFile
  , loadOfxFile
  , parseTransactions
  , loadTransactions
  , prettyRenderOfxFile
  , prettyRenderTransactions

  -- * Parsec parsers
  , ofxFile
  , newline
  , escChar
  , header
  , openingTag
  , closingTag
  , tag
  , date
  , time
  , tzOffset

  -- * Pretty printers
  , pPayee
  , pTransaction
  , pTag
  , pHeader
  , pFile
  , pEither
  , pMaybe
  , pList
  , label
  , pExceptional
  ) where

--
-- # Imports
--

import Control.Applicative (many, optional, (<|>))
import Control.Monad (replicateM, (<=<))
import Data.Data (Data)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import qualified Data.Monoid as M
import qualified Data.Time as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Parsec.String (Parser)
import Text.Parsec
  ( lookAhead, char, manyTill, anyChar, (<?>), eof,
    try, digit, many1, spaces, string, choice, parse )
import qualified Text.Parsec as P
import Text.PrettyPrint
  ( Doc, hang, text, sep, vcat, nest, (<+>), ($$),
    parens, brackets, render )


--
-- # Error handling
--

-- | Error handling. Errors are indicated with a Left String;
-- successes with a Right.
type Err = Either String

--
-- # Data types
--

-- | Headers consists of simple @tag:value@ pairs; this represents the
-- tag.
type HeaderTag = String

-- | The value in an OFX header.
type HeaderValue = String

-- | An OFX file starts with a number of headers, which take the form
-- @tag:value@ followed by a newline. These are followed by a blank
-- line.
data OFXHeader = OFXHeader HeaderTag HeaderValue
  deriving (Eq, Show, Read, Data, Generic, Typeable)

-- | The name of an OFX tag
type TagName = String

-- | The data accompanying an OFX tag.
type TagData = String

-- | The main OFX data consists of a series of tags. OFX 1.03 is SGML,
-- not XML. This means that opening tags need not have closing
-- tags. In OFX, a tag either has data and no child elements, or it
-- has no data and it has child elements.
data Tag = Tag TagName (Either TagData [Tag])
  deriving (Eq, Show, Read, Data, Generic, Typeable)

-- | All the data from an OFX file.
data OFXFile = OFXFile
  { fHeader :: [OFXHeader]

  , fTag :: Tag
  -- ^ All the data will be contained in a root tag with the TagName
  -- @OFX@.

  } deriving (Eq, Show, Read, Data, Generic, Typeable)

--
-- # Parsers
--

-- | Parses either a UNIX or an MS-DOS newline. According to 1.2.2,
-- OFX does not contain any white space between tags. However, since I
-- have seen OFX files that do have whitespace between tags, the
-- parser makes allowance for this.
newline :: Parser ()
newline = () <$ char '\n' <|> () <$ (char '\r' *> char '\n')
          <?> "newline"

-- | Parses a character, possibly with an escape sequence. The
-- greater-than sign, less-than sign, and ampersand must be entered
-- with escape sequences.
--
-- According to OFX spec section 2.3.2.1, ampersands, less-than signs,
-- and greater-than signs must appear as entities.  However some banks
-- deliver broken OFX files that do not use entities for ampersands
-- (and possibly for less-than or greater-than signs too, although I
-- have not yet observed such behavior.) There is now an error message
-- that reflects this problem.  Client code can filter the OFX data
-- for known offenders before passing it to this library.
escChar :: Parser Char
escChar =
  do
    c <- anyChar
    case c of
      '&' -> do
        let mkParser (ch, str) = try (ch <$ string str)
        ent <- choice (map mkParser
                [('<', "lt"), ('>', "gt"), ('&', "amp")])
                <?> ( "entity (\"lt\", \"gt\", or \"amp\")\n"
                      ++ "some banks create broken OFX files. Try\n"
                      ++ "removing the ampersands from the file and\n"
                      ++ "trying again.")
        _ <- char ';'
        return ent
      _ -> return c
  <?> "character"

header :: Parser OFXHeader
header
  = OFXHeader
  <$> manyTill anyChar (char ':')
  <*  optional (many (char ' '))
  <*> manyTill anyChar newline
  <?> "OFX header"
  
-- | Parses any opening tag. Returns the name of the tag.
openingTag :: Parser TagName
openingTag =
  do
    _ <- char '<'
    cs <- manyTill escChar (char '>')
    case cs of
      [] -> fail "opening tag with empty name"
      x:_ ->
        if x == '/'
        then fail "this is a closing tag"
        else return cs
  <?> "opening tag"

-- | Parses a closing tag with the given name.
closingTag :: TagName -> Parser ()
closingTag n =
  do
    _ <- char '<'
    _ <- char '/'
    cs <- manyTill escChar (char '>')
    if cs == n
      then return ()
      else fail $ "expecting closing tag named " ++ n
                  ++ "; saw closing tag named " ++ cs
  <?> "closing tag named " ++ n

-- | Parses any tag. The tag itself must be followed by at least one
-- character: either the next tag if this is an aggregate tag, or the
-- data if this is a data tag. OFX does not allow empty tags.
--
-- The OFX spec seems to say that OFX files do not include trailing
-- newlines after tags or data, but I have seen these newlines in QFX
-- files, so this parses optional trailing newlines and spaces.
tag :: Parser Tag
tag =
  do
    -- try is needed because openingTag will overlap with closingTag
    n <- try (openingTag <* spaces)
    children <- many tag
    if null children
      then Tag n . Left
           <$> manyTill escChar
                (eof <|> lookAhead (() <$ char '<') <|> newline)
           <* spaces
           <* optional (try (closingTag n))
           <* spaces
      else Tag n (Right children) <$ spaces <* closingTag n
                                  <* spaces
  <?> "OFX tag"
        

-- | Parses an entire OFX file, including headers.
ofxFile :: Parser OFXFile
ofxFile
  = OFXFile
  <$> manyTill header newline
  <*> tag
  <* spaces
  <* eof
  <?> "OFX file"

-- | Parses an OFX date; provides an error message if the parse fails.
parseDate :: String -> Err T.ZonedTime
parseDate s = case P.parse date "" s of
  Left e -> Left $ "could not parse date: " ++ s ++ ": "
            ++ show e
  Right g -> return g

-- | Parses an OFX date. Fails if the date is not valid or if there is
-- no date to be parsed.
date :: Parser T.ZonedTime
date =
  do
    ys <- fmap read $ replicateM 4 digit
    ms <- fmap read $ replicateM 2 digit
    ds <- fmap read $ replicateM 2 digit
    day <- case T.fromGregorianValid ys ms ds of
      Nothing -> fail $ "invalid date: " ++ show ys
                        ++ "-" ++ show ms ++ "-" ++ show ds
      Just d -> return d
    mayTime <- optional time
    case mayTime of
      Nothing ->
        let localTime = T.LocalTime day T.midnight
        in return $ T.ZonedTime localTime T.utc
      Just (t, z) -> return $ T.ZonedTime (T.LocalTime day t) z
  <?> "date"

  
-- | Parses an OFX time. Fails if the time is not valid or if there is
-- no time to parse. Fails if there is no time to parse; however, if
-- there is a time but no zone, returns the time and UTC for the zone.
time :: Parser (T.TimeOfDay, T.TimeZone)
time =
  do
    h <- fmap read $ replicateM 2 digit
    m <- fmap read $ replicateM 2 digit
    s <- fmap read $ replicateM 2 digit
    (milli, tz) <- do
      mayDot <- optional (char '.')
      case mayDot of
        Nothing -> return (0, T.utc)
        Just _ -> do
          mil <- fmap ((/ 1000) . read) $ replicateM 3 digit
          mayTz <- optional tzOffset
          case mayTz of
            Nothing -> return (mil, T.utc)
            Just t -> return (mil, t)
    let sec = s + milli
    return (T.TimeOfDay h m sec, tz)
  <?> "time"
                

-- | Parses a time zone offset. Fails if there is no time zone offset
-- to parse.
tzOffset :: Parser T.TimeZone
tzOffset =
  do
    _ <- char '['
    sn <- parseSign
    whole <- many1 digit
    mayDot <- optional (char '.')
    frac <- case mayDot of
      Nothing -> return "0"
      Just _ -> many1 digit
    mayColon <- optional (char ':')
    name <- case mayColon of
      Nothing -> return ""
      Just _ -> many1 P.letter
    _ <- char ']'
    let off = sn $ round ((read (whole ++ "." ++ frac))
                                * (60 :: Double))
    return $ T.TimeZone off False name
  <?> "time zone offset"
  where
    parseSign = do
      mayMinus <- optional (char '-')
      case mayMinus of
        Nothing -> do
          mayPlus <- optional (char '+')
          return $ case mayPlus of
            Nothing -> id
            Just _ -> negate
        Just _ -> return negate

--
-- # Manipulating trees of tags
--

-- | Finds child tags with the given name. When a tag is found, that
-- tag is not searched for further children with the same name.
find :: TagName -> Tag -> [Tag]
find n t@(Tag x p)
  | n == x = [t]
  | otherwise = case p of
      Left _ -> []
      Right ts -> concatMap (find n) ts

-- | Descends through a tree of tags to find a tag at a specific
-- location in the tree. Fails if any part of the search fails. For
-- example, to find the financial institution ORG tag, where @t@ is
-- the root @OFX@ tag:
--
-- > findPath ["SIGNONMSGSRSV1", "SONRS", "FI", "ORG"] t

findPath :: [TagName] -> Tag -> Maybe Tag
findPath [] t = Just t
findPath (n:ns) t = case listToMaybe (find n t) of
  Nothing -> Nothing
  Just r -> findPath ns r

-- | Gets the data from a tag, if it is a tag with data.
tagData :: Tag -> Maybe TagData
tagData (Tag _ ei) = either return (const Nothing) ei

-- | Goes to a certain path in the tag hierarchy and pulls the
-- requested data, if the tag is present and it is a data tag.  For
-- example, to get the name of the financial institution:
--
-- > pathData ["SIGNONMSGSRSV1", "SONRS", "FI", "ORG"] f
pathData :: [TagName] -> OFXFile -> Maybe TagData
pathData p (OFXFile _ t) = findPath p t >>= tagData


-- | Gets the name of the financial institution from the FI tag, if
-- available. The OFX spec does not require this tag to be present.
fiName :: OFXFile -> Maybe TagData
fiName = pathData ["SIGNONMSGSRSV1", "SONRS", "FI", "ORG"]


-- | Gets the credit card number, if available. The OFX spec does not
-- require this tag to be present.
creditCardNumber :: OFXFile -> Maybe TagData
creditCardNumber =
  pathData [ "CREDITCARDMSGSRSV1", "CCSTMTTRNRS", "CCSTMTRS",
             "CCACCTFROM", "ACCTID" ]

-- | Gets the bank account number, if available. The OFX spec does not
-- require this tag to be present.
bankAccountNumber :: OFXFile -> Maybe TagData
bankAccountNumber =
  pathData [ "BANKMSGSRSV1", "STMTTRNRS", "STMTRS",
             "BANKACCTFROM", "ACCTID" ]

-- | Gets either the credit card or bank account number, if available.
accountNumber :: OFXFile -> Maybe TagData
accountNumber f = creditCardNumber f <|> bankAccountNumber f
  

-- | Finds the first tag (either this tag or any children) that has
-- the given name and that is a data tag (not an aggregate tag.) If no
-- data tag with the given name is found, returns Nothing.
findData :: TagName -> Tag -> Maybe TagData
findData n (Tag tn e) = case e of
  Left d -> if tn == n then Just d else Nothing
  Right ts -> M.getFirst . M.mconcat .  map M.First
              . map (findData n) $ ts


-- | Finds the first tag (either this tag or any children) that has
-- the given name and that is a data tag. Gives an error message if
-- the tag is not found.
required :: TagName -> Tag -> Err TagData
required n t = case findData n t of
  Nothing -> Left $ "required tag missing: " ++ n
  Just r -> return r


--
-- # OFX data
-- 

-- | OFX transaction types. These are used in STMTTRN aggregates, see
-- OFX spec section 11.4.2.3.1.1.
data TrnType
  = TCREDIT
  | TDEBIT

  | TINT
  -- ^ Interest earned or paid (which it is depends on sign of amount)

  | TDIV
  -- ^ Dividend

  | TFEE
  | TSRVCHG

  | TDEP
  -- ^ Deposit

  | TATM
  -- ^ ATM debit or credit (which it is depends on sign of amount)

  | TPOS
  -- ^ Point of sale debit or credit (which it is depends on sign of
  -- amount)

  | TXFER
  -- ^ Transfer

  | TCHECK
  | TPAYMENT
  -- ^ Electronic payment

  | TCASH
  -- ^ Cash withdrawal

  | TDIRECTDEP
  -- ^ Direct deposit

  | TDIRECTDEBIT
  -- ^ Merchant initiated debit

  | TREPEATPMT
  -- ^ Repeating payment / standing order

  | TOTHER
  deriving (Eq, Ord, Show, Read, Data, Generic, Typeable)

-- | A single STMTTRN, see OFX spec section 11.4.2.3.1. This is most
-- likely what you are interested in after downloading a statement
-- from a bank.
data Transaction = Transaction
  { txTRNTYPE :: TrnType
    -- ^ Transaction type

  , txDTPOSTED :: T.ZonedTime
    -- ^ Date transaction was posted to account

  , txDTUSER :: Maybe T.ZonedTime
    -- ^ Date user initiated transaction, if known

  , txDTAVAIL :: Maybe T.ZonedTime
    -- ^ Date funds are available

  , txTRNAMT :: String
    -- ^ Amount of transaction. This is left as the string that was
    -- originally in the download. That means the transaction may
    -- include a plus or minus sign (no sign is the same as a plus
    -- sign.) According to section 3.2.9.2, amounts are always signed
    -- from the perspective of the customer.
    --
    -- Typically negative amounts:
    --
    -- * Investment buy amount, investment sell quantity
    --
    -- * Bank statement debit amounts, checks, fees
    --
    -- * Credit card purchases
    --
    -- * Margin balance (unless the institution owes the client money)
    --
    -- Typically positive amounts:
    --
    -- * Investment sell amount, investment buy quantity
    --
    -- * Bank statement credits
    --
    -- * Credit card payments
    --
    -- * Ledger balance (unless the account is overdrawn)
    --
    -- Formats for amounts are described in 3.2.9.1. If there is no
    -- decimal point, there is an implied decimal point at the end of
    -- the value. Trailing and leading spaces \"should\" be
    -- stripped. Positive or minus is indicated with a leading sign; a
    -- plus sign is assumed if there is no sign.
    --
    -- An amount has a maximum of 32 alphanumeric characters,
    -- including digits and punctuation.
    --
    -- The radix point is indicated with either a period or a
    -- comma. Amounts \"should\" not include any digit grouping
    -- characters.

    , txFITID :: String
    -- ^ Transaction ID issued by financial institution. Used to
    -- detect duplicate downloads.

    , txCORRECTFITID :: Maybe String
    -- ^ If present, this indicates the FITID of a previously sent
    -- transaction that is corrected by this record. This transaction
    -- replaces or deletes the transaction that it corrects, based on
    -- the value of CORRECTACTION below.

    , txCORRECTACTION :: Maybe CorrectAction
    -- ^ See 'CorrectAction' and 'txCORRECTFITID'

    , txSRVRTID :: Maybe String
    -- ^ Server assigned transaction ID; used for transactions
    -- initiated by client, such as payment or funds transfer

    , txCHECKNUM :: Maybe String
    -- ^ Check or other reference number

    , txREFNUM :: Maybe String
    -- ^ Reference number that uniquely identifies the
    -- transaction. Can be used in addition to or instead of a
    -- CHECKNUM.

    , txSIC :: Maybe String
    -- ^ Standard Industrial Code

    , txPAYEEID :: Maybe String
    -- ^ Payee identifier if available

    , txPayeeInfo :: Maybe (Either String Payee)
    -- ^ Information on the payee. The OFX spec seems to be saying
    -- that every transaction must have either NAME, wich is \"name of
    -- payee or description of transaction\", or the Payee
    -- aggregate. The former is indicated with a Left, the latter with
    -- a Right.

    , txAccountTo :: Maybe (Either BankAcctTo CCAcctTo)
    -- ^ Information on a transfer. If this transaction wa sa
    -- transfer, this may contain information about the account being
    -- transferred to.

    , txMEMO :: Maybe String
    -- ^ Extra information not in NAME

    , txCurrency :: Maybe (Either Currency OrigCurrency)
    -- ^ Currency option. OFX spec says to choose either CURRENCY or
    -- ORIGCURRENCY.
    } deriving (Show, Read, Data, Generic, Typeable)

data Payee = Payee
  { peNAME :: String
  , peADDR1 :: String
  , peADDR2 :: Maybe String
  , peADDR3 :: Maybe String
  , peCITY :: String
  , peSTATE :: String
  , pePOSTALCODE :: String
  , peCOUNTRY :: Maybe String
  , pePHONE :: String
  } deriving (Eq, Show, Read, Data, Generic, Typeable)

-- | Can be either REPLACE or DELETE.
data CorrectAction
  = REPLACE
  -- ^ Replaces the transaction referenced by the CORRECTFITID

  | DELETE
  -- ^ Deletes the transaction referenced by the CORRECTFITID
  deriving (Eq, Show, Read, Data, Generic, Typeable)

data BankAcctTo = BankAcctTo
  { btBANKID :: String
  -- ^ Routing and transit number

  , btBRANCHID :: Maybe String
  -- ^ Bank identifier for international banks

  , btACCTID :: String
  -- ^ Account number

  , btACCTTYPE :: AcctType
  -- ^ Type of account

  , btACCTKEY :: Maybe String
  -- ^ Checksum for international banks
  } deriving (Show, Read, Data, Generic, Typeable)

data CCAcctTo = CCAcctTo
  { ctACCTID :: String
  -- ^ Account number

  , ctACCTKEY :: Maybe String
  -- ^ Checksum for international banks

  } deriving (Eq, Show, Read, Data, Generic, Typeable)

data AcctType
  = ACHECKING
  | ASAVINGS
  | AMONEYMRKT
  | ACREDITLINE
  deriving (Eq, Show, Ord, Read, Data, Generic, Typeable)

acctType :: String -> Err AcctType
acctType s
  | s == "CHECKING" = return ACHECKING
  | s == "SAVINGS" = return ASAVINGS
  | s == "MONEYMRKT" = return AMONEYMRKT
  | s == "CREDITLINE" = return ACREDITLINE
  | otherwise = Left $ "unrecognized account type: " ++ s

-- | Holds all data both for CURRENCY and for ORIGCURRENCY.
data CurrencyData = CurrencyData

  { cdCURRATE :: String
  -- ^ Ratio of CURDEF currency to CURSYM currency, in decimal form

  , cdCURSYM :: String
  -- ^ ISO-4217 3-letter currency identifier
  } deriving (Eq, Show, Read, Data, Generic, Typeable)

data Currency = Currency CurrencyData
  deriving (Eq, Show, Read, Data, Generic, Typeable)

data OrigCurrency = OrigCurrency CurrencyData
  deriving (Eq, Show, Read, Data, Generic, Typeable)

--
-- # Helpers to build aggregates
--
trnType :: TagData -> Maybe TrnType
trnType d = case d of
  "CREDIT" -> Just TCREDIT
  "DEBIT" -> Just TDEBIT
  "INT" -> Just TINT
  "DIV" -> Just TDIV
  "FEE" -> Just TFEE
  "SRVCHG" -> Just TSRVCHG
  "DEP" -> Just TDEP
  "ATM" -> Just TATM
  "POS" -> Just TPOS
  "XFER" -> Just TXFER
  "CHECK" -> Just TCHECK
  "PAYMENT" -> Just TPAYMENT
  "CASH" -> Just TCASH
  "DIRECTDEP" -> Just TDIRECTDEP
  "DIRECTDEBIT" -> Just TDIRECTDEBIT
  "REPEATPMT" -> Just TREPEATPMT
  "OTHER" -> Just TOTHER
  _ -> Nothing


-- | Gets a single Transaction from a tag. The tag should be the one
-- named STMTTRN. Fails with an error message if any required field
-- was not present.
transaction :: Tag -> Err Transaction
transaction t = do
  let fromMaybe e = maybe (Left e) Right
  trntyStr <- required "TRNTYPE" t
  trnTy <- fromMaybe ("could not parse transaction type: " ++ trntyStr)
           $ trnType trntyStr

  dtpStr <- required "DTPOSTED" t
  dtp <- parseDate dtpStr

  let mayDtuStr = findData "DTUSER" t
  dtu <- maybe (return Nothing) (fmap Just . parseDate) mayDtuStr
      
  let mayDtAvail = findData "DTAVAIL" t

  dta <- maybe (return Nothing) (fmap Just . parseDate) mayDtAvail
  amt <- required "TRNAMT" t
  fitid <- required "FITID" t
  let correctFitId = findData "CORRECTFITID" t
  correctAct <-
    case findData "CORRECTACTION" t of
      Nothing -> return Nothing
      Just d -> 
        maybe (return Nothing)
          (fromMaybe ("could not parse correct action: " ++ d))
        . safeRead
        $ d
  let srvrtid = findData "SRVRTID" t
      checknum = findData "CHECKNUM" t
      refnum = findData "REFNUM" t
      sic = findData "SIC" t
      payeeId = findData "PAYEEID" t

  let mayPyeInfo = fmap (return . Left) (findData "NAME" t)
                   <|> fmap (fmap Right) (payee t)
  pyeInfo <- maybe (return Nothing) (fmap Just) mayPyeInfo
 
  let mayAcctTo = (fmap (fmap Left) $ bankAcctTo t)
               <|> (fmap (fmap Right) $ ccAcctTo t)
      mayCcy = (fmap (fmap Left) $ currency t)
            <|> (fmap (fmap Right) $ origCurrency t)
  acctTo <- maybe (return Nothing) (fmap Just) mayAcctTo
  ccy <- maybe (return Nothing) (fmap Just) mayCcy
  let memo = findData "MEMO" t

  return Transaction
    { txTRNTYPE = trnTy
    , txDTPOSTED = dtp
    , txDTUSER = dtu
    , txDTAVAIL = dta
    , txTRNAMT = amt
    , txFITID = fitid
    , txCORRECTFITID = correctFitId
    , txCORRECTACTION = correctAct
    , txSRVRTID = srvrtid
    , txCHECKNUM = checknum
    , txREFNUM = refnum
    , txSIC = sic
    , txPAYEEID = payeeId
    , txPayeeInfo = pyeInfo
    , txAccountTo = acctTo
    , txMEMO = memo
    , txCurrency = ccy
    }      

-- | Parses a Payee record from its parent tag.
payee
  :: Tag
  -- ^ The tag which contains the PAYEE tag, if there is one. This
  -- would typically be a STMTTRN tag.

  -> Maybe (Err Payee)
  -- ^ Nothing if there is no PAYEE tag. Just if a PAYEE tag is found,
  -- with a Left if the tag is lacking a required element, or a
  -- Right if the tag is successfully parsed.
  --
  -- If there is more than one PAYEE tag, only the first one is
  -- considered.
payee = fmap getPayee . listToMaybe . find "PAYEE"
  where
    getPayee t = Payee
      <$> required "NAME" t
      <*> required "ADDR1" t
      <*> pure (findData "ADDR2" t)
      <*> pure (findData "ADDR3" t)
      <*> required "CITY" t
      <*> required "STATE" t
      <*> required "POSTALCODE" t
      <*> pure (findData "COUNTRY" t)
      <*> required "PHONE" t
  

currency :: Tag -> Maybe (Err Currency)
currency
  = fmap (fmap Currency)
  . fmap currencyData
  . listToMaybe
  . find "CURRENCY"

origCurrency :: Tag -> Maybe (Err OrigCurrency)
origCurrency
  = fmap (fmap OrigCurrency)
  . fmap currencyData
  . listToMaybe
  . find "ORIGCURRENCY"


-- | Parses currency data.
currencyData
  :: Tag
  -- ^ The tag that contains the data, e.g. CURRENCY or ORIGCURRENCY.

  -> Err CurrencyData
currencyData t = CurrencyData
  <$> required "CURRATE" t
  <*> required "CURSYM" t

bankAcctTo :: Tag -> Maybe (Err BankAcctTo)
bankAcctTo = fmap getTo . listToMaybe . find "BANKACCTTO"
  where
    getTo t = BankAcctTo
      <$> required "BANKID" t
      <*> pure (findData "BRANCHID" t)
      <*> required "ACCTID" t
      <*> (required "ACCTTYPE" t >>= acctType)
      <*> pure (findData "ACCTKEY" t)

ccAcctTo :: Tag -> Maybe (Err CCAcctTo)
ccAcctTo = fmap getTo . listToMaybe . find "CCACCTTO"
  where
    getTo t = CCAcctTo
      <$> required "ACCTID" t
      <*> pure (findData "ACCTKEY" t)

safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  (x, ""):[] -> Just x
  _ -> Nothing


-- | Pulls all Transactions from a file. Might fail if the OFX file
-- does not conform to the specification (or if there are bugs in this
-- library.) In case of the former, you can manually parse the
-- transaction information yourself using functions like
-- 'pathData'. In case of the latter, please send bugreports :-)
transactions :: OFXFile -> Err [Transaction]
transactions = mapM transaction . find "STMTTRN" . fTag

--
-- # Pretty printers
--
pPayee :: Payee -> Doc
pPayee p = hang "Payee:" 2 ls
  where
    ls = sep [ label "Name" (text . peNAME $ p)
             , label "Addr1" (text . peADDR1 $ p)
             , label "Addr2" (pMaybe text . peADDR2 $ p)
             , label "Addr3" (pMaybe text . peADDR3 $ p)
             , label "City" (text . peCITY $ p)
             , label "State" (text . peSTATE $ p)
             , label "Postal" (text . pePOSTALCODE $ p)
             , label "Country" (pMaybe text . peCOUNTRY $ p)
             , label "Phone" (text . pePHONE $ p)
             ]

pTransaction :: Transaction -> Doc
pTransaction a = hang "Transaction:" 2 ls
  where
    ls = sep [ label "TRNTYPE" (text . show . txTRNTYPE $ a)
             , label "DTPOSTED" (text . show . txDTPOSTED $ a)
             , label "DTUSER" (text . show . txDTUSER $ a)
             , label "DTAVAIL" (text . show . txDTAVAIL $ a)
             , label "TRNAMT" (text . txTRNAMT $ a)
             , label "FITID" (text . txFITID $ a)
             , label "CORRECTFITID"
               (pMaybe text . txCORRECTFITID $ a)
             , label "CORRECTACTION"
               (text . show . txCORRECTACTION $ a)
             , label "SRVRTID" (pMaybe text . txSRVRTID $ a)
             , label "CHECKNUM" (pMaybe text . txCHECKNUM $ a)
             , label "REFNUM" (pMaybe text . txREFNUM $ a)
             , label "SIC" (pMaybe text . txSIC $ a)
             , label "PAYEEID" (pMaybe text . txPAYEEID $ a)
             , label "PAYEEINFO"
               (pMaybe (pEither text (text . show)) . txPayeeInfo $ a)
             , label "ACCOUNTTO"
               (pMaybe id . fmap (text . show)
                          . txAccountTo $ a)
             , label "MEMO" (pMaybe text . txMEMO $ a)
             , label "CURRENCY"
               (pMaybe (text . show) . txCurrency $ a)
             ]

pTag :: Tag -> Doc
pTag (Tag n ei) = case ei of
  Left d -> "<" <> text n <> ">" <> text d
  Right ts -> vcat $ "<" <> text n <> ">"
                   : map (nest 2 . pTag) ts
                   ++ ["</" <> text n <> ">"]

pHeader :: OFXHeader -> Doc
pHeader (OFXHeader t v) = text t <> ": " <> text v

pFile :: OFXFile -> Doc
pFile (OFXFile hs t)
  = "OFX file:"
  $$ nest 2 (vcat [ pList . map pHeader $ hs
                   , mempty
                   , pTag t ])

pEither :: (a -> Doc) -> (b -> Doc) -> Either a b -> Doc
pEither fa fb = either (\l -> "Left" <+> parens (fa l))
                       (\r -> "Right" <+> parens (fb r))

pMaybe :: (a -> Doc) -> Maybe a -> Doc
pMaybe f = maybe "Nothing" (\x -> "Just" <+> parens (f x))

pList :: [Doc] -> Doc
pList ds = case ds of
  [] -> "[]"
  x:[] -> brackets x
  x:xs -> sep $ hang "[" 2 x
              : map (\d -> hang "," 2 d) xs
              ++ [ "]" ]

label :: String -> Doc -> Doc
label s = hang (text (s ++ ":")) (length s + 2)

pExceptional
  :: (e -> Doc)
  -> (a -> Doc)
  -> Either e a
  -> Doc
pExceptional fe fa =
  either (\e -> hang "Exception:" 2 $ parens (fe e))
         (\g -> hang "Success:" 2 $ parens (fa g))

-- # Running Parsers

-- | Parses an input file.  Returns either an error message or the
-- resulting 'OFXFile'.
parseOfxFile :: String -> Err OFXFile
parseOfxFile = either (Left . show) (Right . id) . parse ofxFile ""

-- | Parses an OFX file and gets the list of 'Transaction'.
parseTransactions :: String -> Err [Transaction]
parseTransactions = transactions <=< parseOfxFile

-- | Loads an OFX file from disk, parses it, and returns the
-- resulting 'OFXFile'.  Uses 'fail' if the parse fails.
loadOfxFile :: FilePath -> IO OFXFile
loadOfxFile fp = do
  txt <- readFile fp
  case parseOfxFile txt of
    Left e -> fail e
    Right g -> return g

-- | Loads an OFX file from disk, parses it, and returns the resulting list of
-- 'Transaction'.  Uses 'fail' if the parse fails.
loadTransactions :: FilePath -> IO [Transaction]
loadTransactions fp = do
  txt <- readFile fp
  case parseTransactions txt of
    Left e -> fail e
    Right g -> return g

-- # Parsing and pretty printing

-- | Parses an input file to an OfxFile.  Returns a pretty-printed
-- string with the results of the parse.
prettyRenderOfxFile
  :: String
  -- ^ File contents to parse
  -> String
  -- ^ Pretty printed result of rending the result of the parse, which
  -- is either an error message or an 'OFXFile'.
prettyRenderOfxFile
  = render
  . pExceptional text pFile
  . parseOfxFile

-- | Parses an input file to an OfxFile, and then to a list of
-- 'Transaction'.  Returns a pretty-printed string with the results.
prettyRenderTransactions
  :: String
  -- ^ File contents to parse
  -> String
  -- ^ Pretty printed result of rendering the result of the parse,
  -- which is either an error message or a list of 'Transaction'.
prettyRenderTransactions
  = render
  . pExceptional text (pList . map pTransaction)
  . parseTransactions
