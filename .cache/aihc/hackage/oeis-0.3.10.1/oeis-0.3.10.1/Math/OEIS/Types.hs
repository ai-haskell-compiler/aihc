module Math.OEIS.Types where

type SequenceData = [Integer]

-- | Programming language that some code to generate the sequence is written
-- in. The only languages indicated natively by the OEIS database are
-- Mathematica and Maple; any other languages will be listed (usually in
-- parentheses) at the beginning of the actual code snippet.
data Language = Mathematica | Maple | Other deriving (Show, Eq)

-- | OEIS keywords. For more information on the meaning of each keyword, see
-- <http://oeis.org/eishelp2.html#RK>.
data Keyword = Base | Bref | Changed | Cofr | Cons | Core | Dead | Dumb | Dupe |
               Easy | Eigen | Fini | Frac | Full | Hard | More | Mult |
               New | Nice | Nonn | Obsc | Sign | Tabf | Tabl | Uned |
               Unkn | Walk | Word
       deriving (Eq,Show,Read)

-- | Data structure for storing an OEIS entry. For more information on the
-- various components, see <http://oeis.org/eishelp2.html>.
data OEISSequence =
  OEIS { catalogNums  :: [String],
         -- ^ Catalog number(s), e.g. A000040, N1425. (%I)
         sequenceData :: SequenceData,
         -- ^ The actual sequence data (or absolute values of the sequence data in the case of signed sequences).  (%S,T,U)
         signedData   :: SequenceData,
         -- ^ Signed sequence data (empty for sequences with all positive entries).  (%V,W,X)
         description  :: String,
         -- ^ Short description of the sequence. (%N)
         references   :: [String],
         -- ^ List of academic references. (%D)
         links        :: [String],
         -- ^ List of links to more information on the web. (%H)
         formulas     :: [String],
         -- ^ Formulas or equations involving the sequence. (%F)
         xrefs        :: [String],
         -- ^ Cross-references to other sequences. (%Y)
         author       :: String,
         -- ^ Author who input the sequence into the database. (%A)
         offset       :: Int,
         -- ^ Subscript\/index of the first term. (%O)
         firstGT1     :: Int,
         -- ^ Index of the first term \> 1.  (%O)
         programs     :: [(Language,String)],
         -- ^ Code that can be used to generate the sequence. (%p,t,o)
         extensions   :: [String],
         -- ^ Corrections, extensions, or edits. (%E)
         examples     :: [String],
         -- ^ Examples. (%e)
         keywords     :: [Keyword],
         -- ^ Keywords. (%K)
         comments     :: [String]
         -- ^ Comments. (%C)
       } deriving Show
