
-- |
--
-- NOTE We need to make sure to be compatible to everything here:
-- @http://evolution.genetics.washington.edu/phylip/newicktree.html@ In
-- particular, we do not do any conversion from @_@ to @(space)@ right now.

module Biobase.Newick.Import where

import           Control.Applicative
import           Data.Attoparsec.Text (Parser,(<?>))
import           Data.Char (isAlpha)
import           Data.Text (Text)
import           Data.Tree
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Biobase.Newick.Types



type PNT = Parser (Tree Info)

newicksFromFile :: FilePath -> IO (Either String [NewickTree])
newicksFromFile f = newicksFromText <$> T.readFile f

newicksFromText :: Text -> Either String [NewickTree]
newicksFromText = A.parseOnly manyNewick

manyNewick = some (newick <* A.skipSpace) <* A.endOfInput

newick :: Parser NewickTree
newick = NewickTree <$> tree <* ";" <?> "newick"

tree :: PNT
tree = A.skipSpace *> (branched <|> leaf) <?> "tree"

branched :: PNT
branched = flip Node <$ "(" <*> tree `A.sepBy1` "," <* ")" <*> info <?> "branched"

leaf :: PNT
leaf = (flip Node []) <$> info <?> "leaf"

info :: Parser Info
info = Info <$> name <*> plength <?> "info"

-- |
--
-- NOTE http://evolution.genetics.washington.edu/phylip/newicktree.html
--
-- A name can be any string of printable characters except blanks, colons,
-- semicolons, parentheses, and square brackets.
--
-- I am excluding commas as well.

name :: Parser Text
name = A.takeWhile accept <?> "name"
  where accept a = not $ A.isHorizontalSpace a || A.inClass ",:;()[]" a -- isAlpha a || A.inClass "_." a || A.isHorizontalSpace a

plength :: Parser Double
plength = ":" *> A.double <|> pure 0 <?> "plength"


{-
test = mapM_ (\x -> (either error (putStr . drawTree . fmap show . head . map getNewickTree) $ A.parseOnly manyNewick x) >> putStrLn "\n\n\n")
        [ "B:0.2;"
        , "A;"
        , ";"
        , "(,);"
        , "((raccoon:19.19959,bear:6.80041):0.84600,((sea_lion:11.99700, seal:12.00300):7.52973,((monkey:100.85930,cat:47.14069):20.59201, weasel:18.87953):2.09460):3.87382,dog:25.46154);"
        , "(Bovine:0.69395,(Gibbon:0.36079,(Orang:0.33636,(Gorilla:0.17147,(Chimp:0.19268, Human:0.11927):0.08386):0.06124):0.15057):0.54939,Mouse:1.21460):0.10;"
        , "(Bovine:0.69395,(Hylobates:0.36079,(Pongo:0.33636,(G._Gorilla:0.17147, (P._paniscus:0.19268,H._sapiens:0.11927):0.08386):0.06124):0.15057):0.54939, Rodent:1.21460);"
        , "A;"
        , "((A,B),(C,D));"
        , "(Alpha,Beta,Gamma,Delta,,Epsilon,,,);"
        ]
-}

