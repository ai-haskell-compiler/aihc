{-# LANGUAGE FlexibleInstances, UndecidableInstances, DefaultSignatures #-}

-- | Format string with named args
--
-- >-- Named args
-- >"My name is {name}, I am {age} years old" ~~ ("name" ~% "Joe") ~~ ("age" ~% 24) ≡ "My name is Joe, I am 24 years old"
-- >-- Arg can have default value
-- >"{var:x} = {val:10}" ~~ ("var" ~% y) ≡ "y = 10"
-- >-- Numeric position can be used
-- >"{0} {1} {0}" ~~ "foo" ~~ "bar" ≡ "foo bar foo"
-- >-- Positions can be omitted
-- >"{} {}" ~~ "foo" ~~ 10 ≡ "foo 10"
-- >-- Double braces to escape them
-- >"{} and {{}}" ~~ 10 ≡ "10 and {}"
module Text.Format (
	FormattedPart(..), Formatted(..), withFlags,
	FormatArg(..), Format(..), Formatter(..),
	prebuild, build, getNamedArguments,
	Formattable(..), Hole(..), fmt, FormatResult(..),
	format, formats, (~~), (~%),

	module Text.Format.Flags
	) where

import Prelude.Unicode

import Control.Applicative
import Data.Char (intToDigit)
import Data.List (find, intercalate, nub)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Semigroup (Semigroup(..))
import qualified Data.Text as T
import Data.Text.Lazy (Text, unpack)
import Data.String
import Numeric
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP

import Text.Format.Flags

data FormattedPart = FormattedPart {
	formattedFlags ∷ FormatFlags,
	formattedValue ∷ String }
		deriving (Eq, Ord, Show)

instance IsString FormattedPart where
	fromString = FormattedPart [] ∘ fromString

newtype Formatted = Formatted { formattedParts ∷ [FormattedPart] } deriving (Eq, Ord, Show)

instance IsString Formatted where
	fromString = Formatted ∘ return ∘ fromString

instance Semigroup Formatted where
	Formatted l <> Formatted r = Formatted $ l ++ r

instance Monoid Formatted where
	mempty = Formatted []
	Formatted l `mappend` Formatted r = Formatted $ l ++ r

withFlags ∷ String → [String] → Formatted
withFlags v fs = Formatted [FormattedPart fs v]

data FormatArg = FormatNamed String ([String] → Formatted) | FormatPos ([String] → Formatted)

data Format = Format {
	formatString ∷ String,
	formatArgs ∷ [FormatArg] }

instance Show Format where
	show = mconcat ∘ map formattedValue ∘ formattedParts ∘ prebuild

instance IsString Format where
	fromString str = Format str []

data Formatter = Formatter {
	formatter ∷ Either String Int,
	formatterDefault ∷ Maybe String,
	formatterFlags ∷ [String] }

instance Show Formatter where
	show (Formatter f def cfgs) = "{" ++ concat parts ++ "}" where
		parts = [either id show f, fromMaybe "" (fmap ('=':) def), if null cfgs then "" else (':' : intercalate "," cfgs)]

instance Read Formatter where
	readsPrec _ = readP_to_S $ between (char '{') (char '}') $ do
		n ← munch (∉ "=:}")
		v ← option Nothing $ do
			_ ← char '='
			v' ← munch1 (∉ ":}")
			return $ Just v'
		cs ← option [] $ do
			_ ← char ':'
			flip sepBy (char ',') (munch1 (∉ ",}"))
		return $ Formatter (maybe (Left n) Right $ readMaybe n) v cs

prebuild ∷ Format → Formatted
prebuild = buildFormat True

build ∷ Format → Formatted
build = buildFormat False

buildFormat ∷ Bool → Format → Formatted
buildFormat pre fstr = build' 0 fstr where
	build' ∷ Int → Format → Formatted
	build' _ (Format "" _) = mempty
	build' i (Format ('{':'{':fstr') args) = fromString "{" `mappend` build' i (Format fstr' args)
	build' i (Format ('}':'}':fstr') args) = fromString "}" `mappend` build' i (Format fstr' args)
	build' i (Format ('{':'}':fstr') args) = formatArg' (Formatter (Right i) Nothing []) args `mappend` build' (succ i) (Format fstr' args)
	build' i (Format ('{':fstr') args) = case reads ('{':fstr') of
		[] → error $ "Can't parse formatter at " ++ fstr'
		(f, fstr''):_ → formatArg' f args `mappend` build' i (Format fstr'' args)
	build' i (Format fstr' args) = fromString s `mappend` build' i (Format fstr'' args) where
		(s, fstr'') = break (∈ "{}") fstr'
	formatArg' ∷ Formatter → [FormatArg] → Formatted
	formatArg' f@(Formatter (Left name) defVal fmtCfgs) args
		| pre = fromMaybe (formatted f fmtCfgs) lookArg
		| otherwise = fromMaybe (error $ "Argument " ++ name ++ " not set") (lookArg <|> fmap (flip formatted fmtCfgs) defVal)
		where
			lookArg = do
				FormatNamed _ fval ← find byName args
				return $ fval fmtCfgs
			byName (FormatNamed n _) = n ≡ name
			byName _ = False
	formatArg' f@(Formatter (Right i) defVal fmtCfgs) args
		| pre = fromMaybe (formatted f fmtCfgs) lookIdx
		| otherwise = fromMaybe (error $ "Argument at index " ++ show i ++ " not set") (lookIdx <|> fmap (flip formatted fmtCfgs) defVal)
		where
			lookIdx = do
				FormatPos fval ← listToMaybe $ drop i $ filter isPos args
				return $ fval fmtCfgs
			isPos (FormatPos _) = True
			isPos _ = False

getNamedArguments ∷ Format → [String]
getNamedArguments = nub ∘ getNamedArguments'
	where
		getNamedArguments' ∷ Format → [String]
		getNamedArguments' (Format "" _) = mempty
		getNamedArguments' (Format ('{':'{':fstr') args) = getNamedArguments' (Format fstr' args)
		getNamedArguments' (Format ('}':'}':fstr') args) = getNamedArguments' (Format fstr' args)
		getNamedArguments' (Format ('{':'}':fstr') args) = getNamedArguments' (Format fstr' args)
		getNamedArguments' (Format fstr@('{':fstr') args) = case reads fstr of
			[] → error $ "Can't parse formatter at " ++ fstr'
			(f, fstr''):_ → getNamedArgument f `mappend` getNamedArguments' (Format fstr'' args)
			where
		getNamedArguments' (Format fstr' args) = getNamedArguments' (Format fstr'' args) where
			(_, fstr'') = break (∈ "{}") fstr'
		getNamedArgument ∷ Formatter → [String]
		getNamedArgument (Formatter (Left name) _ _) = [name]
		getNamedArgument _ = mempty

-- | Formattable class, by default using @show@
class Formattable a where
	formattable ∷ a → FormatFlags → Formatted
	default formattable ∷ Show a ⇒ a → FormatFlags → Formatted
	formattable x _ = fromString ∘ show $ x

formatted ∷ Formattable a ⇒ a → FormatFlags → Formatted
formatted v fmts = Formatted ∘ map addFmts ∘ formattedParts ∘ formattable v $ fmts where
	addFmts (FormattedPart flags' v') = FormattedPart (nub $ fmts ++ flags') v'

instance Formattable String where
	formattable s _ = fromString s

instance Formattable Char where
	formattable ch _ = fromString [ch]

instance Formattable Int where
	formattable i fmts = fromString ∘ formatInt (baseFlag fmts) $ i
instance Formattable Integer where
	formattable i fmts = fromString ∘ formatInt (baseFlag fmts) $ i
instance Formattable Double where
	formattable d fmts = fromString ∘ formatDouble (preciseFlag fmts) $ d
instance Formattable Float where
	formattable f fmts = fromString ∘ formatDouble (preciseFlag fmts) $ f
instance Formattable Bool

instance Formattable Text where
	formattable s _ = fromString ∘ unpack $ s

instance Formattable T.Text where
	formattable s _ = fromString ∘ T.unpack $ s

instance Formattable Formatter where
	formattable s _ = fromString ∘ show $ s

class Hole a where
	hole ∷ a → [FormatArg]

instance Hole Formatted where
	hole v = [FormatPos $ const v]

instance {-# OVERLAPPING #-} Hole FormatArg where
	hole = return

instance {-# OVERLAPPING #-} Hole [FormatArg] where
	hole = id

instance {-# OVERLAPPING #-} Hole [[FormatArg]] where
	hole = concat

instance {-# OVERLAPPABLE #-} Formattable a ⇒ Hole a where
	hole v = [FormatPos $ formatted v]

fmt ∷ Formattable a ⇒ a → FormatArg
fmt v = FormatPos $ formatted v

class FormatResult r where
	formatResult ∷ Format → r

instance FormatResult Format where
	formatResult = id

instance {-# OVERLAPPING #-} FormatResult String where
	formatResult = mconcat ∘ map formattedValue ∘ formattedParts ∘ build

instance {-# OVERLAPPABLE #-} IsString s ⇒ FormatResult s where
	formatResult = fromString ∘ formatResult

instance {-# OVERLAPPABLE #-} FormatResult Formatted where
	formatResult = build

format ∷ FormatResult r ⇒ String → r
format = formatResult ∘ fromString

formats ∷ FormatResult r ⇒ String → [FormatArg] → r
formats f = formatResult ∘ Format f

infixl 7 ~~

(~~) ∷ (Hole a, FormatResult r) ⇒ Format → a → r
fstr ~~ arg = formatResult $ fstr { formatArgs = formatArgs fstr ++ hole arg }

infixr 8 ~%

(~%) ∷ Formattable a ⇒ String → a → FormatArg
name ~% value = FormatNamed name (formatted value)

-- * Util

formatInt ∷ (Show a, Integral a) ⇒ a → a → String
formatInt base v = showIntAtBase base intToDigit v ""

formatDouble ∷ RealFloat a ⇒ Maybe Int → a → String
formatDouble p v = showGFloat p v ""
