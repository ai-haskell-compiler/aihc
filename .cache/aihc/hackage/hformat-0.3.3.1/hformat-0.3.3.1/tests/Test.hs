{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Prelude.Unicode

import Text.Format

import Test.Hspec

main ∷ IO ()
main = hspec $ do
	describe "positional arguments" $ do
		it "should format unnamed arguments"
			(format "{} + {} = {}" ~~ (10 ∷ Int) ~~ (12 ∷ Int) ~~ (22 ∷ Int) ≡ str "10 + 12 = 22")
		it "should format positional arguments"
			(format "{0} + {0} = {1}" ~~ (10 ∷ Int) ~~ (20 ∷ Int) ≡ str "10 + 10 = 20")
	describe "named arguments" $
		it "should format named arguments"
			(format "{x} + {y} = {z}" ~~ "x" ~% (1 ∷ Int) ~~ "y" ~% (2 ∷ Int) ~~ "z" ~% (3 ∷ Int) ≡ str "1 + 2 = 3")
	describe "default values" $ do
		it "should accept default values for positional arguments"
			(format "{0=foo} is {1=bar}" ~~ str "blah" ≡ str "blah is bar")
		it "should accept default values for named arguments"
			(format "{x=12} + {y=13}" ~~ "y" ~% (10 ∷ Int) ≡ str "12 + 10")
	describe "format options" $ do
		it "should accept format options"
			(format "x is {0=foo:octal}" ~~ (10 ∷ Int) ≡ str "x is 12")
	describe "colorized output" $ do
		it "should accept colors"
			(format "x is {0:red}" ~~ (10 ∷ Int) ≡ Formatted [FormattedPart [] "x is ", FormattedPart ["red"] "10"])
	describe "lists" $
		it "should accept list of values"
			(format "{0} + {x:10} = {1}" ~~ [fmt (3 ∷ Int), "x" ~% (5 ∷ Int), fmt (8 ∷ Int)] ≡ str "3 + 5 = 8")
	describe "escape" $
		it "should escape curly braces"
			(format "{} is not {{}}" ~~ str "{}" ≡ str "{} is not {}")
	describe "mix" $
		it "should process mixed arguments"
			(format "{1=foo} and {} are {what=args}" ~~ str "what" ~% str "quux" ~~ (10 ∷ Int) ~~ (20 ∷ Int) ≡ str "20 and 10 are quux")
	describe "prebuild" $
		it "should show partially formatted" $
			show (format "{0} ≡ {1}" ~~ str "foo" ∷ Format) ≡ str "foo ≡ {1}"
	describe "list of named arguments" $
		it "should list the named arguments" $
			getNamedArguments "Positional arguments are ignored: {0} {} {2}. I want just the following, without repetition: {foo}, {bar} and {foo}." ≡ ["foo", "bar"]

str ∷ String → String
str = id
