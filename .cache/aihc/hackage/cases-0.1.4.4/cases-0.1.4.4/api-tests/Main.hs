module Main (main) where

import qualified Cases
import Test.Hspec
import Prelude

main :: IO ()
main = hspec do
  it "spinalizeCamelCase" do
    shouldBe "abc-def" $ Cases.spinalize "abcDef"
    shouldBe "abc-def" $ Cases.spinalize "abcDEF"
    shouldBe "my-html-processor" $ Cases.spinalize "myHTMLProcessor"

  it "spinalizeSymbols" do
    shouldBe "abc-def" $ Cases.spinalize "abc_def"
    shouldBe "abc-def" $ Cases.spinalize "abc-def"
    shouldBe "abc-def" $ Cases.spinalize "abc/def"
    shouldBe "abc-def" $ Cases.spinalize "abc,def"
    shouldBe "abc-def" $ Cases.spinalize "abc.def"
    shouldBe "abc-def" $ Cases.spinalize "abc:def"

  it "spinalizeMultipleDelimiters" do
    shouldBe "abc-def" $ Cases.spinalize "abc_-:,/def"
    shouldBe "abc-def" $ Cases.spinalize "abc /-def"

  it "spinalizeSpaces" do
    shouldBe "abc-def" $ Cases.spinalize "abc def"
    shouldBe "abc-def" $ Cases.spinalize "abc\ndef"
    shouldBe "abc-def" $ Cases.spinalize "abc\rdef"
    shouldBe "abc-def" $ Cases.spinalize "abc\r\n\rdef"
    shouldBe "abc-def" $ Cases.spinalize "abc\tdef"
    shouldBe "abc-def" $ Cases.spinalize "abc  def"

  it "spinalizeTrimming" do
    shouldBe "abc-def" $ Cases.spinalize " abc def "
    shouldBe "abc-def" $ Cases.spinalize "/abc/def/"

  it "spinalizeNumbers" do
    shouldBe "abc-12" $ Cases.spinalize "abc12"

  it "spinalizeUnicode" do
    shouldBe "ёжик-лижет-мёд" $ Cases.spinalize "Ёжик лижет мёд."

  it "snakify" do
    shouldBe "ёжик_лижет_мёд" $ Cases.snakify "Ёжик лижет мёд."

  it "camelize" do
    shouldBe "abcDef" $ Cases.camelize "abc-def"
    shouldBe "AbcDef" $ Cases.camelize "Abc-def"
    shouldBe "parseDbmXml" $ Cases.camelize "parse DBM XML"
