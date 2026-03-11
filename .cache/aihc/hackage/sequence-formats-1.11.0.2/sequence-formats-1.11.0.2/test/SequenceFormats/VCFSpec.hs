{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.VCFSpec (spec) where

import           Control.Foldl                    (list, purely)
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import           Pipes                            (each, runEffect, (>->))
import qualified Pipes.Prelude                    as P
import           Pipes.Safe                       (runSafeT)
import           SequenceFormats.FreqSum          (FreqSumEntry (..))
import           SequenceFormats.Utils            (Chrom (..),
                                                   SeqFormatException (..))
import           SequenceFormats.VCF              (VCFentry (..),
                                                   VCFheader (..), getDosages,
                                                   vcfHeaderParser,
                                                   getGenotypes, isBiallelicSnp,
                                                   isTransversionSnp,
                                                   readVCFfromFile,
                                                   vcfToFreqSumEntry,
                                                   writeVCFfile)
import           Test.Hspec

spec :: Spec
spec = do
    testParseVCFheader
    testReadVCFfromFile
    testReadVCFfromFileCompressed
    testGetGenotypes
    testGetDosages
    testIsTransversionSnp
    testVcfToFreqsumEntry
    testIsBiallelicSnp
    testWriteVCF

testParseVCFheader :: Spec
testParseVCFheader = describe "parseVCFheader" $ do
    let htext = "##blabla1\n##blabla2\n#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\n"
    it "should correctly parse a dummy header" $ 
        parseOnly vcfHeaderParser htext `shouldBe` Right (VCFheader ["##blabla1", "##blabla2"] [])

testReadVCFfromFile :: Spec
testReadVCFfromFile = describe "readVCFfromFile" $ do
    (vcfH, vcfRows) <- runIO . runSafeT $ do
        (vcfH_, vcfProd_) <- readVCFfromFile "testDat/example.vcf"
        vcfRows_ <- purely P.fold list vcfProd_
        return (vcfH_, vcfRows_)
    let vcfHc = vcfHeaderComments vcfH
    it "reads the correct header lines" $ do
        vcfHc !! 0 `shouldBe` "##fileformat=VCFv4.2"
        vcfHc !! 18 `shouldBe` "##bcftools_callCommand=call -c -v"
    it "reads the correct sample names" $
        vcfSampleNames vcfH `shouldBe` ["12880A", "12881A", "12883A", "12884A", "12885A"]
    it "reads the correct vcf genotype rows" $ do
        vcfRows !! 0 `shouldBe` vcf1
        vcfRows !! 6 `shouldBe` vcf7

testReadVCFfromFileCompressed :: Spec
testReadVCFfromFileCompressed = describe "readVCFfromFile with gzip" $ do
    (vcfH, vcfRows) <- runIO . runSafeT $ do
        (vcfH_, vcfProd_) <- readVCFfromFile "testDat/example.vcf.gz"
        vcfRows_ <- purely P.fold list vcfProd_
        return (vcfH_, vcfRows_)
    let vcfHc = vcfHeaderComments vcfH
    it "reads the correct header lines" $ do
        vcfHc !! 0 `shouldBe` "##fileformat=VCFv4.2"
        vcfHc !! 18 `shouldBe` "##bcftools_callCommand=call -c -v"
    it "reads the correct sample names" $
        vcfSampleNames vcfH `shouldBe` ["12880A", "12881A", "12883A", "12884A", "12885A"]
    it "reads the correct vcf genotype rows" $ do
        vcfRows !! 0 `shouldBe` vcf1
        vcfRows !! 6 `shouldBe` vcf7

vcf1 :: VCFentry
vcf1 =
    let gfields = Just (["GT", "PL"], [["0/0", "0,3,37"], ["0/0", "0,6,67"], ["0/1", "51,0,28"], ["0/0", "0,54,255"], ["0/0", "0,9,83"]])
    in  VCFentry (Chrom "1") 10492 (Just "testId") "C" ["T"] (Just 15.0302) Nothing ["DP=28", "PV4=1,1,0.30985,1"] gfields

vcf1bad :: VCFentry
vcf1bad =
    let gfields = Just (["PL"], [["0/0", "0,3,37"], ["0/0", "0,6,67"], ["0/1", "51,0,28"], ["0/0", "0,54,255"], ["0/0", "0,9,83"]])
    in  VCFentry (Chrom "1") 10492 (Just "testId") "C" ["T"] (Just 15.0302) Nothing ["DP=28", "PV4=1,1,0.30985,1"] gfields

vcf7 :: VCFentry
vcf7 =
    let gfields = Just (["GT", "PL"], [["1/1", "0,0,0"], ["1/1", "0,0,0"], ["1/1", "40,6,0"], ["1/1", "105,9,0"], ["1/1", "0,0,0"]])
    in  VCFentry (Chrom "2") 30923 Nothing "G" [] Nothing Nothing ["DP=5", "FQ=-28.9619"] gfields

testGetGenotypes :: Spec
testGetGenotypes = describe "getGenotypes" $ do
    it "should successfully read genotypes if GT format field is there" $
        getGenotypes vcf1 `shouldReturn` ["0/0", "0/0", "0/1", "0/0", "0/0"]
    it "should yield Left err if GT format field isn't found" $
        getGenotypes vcf1bad `shouldThrow` (== SeqFormatException "GT format field not found")

testGetDosages :: Spec
testGetDosages = describe "getDosages" $ do
    it "should read correct dosages" $ do
        getDosages vcf1 `shouldReturn` [Just (0, 2), Just (0, 2), Just (1, 2), Just (0, 2), Just (0, 2)]
        let vcf1' = vcf1 {vcfGenotypeInfo = Just (["GT", "PL"], [
                ["0/0", "0,3,37"], ["0/0", "0,6,67"], [".", "51,0,28"], ["1", "0,54,255"],
                ["0/0", "0,9,83"]])}
        getDosages vcf1' `shouldReturn` [Just (0, 2), Just (0, 2), Nothing, Just (1, 1), Just (0, 2)]

testIsTransversionSnp :: Spec
testIsTransversionSnp = describe "isTransversionSnp" $ do
    it "should reject triAllelic SNPs" $
        isTransversionSnp "A" ["C", "T"] `shouldBe` False
    it "should reject transitions" $ do
        isTransversionSnp "A" ["G"] `shouldBe` False
        isTransversionSnp "T" ["C"] `shouldBe` False
    it "should accept transversions" $ do
        isTransversionSnp "T" ["A"] `shouldBe` True
        isTransversionSnp "C" ["G"] `shouldBe` True

testVcfToFreqsumEntry :: Spec
testVcfToFreqsumEntry = describe "vcfToFreqsumEntry" $
    it "should convert correctly" $ do
        let r = FreqSumEntry (Chrom "1") 10492 (Just "testId") Nothing 'C' 'T' [Just (0, 2), Just (0, 2), Just (1, 2), Just (0, 2), Just (0, 2)]
        vcfToFreqSumEntry vcf1 `shouldReturn` r

testIsBiallelicSnp :: Spec
testIsBiallelicSnp = describe "isBiallelicSnp" $ do
    it "should reject triAllelic" $
        isBiallelicSnp "A" ["C", "T"] `shouldBe` False
    it "should accept biallelic" $
        isBiallelicSnp "A" ["T"] `shouldBe` True

vcfHeader :: VCFheader
vcfHeader =
    let commentLines = [
            "##fileformat=VCFv4.2",
            "##FILTER=<ID=PASS,Description=\"All filters passed\">",
            "##samtoolsVersion=1.3+htslib-1.3",
            "##samtoolsCommand=samtools mpileup -vI -f /projects1/Reference_Genomes/Human/hs37d5/hs37d5.fa -r 1:1-200000 12880A.bam 12881A.bam 12883A.bam 12884A.bam 12885A.bam",
            "##reference=file:///projects1/Reference_Genomes/Human/hs37d5/hs37d5.fa",
            "##contig=<ID=1,length=249250621>",
            "##contig=<ID=2,length=243199373>",
            "##contig=<ID=3,length=198022430>",
            "##contig=<ID=4,length=191154276>",
            "##contig=<ID=5,length=180915260>",
            "##contig=<ID=hs37d5,length=35477943>",
            "##ALT=<ID=*,Description=\"Represents allele(s) other than observed.\">",
            "##INFO=<ID=INDEL,Number=0,Type=Flag,Description=\"Indicates that the variant is an INDEL.\">",
            "##FORMAT=<ID=PL,Number=G,Type=Integer,Description=\"List of Phred-scaled genotype likelihoods\">",
            "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
            "##INFO=<ID=AF1,Number=1,Type=Float,Description=\"Max-likelihood estimate of the first ALT allele frequency (assuming HWE)\">",
            "##INFO=<ID=DP4,Number=4,Type=Integer,Description=\"Number of high-quality ref-forward , ref-reverse, alt-forward and alt-reverse bases\">",
            "##bcftools_callVersion=1.3+htslib-1.3",
            "##bcftools_callCommand=call -c -v"]
        sampleNames = ["12880A", "12881A", "12883A", "12884A", "12885A"]
    in  VCFheader commentLines sampleNames

testWriteVCF :: Spec
testWriteVCF = describe "writeVCF" $ do
    let tmpVCF = "/tmp/vcfWriteTest.vcf"
        testDatVCFprod = each [vcf1, vcf7]
        cons = writeVCFfile tmpVCF vcfHeader
    runIO . runSafeT . runEffect $ testDatVCFprod >-> cons
    (vcfH, vcfRows) <- runIO . runSafeT $ do
        (vcfH_, vcfProd_) <- readVCFfromFile tmpVCF
        vcfRows_ <- purely P.fold list vcfProd_
        return (vcfH_, vcfRows_)
    it "correctly write and reads back VCF data" $ do
        let vcfHc = vcfHeaderComments vcfH
        vcfHc !! 0 `shouldBe` "##fileformat=VCFv4.2"
        vcfHc !! 18 `shouldBe` "##bcftools_callCommand=call -c -v"
        vcfSampleNames vcfH `shouldBe` ["12880A", "12881A", "12883A", "12884A", "12885A"]
        vcfRows !! 0 `shouldBe` vcf1
        vcfRows !! 1 `shouldBe` vcf7

