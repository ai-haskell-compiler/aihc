{-# LANGUAGE OverloadedStrings #-}

import qualified System.FilePath as FilePath
import qualified System.FilePath.ByteString as RawFilePath
import Criterion.Main

main :: IO ()
main = defaultMain
	[ bgroup "combine"
		[ bench "FilePath" $ nf (FilePath.combine "foo") "bar"
		, bench "RawFilePath" $ nf (RawFilePath.combine "foo") "bar"
		]
	, bgroup "dropTrailingPathSeparator"
		[ bench "FilePath" $ nf FilePath.dropTrailingPathSeparator "foo/"
		, bench "RawFilePath" $ nf RawFilePath.dropTrailingPathSeparator "foo/"
		]
	, bgroup "FilePath conversion"
		[ bench "encode" $ nf RawFilePath.encodeFilePath "foobar.baz"
		, bench "decode" $ nf RawFilePath.decodeFilePath "foobar.baz"
		]
	]
