module Main where
import Crypto.Cipher.Benchmarks
import Crypto.Cipher.RC4
main = defaultMainAll [Stream $ GStreamCipher (undefined :: RC4)]
