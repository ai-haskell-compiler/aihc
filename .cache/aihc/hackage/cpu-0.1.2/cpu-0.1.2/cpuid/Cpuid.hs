module Main where

import System.Environment
import System.Cpuid
import Text.Printf

usage = do
    error "usage: cpuid <eax> [ecx]"

printResult (eax,ebx,ecx,edx) = do
    printf "%.8x %.8x %.8x %.8x\n" eax ebx ecx edx

main = do
    args <- getArgs
    case args of
        []        -> usage
        eax:[]    -> cpuid (read eax) >>= printResult
        eax:ecx:_ -> cpuidWithIndex (read eax) (read ecx) >>= printResult
