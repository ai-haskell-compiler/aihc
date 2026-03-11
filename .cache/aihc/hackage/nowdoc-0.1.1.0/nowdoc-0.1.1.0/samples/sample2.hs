{-# LANGUAGE QuasiQuotes #-}

import Text.Nowdoc

main :: IO ()
main = putStr [nowdoc|
#include <stdio.h>

int
main(int argc, char *argv[])
{
	printf("Hello, world!\n");
	return 0;
}
|]
