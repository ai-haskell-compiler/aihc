#!/bin/sh

echo "Testing C ... "
gcc sum.c -o sum-c -I../include ../dist/build/cbits/SFMT.o
/usr/bin/time ./sum-c
