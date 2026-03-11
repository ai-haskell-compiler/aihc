#!/bin/sh

time_run () {
    arg=$1
    shift
    for e in $* ; do
	time ../dist/build/$e/$e $arg > $e.out
    done
    diffn $*
}

diffn () {
    ref=$1
    ret=0
    shift
    for f in $* ; do
	diff $ref.out $f.out
	ret=$(( $ret + $?))
    done
    echo '########' Failures: $ret
}

time_run 8 nsieve-bits-u nsieve-bits-c nsieve-bits-s
time_run 2098 meteor-contest-u meteor-contest-c
