binary-ieee754
==============

[![Hackage](https://img.shields.io/hackage/v/binary-ieee754.svg?style=flat)](http://hackage.haskell.org/package/binary-ieee754)
[![Build Status](https://travis-ci.org/winterland1989/binary-ieee754.svg)](https://travis-ci.org/winterland1989/binary-ieee754)

This package backports ieee754 double/float combinators from [binary](http://hackage.haskell.org/package/binary) 0.8.4 to older version, and simply re-export these combinators for binary >= 0.8.4. It's meant for packages which need fast double/float combinators and old binary compatibility.
