# LAME for Haskell

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/lame.svg?style=flat)](https://hackage.haskell.org/package/lame)
[![Stackage Nightly](http://stackage.org/package/lame/badge/nightly)](http://stackage.org/nightly/package/lame)
[![Stackage LTS](http://stackage.org/package/lame/badge/lts)](http://stackage.org/lts/package/lame)
![CI](https://github.com/mrkkrp/lame/workflows/CI/badge.svg?branch=master)

This is a high-level Haskell binding to the
[LAME](http://lame.sourceforge.net/) encoder.

## Provided functionality

* Fast MP3 encoder working in different modes.
* Setting of all common tags, including pictures.

## Limitations

* No decoding. It is done with a separate library in LAME.
* Relatively limited (compared
  to [ID3 specs](http://id3.org/id3v2.3.0#Text_information_frames)) number
  of tag fields available for setting.
* Some sample widths are not supported: less than or equal to 8 bit and
  greater than 16 bits (for integer samples, floats work fine).
* Some psycho-acoustic and noise-shaping settings are not available for
  tweaking.

## Contribution

Please direct all issues, bugs, and questions to [the GitHub issue tracker
for this project](https://github.com/mrkkrp/lame/issues).

Pull requests are also welcome.

## License

Copyright © 2017–present Mark Karpov

Distributed under BSD 3 clause license.
