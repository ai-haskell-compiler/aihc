cgi
===

[![hackage release](https://img.shields.io/hackage/v/cgi.svg?label=hackage)](http://hackage.haskell.org/package/cgi)
[![stackage LTS package](http://stackage.org/package/cgi/badge/lts)](http://stackage.org/lts/package/cgi)
[![stackage Nightly package](http://stackage.org/package/cgi/badge/nightly)](http://stackage.org/nightly/package/cgi)
[![CI Status](https://github.com/cheecheeo/haskell-cgi/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/cheecheeo/haskell-cgi/actions/workflows/haskell-ci.yml)

This is a Haskell library for writing CGI programs.
Its features include:

- Access to CGI parameters (e.g. form input) from both
  GET and POST requests.
- Access to CGI environment variables.
- Ability to set arbitrary response headers.
- Support for HTTP cookies.
- An efficient implementation of multipart/form-data
  using Data.ByteString. This allows for efficient
  handling of file uploads.
- A CGI monad transformer.
- Basic exception handling and logging (these should be improved)
- Low-level run functions that allow using programs written with
  this package with protocols other than CGI, for example FastCGI.

On hackage: http://hackage.haskell.org/package/cgi
Source: https://github.com/cheecheeo/haskell-cgi
