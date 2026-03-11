# jsonpath-hs

Implementation of jsonpath as [described by Steffen Göessner](https://goessner.net/articles/JsonPath/).

## State of this library

This library is still work in progress, but feel free to use it create issues. It lacks some features and has a few variances from the description.

### Missing Features
* The Length funtion: The ability to say `$.length`. It will just look for `length` key as of now.
* ScriptExpression: The ability to say things like `$.book[(3+1)]`

### Variances
* The `$` sign in the beginning is not compulsory
* The `$..*` will not produce the root object itself.
## Shout out to [JSON-Path-Test-Suite](https://github.com/gregsdennis/JSON-Path-Test-Suite/tree/master/Tests)
I have copied a few of the tests from there, I will probably just sub-module the repository if and when the whole test suite is green.

## Uses
I am using this library to support GCP authentication in the [Kubernetes haskell client](http://github.com/kubernetes-client/haskell).
