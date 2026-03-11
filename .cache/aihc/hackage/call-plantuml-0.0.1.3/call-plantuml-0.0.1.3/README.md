# `call-plantuml` [![Haskell CI](https://github.com/marcellussiegburg/call-plantuml/workflows/Haskell%20CI/badge.svg)](https://github.com/marcellussiegburg/call-plantuml/workflows/Haskell%20CI/badge.svg)

This is a simple library to call [PlantUML](https://plantuml.com) given a diagram specification.
PlantUML is included (as JAR file) within this library.

## Requirements

- Java Runtime Environment:
  There is currently no warning if you have not set up any Java Runtime Environment.
  However, you will get runtime errors if it is not available when a call to PlantUML happens.
  If you want to force a check, please perform the test cases.
