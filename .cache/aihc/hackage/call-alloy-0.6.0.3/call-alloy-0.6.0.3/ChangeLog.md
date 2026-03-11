# Changelog for call-alloy

## Unreleased changes

## Released changes

### 0.6.0.3

- fix native-access bug with recent Java versions

### 0.6.0.2

- ignore finalizer warnings

### 0.6.0.1

- fix bug on different State 0 entry response

### 0.6

- upgrade to Alloy 6.2.0
- document Alloy version
- improve documentation

### 0.5

- use MonadThrow instead of MonadError
- add readme example to test cases

### 0.4.1.1

- introduce flag for testing different SAT solvers

### 0.4.1

- add support for different SAT solvers

### 0.4.0.3

- fix terminal spamming due to process abortion on Windows

### 0.4.0.2

- make resilient to IO errors due to parallelism or abort

### 0.4.0.1

- fix too early abortion of process execution
- show raw output on library parsing issues

### 0.4

- provide required Java libraries in package data directory
- use async for process interaction
- terminate calls to Java properly (even on interrupts)

### 0.3.0.3

- fix runtime error on WARN

### 0.3.0.2

- fix runtime error on PARTIAL_INSTANCE

### 0.3.0.1

- support "State0" line in retrieved instances (introduced by Alloy 6)
- support CRLF at line end

### 0.3

- upgrade to Alloy 6.0.0
- allow slashes in object names
- allow identity relations
- improve feedback on misspelled/missing relation and signature names
- remove 'relToMap'

### 0.2.2.0

- deprecate 'relToMap'
- provide functions for returning raw output of instances
- provide functions for typed retrieval
- deprecate 'getSingle', 'getDouble', 'getTriple', 'objectName'

### 0.2.1.1

- fix errors due to long Alloy code by starting timeout after transferring code

### 0.2.1.0

- enable to abort instance generation early by using timeout

### 0.2.0.6

- allow parsing `'` as part of words.
  (Especially `skolem` may return them if variable names in predicates to check
  are not unique.)
- add version constraint for `Win32`
- allow later versions of `bytestring`
