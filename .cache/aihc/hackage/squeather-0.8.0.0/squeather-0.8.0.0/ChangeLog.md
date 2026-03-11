# Changelog for squeather

## Version 0.8.0.0, May 7, 2021

* Updated SQLite library to version 3.35.5.

* Removed references and links to Github.

## Version 0.6.0.0, December 29, 2020

* Added SQLite compile-time option to disable double-quoted string literals

## Version 0.4.2.0, December 27, 2020

* Updated SQLite library to version 3.34

## Version 0.4.0.0, February 22, 2020

* Changed Database and Statement types so they clean
up after themselves, so there is no need to (and, indeed,
then cannot be) closed or finalized as they do it themselves.

* Changed OpenFlags and associated types to make
API self-documenting

* Added compile-time option for threaded mode into package.yaml
(though this is the default, so nothing should change from the
user's persepctive)

* open function now calls sqlite3_initialize (this should
not result in any user-visible changes)
