# Revision history for vty-windows

0.1.0.0
-------
* First version. Released on an unsuspecting world.

0.1.0.1
-------
Bug fix: Build was failing due to incorrect bounds check

0.1.0.2
-------
Bug fix: Attribute resetting escape sequence was incorrect.

0.1.0.3
-------
Bug fixes:
 * Fixed the sgr capability sequence.
 * Removed duplicate capability strings.

Package changes:
 * Removed unnecessary version bounds on dependencies.

0.2.0.0
-------
* The color mode configuration moved from Windows specific
  settings to the vty user config.

0.2.0.1
-------
* On shutdown, make sure to shut down output interface before shutting down input interface.
  Fixes mouse reset bug.

0.2.0.2
-------
* Fixed bug in processing response of the ReadConsoleInputW that caused vty-windows to crash.

0.2.0.3
-------
* Now vty-windows supports running in MSYS/MSYS2 terminals. This was achieved by modifying
  the way the input and output buffers are initialized.