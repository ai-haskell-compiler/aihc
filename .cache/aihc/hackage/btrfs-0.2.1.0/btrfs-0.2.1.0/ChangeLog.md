#### 0.2.1.0 *2023-06-23*

	* Support building with `ghc-9`.
	* Support building with `unix-2.8`.

#### 0.2.0.0 *2018-10-25*

	* Implement getFSInfo.
	* Remove an ugly hack that was used to block signals while defrag was running.
	* Support Zstd compression.
	* Make CompressionType abstract.
	* Fix handling of filenames that are not valid according to the current locale.
	* Deprecate System.Linux.Btrfs.ByteString.
	* Support getting/setting file system labels.

#### 0.1.2.3 *2017-01-30*

	* System.Linux.Btrfs.UUID.fromString did not handle all malformed
	UUIDs correctly.

#### 0.1.2.2 *2016-11-15*

	* Fix compilation error when libcap is not installed.

#### 0.1.2.1 *2016-11-13*

	* Support cloneRangeIfSame on read-only subvolumes (requires
	CAP_SYS_ADMIN).

#### 0.1.2.0 *2016-02-23*

	* Expose System.Linux.Btrfs.Time.
	* Add example program that prints the file creation timestamp.

#### 0.1.1.1 *2014-10-05*

	* Support getting/setting the id of the default subvolume.

#### 0.1.1.0 *2014-10-05*

	* Support defraging file ranges.

#### 0.1.0.0 *2014-09-01*

	* Initial public release.
