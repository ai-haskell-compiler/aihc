#### 0.2.0.1 *2024-12-16*

	* Fix build with pre 5.6 linux-headers, where there is no support for time namespaces.

#### 0.2.0.0 *2024-11-29*

	* Add support for CGroup and time namespaces.

#### 0.1.3.1 *2023-06-23*

	* Support building with `unix-2.8`.

#### 0.1.3.0 *2018-06-21*

	* Use raw POSIX io to write to proc files (Baojun Wang).
	* Support denying setgroups to processes in user namespaces, needed to support unprivileged creation of user namespaces (Baojun Wang).

#### 0.1.2.0 *2014-10-30*

	* Provide Read instances for UserMapping & GroupMapping.

#### 0.1.1.1 *2014-10-30*

	* Improve documentation.

#### 0.1.1.0 *2014-10-29*

	* Export data constructors for UserMapping & GroupMapping.
