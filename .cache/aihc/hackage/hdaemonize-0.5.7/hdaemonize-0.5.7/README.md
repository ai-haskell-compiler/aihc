`hdaemonize`
=================
[![Build Status](https://travis-ci.org/unprolix/hdaemonize.svg?branch=master)](https://travis-ci.org/unprolix/hdaemonize)

`hdaemonize` is a simple library that hides some of the complexities
of writing UNIX daemons in Haskell.

Obtaining
-----------

The latest version is available (BSD license) at
[GitHub](https://github.com/unprolix/hdaemonize).

Using
-------

The synopsis is:

    import System.Posix.Daemonize
    main = daemonize $ program

This code will make `program` do what good daemons should do, that is,
detach from the terminal, close file descriptors, create a new process
group, and so on.

If you want more functionality than that, it is available as a
`serviced` function.

Here is an example:

    import Control.Concurrent
    import System.Posix.Daemonize

    loop i log = do threadDelay $ 10^6
                    log (show i)
            	    writeFile "/tmp/counter" $ show i
                    if i == 5 then undefined else loop (i + 1) log

    main = serviced (loop 0)

Let us say this program is compiled as `mydaemon`. Then:

    # mydaemon start

starts the service. A second call to start will complain that the
program is already running.

During its execution, mydaemon will simply write a new number to
`/tmp/counter` every second, until it reaches 5. Then, an exception
will be thrown. This exception will be caught by `hdaemonize`, and
logged to `/var/log/daemon.log` or similar (this is depends on how
`syslog` works on your platorm).  `log (show i)` will leave messages
in the same file.

When the exception is thrown, the program will be restared in 5
seconds, and will start counting from 0 again.

The following commands are also made available:

    # mydaemon stop
    # mydaemon restart

Finally, if configured to do so, `mydaemon` drops privileges by
changing its effective user/group ID. (If no user/group ID are
specified, it will continue execution as the original user and group.)

Note that if you wish to parse your own commandline arguments, you can
replace invocation of the `serviced` function with `serviced'`. This
requires specification of an `Operation` which indicates whether the
daemon should be started, stopped, restarted, or whether its status
should be queried.

Changelog
---------

* 0.5.6
    * Add `serviced'` function and `Operation` (`Start`, `Stop`, etc.) to allow invocation separated from commandline
    * Only attempt to change effective user/group ID when explicitly specified.
    * Do not attempt to set user or group to the daemon's name in the absence of a specified user or group.

* 0.5.5
    * Fix a bug where hdaemonize fails when user or group "daemon" is absent

* 0.5.4
    * Update to use hsyslog == 5.

* 0.5.2
    * Fix pre-AMP builds.

* 0.5.1
    * Updated to use hsyslog >=4

* 0.4
    * added support for a privileged action before dropping privileges

* 0.3
    * merged with updates by madhadron

* 0.2
    * provided documentation
    * backported to older GHC versions, tested on 6.8.1

* 0.1
    * initial public release


Authors
-------
Jeremy Bornstein <jeremy@jeremy.org>

Lana Black <lanablack@amok.cc>

Anton Tayanovskyy <name.surname@gmail.com>.

The code is originally based on a public posting by
[Andre Nathan](http://sneakymustard.com/), used by permission.
