# wild-bind

WildBind is a dynamic and programmable key binding framework. See https://github.com/debug-ito/wild-bind for WildBind in general.

## Architecture and Terminology

WildBind consists of `FrontEnd` and `Binding`.

                     +-------------------+
    (user) --input-> |   desktop env.    |---[FrontEnd]---[Binding]
                     |                   |                    |
                     | (front-end state) |             (back-end state)
                     +-------------------+

- A `FrontEnd` interfaces with a desktop environment. It reads input from the user and the state of the desktop environment. The state is called "front-end state". `FrontEnd` passes those two kinds of data to `Binding`.
- A `Binding` binds actions to input symbols. Optionally it has its own state, which is called "back-end state".


## wild-bind Packages

- [wild-bind](https://hackage.haskell.org/package/wild-bind): WildBind core data types and functions. This package defines `FrontEnd`, `Binding` and other common types. Although WildBind is mainly targeted to number pads, its core is independent of any input types or desktop environments.
- [wild-bind-x11](https://hackage.haskell.org/package/wild-bind-x11): A `FrontEnd` implementation for X11 desktop environments.
- [wild-bind-indicator](https://hackage.haskell.org/package/wild-bind-indicator): A GUI that describes current `Binding` to the user.
- [wild-bind-task-x11](https://hackage.haskell.org/package/wild-bind-task-x11): A bundle package that combines all packages above. End users should use this package first.


## Author

Toshio Ito <debug.ito at gmail.com>

