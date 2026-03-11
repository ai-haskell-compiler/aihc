# om-elm

This package provides utilities for serving Elm programs directly
from your Haskell binary. It uses TemplateHaskell to compile your Elm
program at build time, and construct a WAI Middleware which intercepts
requests appropriate to the Elm program, and passing other requests to
a downstream WAI Application. It is useful for bundling the browser side
of a web application with its backing web services implementation.


