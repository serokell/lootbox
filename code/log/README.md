loot-log
=========

_Usable logging abstraction._

Please, be advised that this logging library is somewhat opinionated. In particular:

1. It assumes that you are using the `fmt` library to format your log messages.
2. It uses a hierarchical structure of loggers and automatically derives the name of
   the logger to write a message to from the name of the module.


## Quick Start

We know that in the beginning of your project you’ll probably be able to get away
with something very straightforward. In this case you can start with
`Loot.Config.Rustic`, which is a trivial logging implementation that writes
directly to `stderr`, works in any `MonadIO` and the only thing you can configure
is enable the output of `Debug` events by compiling with `-DDEBUG`.

```haskell
import Loot.Config.Rustic

main :: IO ()
main = logInfo "hello, logging!"
```

As your project grows you’ll want to make the logging configurable, so you’ll
replace rustic logging with a proper implementation.

## Philosophy

### Static Scope

In most cases you don’t want to worry about the hierarchy of loggers, you just want
to be able to easily tell where the logging message comes from. To facilitate this,
each logging message contains the name of the module and the number of the line in
this module which produced it. In other words, logger names are derived automatically
form your source file hierarchy.

Occasionally you might want to adjust the logging parameters for some part of your
application (e.g. to debug a component). Hopefully, the hierarchy of your modules
reflects the structure of your application so being able to customise logging
for messages having the same module name prefix is exactly what you need.

### Dynamic Scope

What to do if you want to adjust the logging configuration for all messages produced
by any code called by a specific function? Hopefully, you’ll be able to do this
somehow, but this functionality is currently in the works.
