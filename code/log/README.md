loot-log
=========

_Usable logging abstraction._

Please, be advised that this logging library is somewhat opinionated. In particular:

1. It assumes that you are using the `fmt` library to format your log messages.
2. It uses a hierarchical structure of loggers and automatically derives the name of
   the logger to write a message to from the name of the module.
3. It is expected to be used with the `caps` framework.


## Quick Start

1. Make sure your are in the `CapsT` monad.
2. Initialise the `Logging` capability by configuring the logging backend
   (currently it is `log-warper`, so you’ll use `withLogWarper`).
3. Call logging functions where appropriate (add the `MonadLogging` constraint).


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
by any code called by a specific function? Well, you are lucky, because this is exactly
what the `caps` framework is meant to do. The only thing `loot-log` has to do is provide
access to the configuration and then you will simply use `adjustCap`.
