loot-log
=========

_Usable logging abstraction._

Please, be advised that this logging library is somewhat opinionated. In particular:

1. It assumes that you are using the `fmt` library to format your log messages.
2. Uses [`co-log`, `co-log-core`](https://github.com/kowainik/co-log) and
   [`co-log-sys`](https://github.com/serokell/co-log-sys) behind the scenes
3. It is expected to be used with the `caps` framework.


## Quick Start

1. Make sure your are in the `CapsT` monad.
2. Initialise the `Logging` capability by configuring the logging backend
   (using `withLogging`).
3. Call logging functions where appropriate (add the `MonadLogging` constraint).

You can load the logging configuration from a Yaml file, take a look at the
complete example in `docs/example/log-config.yaml`.

## Philosophy

### Static Scope

In most cases you just want to be able to easily tell where the logging message
comes from. To facilitate this, each logging message can contain the name of the 
module and the number of the line in this module which produced it. In other 
words, logger names are derived automatically from your source file hierarchy.

### Dynamic Scope

What to do if you want to adjust the logging configuration for all messages produced
by any code called by a specific function? Well, you are lucky, because this is exactly
what the `caps` framework is meant to do. The only thing `loot-log` has to do is provide
access to the configuration and then you will simply use `adjustCap`.
