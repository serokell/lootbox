loot-config
============

_Listen, these vinyl records can be used to configure your system!_

# Quick Start

## Declare Configuration Structure

You will need at least the following extensions:

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
```

And the following imports:

```haskell
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec)
```

Declare the structure of your configuration:

```haskell
type Options =
    '[ "timeout" ::: Int
     , "server"  ::<
        '[ "host" ::: String
         , "port" ::: Word16
         ]
     ]
```

Simply put:

* `:::` declares a new configuration option
* `::<` declares a new subsection

Now you can get two data types:

```haskell
type Config     = ConfigRec 'Final   Options
type ConfigPart = ConfigRec 'Partial Options
```

The first one is the type of configurations, it is exactly what you expect it to be: something that contains all the options and subsections.

The second one is the type of _partial_ configurations, that is, some of the options may be missing. It is handy in case you want to build a configuration in multiple steps, for example, set some defaults in code, then load some options from a file, and finally overwrite some options from command line arguments. To make this possible, partial configurations form a `Monoid` in which options which are set later overwrite the ones set earlier.


## Load Configuration

If types of all your options have `FromJSON` instances, the resulting partial configuration type will have it as well, so that you can easily load your configuration from a JSON or a YAML file.

When your partial configuration is ready to be used, it has to be _finalised_. For that we use the `finalise` function, which, for our purposes, can be thought of as having the type `finalise :: ConfigPart -> Either [String] Config`, i.e., if the partial configuration had all the options set, we will get a final configuration (where options are guaranteed not to be missing), or if some options were missing we will get a list of their names.


## Access Configuration Options

To access configuration options you can use the `option` and `sub` lenses. For example, here is how to get the hostname:

```haskell
hostName :: Config -> String
hostName cfg = cfg ^. sub #server . option #host
```

To make the funny hash symbols work you will have to enable this extension:

```haskell
{-# LANGUAGE OverloadedLabels #-}
```

By the way, these same two lenses work on partial configurations too, so you can use them to set or adjust options during initialisation, with one major difference: the values they focus on are wrapped into `Maybe` because they can be missing and you can set them or unset, much like with the `at` lens.
