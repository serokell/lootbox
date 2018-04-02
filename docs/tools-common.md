Tools
======

A tool is a small executable meant for performing a simple task related to
the use of the system.

Tools live in the `tools` directory and often consist of a single `Main.hs` file.
To make the life of the user easier, all the tools are expected to provide a
more-or-less similar UX. If you are developing a tool, please, try to do your best!

## Command line arguments

We parse command line arguments with [`optparse-applicative`][optparse-applicative].
It has some pretty nice
features, e.g.:

* `argument` for parsing positional (mandatory) arguments
* `option` for parsing options (optional)
* `hsubparser` for subcommands

## The library

To make it easier to create new tools and to make sure that they follow the same
UX patterns, we have a bunch of useful modules in the [`tools-common`] package.

  [`tools-commmon`]: /tools-common/src/