_(These rules apply to custom snapshot definitions as well.)_


Extra dependencies
-------------------

* First list packages for which you need a specific version from Hackage in alphabetical order.

* Next list packages which you need to pull from GitHub in alphabetical order of package names.

* If any of the previously listed packages require custom versions of their dependencies,
  list those dependencies grouped by the package that made you list this dependency
  under a comment with the name of that package.
