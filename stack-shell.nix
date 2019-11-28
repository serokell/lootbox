let
  project = import ./. { _expose = true; };
in

project.shellFor { packages = []; }
