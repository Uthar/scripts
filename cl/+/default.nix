

with import <nixos> {};


pkgs.writeScriptBin "+"
  ''#!/usr/bin/env -S ${sbcl}/bin/sbcl --script
    ${builtins.readFile ./+.lisp}
  ''
