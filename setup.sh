#!/bin/zsh
# This is just documentation for me to remember how I set this up in the future.
# Not to be confused with Setup.hs
stack new aoc2022 new-template
# Stack will ignore the system installation of GHC and other tools even when
# explicitly requested (with `system-ghc true`) and will instead install its own
# (despite `install-ghc false`) in ~/.stack/programs/x86_64-osx (location is
# obviously platform-dependent).
#
# This might only be a problem if the version it decides to install does not
# have HLS support, as Doom Emacs is integrated (?) with Stack (see
# https://docs.doomemacs.org/latest/modules/lang/haskell/) and will attempt to
# use the language server for the project's version of GHC. If this happens and
# you get an error, use GHCup to check which GHC versions do offer HLS support
# in MacOS, then see stack.yaml.
#
# stack config set install-ghc false --global
# stack config set system-ghc true --global
git init
mkdir data
# TODO: integrate make-day and test-day with Stack/Cabal (akin to NPM scripts).
# copy make-day.sh
# copy test-day.sh
# copy day-template dir
