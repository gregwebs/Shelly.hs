Shelly provides a single module for convenient systems programming in Haskell.

* Shelly is aimed at getting things done rather than being a demonstration of elegance.
* Shelly maintains its own environment, making it thread-safe.

These are in contrast to HSH. Elegance in HSH comes from polymorphic input and output, but this requires type annotations.
If you frequently want a different data type than Text when running a system command, you may want to use HSH.

Shelly is a fork of Shellish for efficiency and correctness.

* Text everywhere instead of String
* uses system-filepath and system-fileio for all its operations, including uses FilePath as the command argument.
* low memory usage through 2 mechanisms
  * providing `run_` and other underscore variants that don't return stdout
  * `runFoldLines` to run a fold operation over each line rather than loading all of stdout into memory
  * this also simplifies the implementation and fixes a handle draining bug in the old version

# Usage

~~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
import Shelly
import Prelude hiding (FilePath)

monit = command_ "monit" ["-c", ".monitrc"]

main = shelly $ verbosely $ do
  monit ["reload"]
  echo "monit reloaded"
~~~~~
