Shelly is a package provides a single module for convenient
systems programming in Haskell, similar in spirit to POSIX
shells.

* Shelly is aimed at getting things done rather than
 being a demonstration of elegance.
* Shelly maintains its own environment, making it thread-safe.

These are in contrast to HSH. Elegance in HSH comes from polymorphic input and output, but this requires type annotations.
If you frequently want a different data type than Text when running a system command, you may want to use HSH.

Shelly is a fork of Shellish for efficiency.
Shelly uses Text instead of String, features low memory usage, and fixes a handle draining bug.
Note that Shelly uses Text *everywhere*, except for the environment variable settings.
This includes exporting a FilePath that is Text.
Using Text *everywhere* is for convenience so you don't have to convert between strings.

# Usage

~~~~~ {.haskell}
    import Shelly
    import Prelude hiding (FilePath)


    monit = command_ "monit" ["-c", ".monitrc"]

    main = shelly $ verbosely $ do
      monit ["reload"]
~~~~~
