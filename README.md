Shelly is a package provides a single module for convenient
systems programming in Haskell, similar in spirit to POSIX
shells.

* Shelly is aimed at getting things done rather than
 being a demonstration of elegance.
* Shelly maintains its own environment, making it thread-safe.

These are in contrast to HSH. Elegance in HSH comes from polymorphic input and output.
If you frequently want a different data type than Text from running a system command, you may want to use HSH.

Shelly is a fork of Shellish for efficiency.
It uses Text instead of String, features low memory usage, and fixes a handle draining bug.
