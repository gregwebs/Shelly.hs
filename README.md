Shelly provides a single module for convenient systems programming in Haskell.

* Shelly is aimed at convenience and getting things done rather than being a demonstration of elegance.
* Shelly maintains its own environment, making it thread-safe.

Shelly is a fork of Shellish (which accomplished the above 2 goals) with many useful improvements

* Good error messages
* Text everywhere instead of String
* Use of system-filepath and system-fileio for all its operations
* Low memory usage
  * `run_` and other underscore variants that don't return stdout
  * `runFoldLines` to run a fold operation over each line rather than loading all of stdout into memory

Please see the shelly-extra package for additional functionality.


## Alternatives

* [HSH](http://hackage.haskell.org/package/HSH) - A good alternative if you want to mixup usage of String and ByteString rather than just use Text.
* [HsShellScript](http://hackage.haskell.org/packages/archive/hsshellscript/3.1.0/doc/html/HsShellScript.html) - Has more extensive shell capabilities


# Usage

Shelly's main goal is ease of scripting.
At the same time there should be a primitive for every shell operation you need so you can easily build abstractions, so there are many of the usual file and environment operations.

There are 2 main entry points for running arbitrary commands: `run` and `cmd`.
They take a FilePath as their first argument. `run` takes a [Text] as its second argument.
`cmd` takes a variadic number of arguments, and they can be either Text or FilePath.

~~~~~ {.haskell}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE ExtendedDefaultRules #-}
    {-# OPTIONS_GHC -fno-warn-type-defaults #-}
    import Data.Text.Lazy as LT
    default (LT.Text)

    monit_ = command_ "monit" ["-c", ".monitrc"]

    main = shelly $ verbosely $ do
      listing <- cmd "ls" "-a" "foo"

      monit_ ["reload"]
      echo "monit reloaded"
~~~~~

Yes, you can write variadic functions in Haskell quite easily, you just can't compose them.
I find `cmd` to be more convenient, but I use `run` when I am building up abstractions.

Shelly's usage of system-filepath means you may need to convert between Text and FilePath sometimes.
This should be infrequent though because

* `cmd` will convert FilePath to Text
* The `</>` and `<.>` combinators convert String/Text into a FilePath automatically

Manual conversion is done through `toTextIgnore` or `toTextWarn`.


# Good error messages

Haskell's #1 weakness is a lack of stack traces.
Shelly gives you something fairly close to this though.
It keeps a log of API usage and prints it out on failure.
This is in addition to the `verbosely` settings that will print out commands and their output as the program is running.
Shelly's own error messages are detailed and in some cases it will catch Haskell exceptions and re-throw them with better messages.


# Future plans

* Switch from lazy text to strict
* fix PATH/which implementation
