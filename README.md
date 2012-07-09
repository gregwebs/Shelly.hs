# Shelly

Shelly provides a single module for convenient systems programming in Haskell.

* is aimed at convenience and getting things done rather than being a demonstration of elegance.
* has detailed and useful error messages
* maintains its own environment, making it thread-safe
* is modern, using Text and system-filepath/system-fileio
* has low memory usage
  * `run_` and other underscore variants that don't return stdout
  * `runFoldLines` to run a fold operation over each line rather than loading all of stdout into memory

Looking to put your Haskell learning to immediate practical use? You don't have to create artifical intelligence, try just automating some of your boring tasks.

The focus of this library on convenience combined with good error messages should make shelly approachable for newer users of Haskell.
I have published [an introductory article to scripting with shelly, targeted towards those not familiar with Haskell](http://www.linux-magazin.de/Online-Artikel/Shell-scripting-with-type-safety-using-Haskell/). There is a paid version in German from Linux Magazin.


## More shelly packages

The [shelly-extra](http://hackage.haskell.org/package/shelly-extra) package has some additional functionality that requires additional dependencies, currently including a convenient concurrency/futures implementation. If you are following along the above article you need to install it.


## Examples

* [Yesod development installer](https://github.com/yesodweb/scripts/blob/master/install.hs)
* [cabal-meta, a haskell install tool](https://github.com/yesodweb/cabal-meta/blob/master/main.hs)


### Blog Posts

* [Shelly automation with Literate Haskell](http://www.scholarslab.org/dh-developer/shell-programming-in-haskell-converting-s5-slides-to-pdf/)


### Testimonials

* [a beginning Haskeller does automation](http://www.reddit.com/r/haskell/comments/w86gu/my_current_job_task_is_boring_so_i_wrote_a_simple/)


## Alternatives

* [HSH](http://hackage.haskell.org/package/HSH) - A good alternative if you want to mixup usage of String and ByteString rather than just use Text.
* [HsShellScript](http://hackage.haskell.org/packages/archive/hsshellscript/3.1.0/doc/html/HsShellScript.html) - Has more extensive shell capabilities.


## Usage

Shelly's main goal is ease of use.
There should be a primitive for every shell operation you need so you can easily build abstractions, so there are many of the usual file and environment operations.

There are 2 main entry points for running arbitrary commands: `run` and `cmd`.
They take a FilePath as their first argument. `run` takes a [Text] as its second argument.
`cmd` takes a variadic number of arguments, and they can be either Text or FilePath.

~~~~~ {.haskell}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE ExtendedDefaultRules #-}
    {-# OPTIONS_GHC -fno-warn-type-defaults #-}
    import Shelly
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

By default, all commands are shell escaped.
If you want the shell to interpret special characters such as `*`, just use `escaping False $ do ...` 

Shelly's usage of system-filepath means you may need to convert between Text and FilePath sometimes.
This should be infrequent though because

* `cmd` will convert FilePath to Text
* The `</>` and `<.>` combinators convert String/Text into a FilePath automatically

Manual conversion is done through `toTextIgnore` or `toTextWarn`.


### Thread-safe working directory

`cd` does not change the process working directory (essentially a global variable), but instead changes the shelly state (which is thread safe).
All of the Shelly API takes this into account, internally shelly converts all paths to absolute paths. You can get turn a relative path into an absolute with `absPath` or `canonic` or you can make a path relative to the Shelly working directory with `relPath`.


### Good error messages

Haskell's #1 weakness for IO code is a lack of stack traces.
Shelly gives you something different: detailed logging.
In most cases this should be more useful than a stack trace.
Shelly keeps a log of API usage and prints it out on failure.
This is in addition to the `verbosely` settings that will print out commands and their output as the program is running.
Shelly's own error messages are detailed and in some cases it will catch Haskell exceptions and re-throw them with better messages.

If you make your own primitive functions that don't use the Shelly API, use `trace` or `tag` to log what they are doing.
Note that you can turn tracing off (not generally recommended) by setting `tracing False`.


### Future plans

* improved SSH API
* Switch from lazy text to strict
* fix PATH/which implementation
