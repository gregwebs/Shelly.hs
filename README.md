# Shelly

[![Build Status](https://travis-ci.org/yesodweb/Shelly.hs.svg?branch=master)](https://travis-ci.org/yesodweb/Shelly.hs)
[![Hackage](https://img.shields.io/hackage/v/shelly.svg)](https://hackage.haskell.org/package/shelly)
[![Stackage Nightly](http://stackage.org/package/shelly/badge/nightly)](http://stackage.org/nightly/package/shelly)
[![Stackage LTS](http://stackage.org/package/shelly/badge/lts)](http://stackage.org/lts/package/shelly)

Shelly provides a single module for convenient systems programming in Haskell.

* is aimed at convenience and getting things done rather than being a demonstration of elegance.
* has detailed and useful error messages
* maintains its own environment, making it thread-safe
* is modern, using Text and system-filepath/system-fileio
* has low memory usage
  * `run_` and other underscore variants that don't return stdout
  * `runFoldLines` to run a fold operation over each line rather than loading all of stdout into memory
  * `runHandle` and `runHandles` for complete control over handles

Looking to put your Haskell learning to immediate practical use? You don't have to create artifical intelligence, try just automating some of your boring tasks.

The focus of this library on convenience combined with good error messages should make shelly approachable for newer users of Haskell.
I have published [an introductory article to scripting with shelly, targeted towards those not familiar with Haskell](http://www.linux-magazin.de/Online-Artikel/Shell-scripting-with-type-safety-using-Haskell/). There is a paid version in German from Linux Magazin.
That article uses the version `shelly < 1.0` which uses lazy text. `shelly > 1.0` uses strict text.



## More shelly packages

The [shelly-extra](http://hackage.haskell.org/package/shelly-extra) package has some additional functionality that requires additional dependencies, currently including a convenient concurrency/futures implementation. If you are following along the above article you need to install it.


## Examples

* [A small deployment script](http://www.alfredodinapoli.com/posts/2015-11-03-how-i-deploy-haskell-code.html)
* [Yesod development installer](https://github.com/yesodweb/scripts/blob/master/install.hs)
* [cabal-meta, a haskell install tool](https://github.com/yesodweb/cabal-meta/blob/master/main.hs)
* [antigen-hs, a zsh plugin manager](https://github.com/Tarrasch/antigen-hs)


### Blog Posts

* [Shelly automation with Literate Haskell](http://www.scholarslab.org/dh-developer/shell-programming-in-haskell-converting-s5-slides-to-pdf/)


### Testimonials

* [a beginning Haskeller does automation](http://www.reddit.com/r/haskell/comments/w86gu/my_current_job_task_is_boring_so_i_wrote_a_simple/)

### Help

* [google group for Haskell shell scripting](https://groups.google.com/forum/#!forum/haskell-shell-scripting)

## Alternatives

### Haskell shell scripting libraries


* [HSH](http://hackage.haskell.org/package/HSH) - A good alternative if you want to mixup usage of String and ByteString rather than just use Text.
* [HsShellScript](http://hackage.haskell.org/packages/archive/hsshellscript/3.1.0/doc/html/HsShellScript.html) - Has extensive low-level shell capabilities.
* [shell-conduit](http://hackage.haskell.org/package/shell-conduit) - efficient streaming via conduits. Makes some portability sacrifices by
  * encouraging one to just use the shell instead of cross-platform Haskell code
  * encouraging one to use a convenience function that searches the PATH at compile-time
* [shell-monad](http://hackage.haskell.org/package/shell-monad) - compile Haskell code down to shell script. This is a different approach from all the rest of the libraries. Writing your script is not as user-friendly as the other Haskell libraries, but it nicely solves the deployment issue.
* [turtle](http://hackage.haskell.org/package/turtle) - In some sense a [redesign of Shelly designed for beginner-friendliness](http://www.reddit.com/r/haskell/comments/2u6b8m/use_haskell_for_shell_scripting/co5ucq9)

HSH and HsShellScript (unlike Shelly currently) implement very efficient mechanisms for piping/redirecting in the system.
turtle, like Shelly offers folding as a way to efficiently deal with a stream.

None of the alternatives to Shelly offer command tracing.
For some this is an absolutely critical feature, particularly given that Haskell does not yet offer up stack traces.


### Haskell file-finding supplements

* [find-conduit](http://hackage.haskell.org/package/find-conduit) - uses conduits, similar speed to GNU find
* [FileManip](hackage.haskell.org/package/FileManip) - uses Lazy IO

Shelly's finders load all files into memory. This is simpler to use if you control the filesystem structure and know the system is bounded in size. However, if the filesystem structure is unbounded it consumes unbounded memory.


### Shell commands with richer input/output

Shelly does not change the nature of shell scripting (text in, text out).
If you want something more revolutionary you might try these:

* PowerShell is probably the best known.
* [Haskell project](https://github.com/pkamenarsky/ytools) using typed JSON
* [RecordStream](https://github.com/benbernard/RecordStream) untyped JSON]


## Usage

Shelly's main goal is ease of use.
There should be a primitive for every shell operation you need so you can easily build abstractions, so there are many of the usual file and environment operations.

There are 2 main entry points for running arbitrary commands: `run` and `cmd`.
They take a FilePath as their first argument. `run` takes a [Text] as its second argument.
`cmd` takes a variadic number of arguments, and they can be either Text or FilePath.

Fun Example: shows an infectious script: it uploads itself to a server and runs itself over ssh.
Of course, the development machine may need to be exactly the same OS as the server.

I recommend using the boilerplate at the top of this example in your projects.
This includes setting line buffering if you are dealing with text and not binary data.

~~~~~ {.haskell}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE ExtendedDefaultRules #-}
    {-# OPTIONS_GHC -fno-warn-type-defaults #-}
    import Shelly
    import Data.Text as T
    default (T.Text)

    main =  do
      hSetBuffering stdout LineBuffering
      shelly $ verbosely $ do
        host <- run "uname" ["-n"]
        if T.stripEnd host === "local-machine"
          then do d <- cmd "date"
                  c <- escaping False $ cmd "git" "log -1 | head -1 | awk '{print $2}'"
                  appendfile "log/deploy.log" $ T.intercalate " - " [T.stripEnd d, c]
                  uploads "my-server:/remote/path/" ["deploy"]
                  sshPairs_ "my-server" [("cd", ["/remote/path"]), ("./deploy", [])]
          else do
                cmd "./script/angel"

    -- same path on remote host
    -- will create directories
    uploads :: Text -> [Text] -> Sh ()
    uploads remote locals = rsync $ ["--relative"] ++ locals ++ [remote]

    rsync args = run_ "rsync" $ ["--delete", "-avz", "--no-g"] ++ args
~~~~~

### Variadic arguments to cmd

Yes, as seen above you can write variadic functions in Haskell quite easily, you just can't compose them as easily.
I find `cmd` to be more convenient, but I often use `run` and `command` variants when I am building up abstractions.
Building up abstractions with cmd will require type signatures.

    -- easy signature, but only allows one argument
    let cabal = cmd "cabal" :: Text -> Sh Text

    -- more complex signature that allows partial application of cmd
    let cabal = cmd "cabal" :: Shelly.ShellCmd result => result



### Escaping

By default, all commands are shell escaped.
If you want the shell to interpret special characters such as `*`, just use `escaping False $ do ...`

### Using Text and FilePath together

Shelly's usage of system-filepath means you may need to convert between Text and FilePath sometimes.
This should be infrequent though because

* `cmd` will convert FilePath to Text
* The `</>` and `<.>` combinators convert String/Text into a FilePath automatically

Manual conversion is done through `toTextIgnore` or `toTextWarn`.


### Thread-safe working directory and relative paths

`cd` does not change the process working directory (essentially a global variable), but instead changes the shelly state (which is thread safe).
All of the Shelly API takes this into account, internally shelly converts all paths to absolute paths. You can turn a relative path into an absolute with `absPath` or `canonic` or you can make a path relative to the Shelly working directory with `relPath`.


### Good error messages

Haskell's #1 weakness for IO code is a lack of stack traces.
Shelly gives you something different: detailed logging.
In most cases this should be more useful than a stack trace.
Shelly keeps a log of API usage and saves it to a .shelly directory on failure.
If you use `shellyNoDir`, the log will instead be printed to stderr.
This is in addition to the `verbosely` settings that will print out commands and their output as the program is running.
Shelly's own error messages are detailed and in some cases it will catch Haskell exceptions and re-throw them with better messages.

If you make your own primitive functions that don't use the existing Shelly API, you can create a wrapper in the Sh monad that use `trace` or `tag` to log what they are doing.
You can turn tracing off (not generally recommended) by setting `tracing False`.


## Future plans

* Don't use the filepath library
