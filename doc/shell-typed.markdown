# Shell scripting with type-safety using Haskell

We can use a DSL in Haskell (a strongly-typed functional programming language) for shell-scripting that looks very similar to bash.

    main = shelly $ do
        apt_get "update" []
        apt_get "install" ["haskell-platform"]
      where
        apt_get mode more = run "apt-get" (["-y", "-q", mode] ++ more)

Our goal however is not imitation, but a better environment for systems programming. The main benefit we now have is type-safety: we can let the type system catch errors at compile time. But these are not ordinary C/Java types: you don't see any type signatures here because Haskell has type-inference, which lets us sparsely specify types and have the compiler figure out the rest. The type system is also more powerful: eventually we will see how it allows us to express more invariants at compile-time.

I am motivated to reduce errors in scripts because the cost of testing and failure can be higher than other programs. Automated testing of scripting is inherently more difficult because we are testing interaction with other programs. At the same time, the cost of not testing can be higher because we may leave our system in a state that requires manual work to be undone, which in the case of deployment scripts could include a non-functioning or even inaccessible state.

For our purposes we can define a script as a program focused on interacting with the OS or other programs. There must be a reason why this has traditionally been left to dynamic languages. I suspect it is largely because scripting languages such as Perl, Ruby, and Bash are high-level languages that have simple interfaces for OS interaction. Let's stay high-level but instead start with strong typing and try to figure out how to make interaction with the OS as convenient as possible. The standard Haskell libraries expose a complete set of primitives, but they lack an easy-to-use and flexible API. We can use the language's flexibility to create a nicer interface.

I would encourage you to follow this code more closely by first [installing Haskell](http://hackage.haskell.org/platform/) and then compiling snippets with `ghc`. You will need to first install Shelly with the command: `cabal install shelly` Try this one out:

    {-# LANGUAGE OverloadedStrings #-}
    import Shelly

    main = shelly $ do
        appendfile "a.hs" "main = print \"hello\""
        run "ghc" ["a.hs"]
        out <- run "./a" []
        echo out

You can save it in a file `makea.hs`, and then run `ghc makea.hs && ./makea.hs`
Note that the top 2 lines are also required to use Shelly in future examples.
If we put a number in the second argument of appendfile, or omit the argument completely, we get a type error. A new user might struggle to understand some of the error messages, but the important thing is we know at compile time the exact line where there is an error.

To help follow along with basic Haskell code it might help to take a look at the Learn You a Haskell book.
We are going to see some type signatures now. You can [learn more about them](http://learnyouahaskell.com/types-and-typeclasses).

In addition to adding type-safety, Haskell is also arguably the most radical departure from the traditional scripting paradigm we can choose. Scripting is focused on interacting with the OS, but in Haskell the type system separates this from pure computation.

    add_two :: Int -> Int
    add_two x = x + 2

This is a function definition. In javascript, this function would be written as: `function add_two(x) { return x + 2; }`

The function `add_two` by definition cannot interact with the OS. It also cannot mutate a variable such as x in any way. Haskell variable are actually constants! Purity pays off greatly for the compiler, which knows it can optimize the code. This is great for parallelism and concurrency, because pure code is thread-safe. Purity also makes it easier to understand what is going on: just by the type signature we know what this function is *not* capable of doing: scripting!

    add_2_input :: IO Int
    add_2_input = do
      line <- getsLine
      return (add_two (read line))

    add_two :: Int -> Int
    add_two x = x + 2

Here we define `add_2_input` explicitly as a function that will perform IO, or input/output.
`read` converts a String to a Haskell value. It knows to convert to an `Int` because of type inference from the `Int` in the type signature. The important thing to note is that the type signature `IO Int` tells us there is input/output, or scripting that returns an Int. You can run this code as an interactive program:

    main = add_2_input >>= print

After compiling with ghc, one can run the program. Entering the number 5 gives:

    ./add_two
    5
    7

`IO` is a monad. Operations in a monad can use the `do` notation syntax seen above.
For our purposes, a monad defines an execution environment. The goal of the Shelly library is to define an optimal environment for scripting, it is the monad ShIO.

My article editor gave me the following task:
Implement a script that reads logins and names from a CSV file and batch-creates Linux user accounts, similar to the "newusers" command.
Allow only [a-z0-9] in LOGIN.

So first let's write pure code that creates our users:


    {-# LANGUAGE OverloadedStrings #-}
    import Data.Char (isLower, isNumber)
    import Data.Text.Lazy (unpack, Text)
    import Data.Monoid ((<>))
    import Data.Text.Lazy (lines, Text, all, unpack, splitOn)
    import Prelude hiding(lines, all)
    import Shelly

    data NewUser = NewUser {
                     login :: Text
                   , lastName :: Text
                   , firstName :: Text
                   }

    usersFromCSV :: Text -> [NewUser]
    usersFromCSV csv = map userFromLine (lines csv) where
      userFromLine line = toUser (line `splitOn` ",") where

        toUser (l:lname:fname:[]) | checkLogin l = NewUser {login = l, lastName = lname, firstName = fname}
        toUser _ = error $ unpack $ "expected \"login,lastname,firstname\" but got: " <> line

        checkLogin = all (\char -> isLower char || isNumber char)


`$` is an operator with very low precedence that lets us avoid parentheses.
The `toUser` function uses pattern matching to determine its argument.
If we have a list of 3 items, the first toUser definition is matched. `checkLogin` is a further guard that must be true before the first definition is invoked. Pattern matching provides code clarity, but also safety because the compiler will warn us if we are missing any patterns. If we didn't have a second definition of `toUser`, using the ` _ ` wildcard pattern, the compiler would emit a warning.

Now to the scripting:


    mk_user :: NewUser -> ShIO Text
    mk_user user =
      run_sudo "addUser" [login user, "--gecos", firstName user <> " " <> lastName user]

    run_sudo :: Text -> [Text] -> ShIO Text
    run_sudo cmd args = run "/usr/bin/sudo" (cmd:args)

    main = shelly $ do
      csv <- readfile "users.csv"
      mapM_ mk_user (usersFromCSV csv)


`shelly` is what kicks off our ShIO monad. `map` is the standard looping function, but here we use `mapM_`. By convention, `M` indicates we are performing the map with a monadic operation, and ` _ ` indicates we are discarding any result.

Hopefully this shows that shell scripting can be about as easy in Haskell as in traditional scripting languages. The separation of pure and impure code can feel natural and make our code easier to understand and maintain. We know if the code compiles that there are no typos in functions or variable names and that every function takes the correct number of arguments all with the correct types. With pattern-matching we go beyond basic types to describe our arguments.

But lets show more uses for types: requiring explicit permission requests [1]. `mk_user` will be defined as requiring sudo, and the caller should indicate that they are using sudo.


    newtype Sudo a = Sudo { sudo :: ShIO a }

    run_sudo :: Text -> [Text] -> Sudo Text
    run_sudo cmd args = Sudo $ run "/usr/bin/sudo" (cmd:args)

    mk_user :: NewUser -> Sudo Text
    mk_user user =
      run_sudo "addUser" [login user, "--gecos", firstName user <> " " <> lastName user]


Now our previous code with fail at compile time.
The type signature of `mk_user` makes its required privileges clear, and every caller must acknowledge that by using `sudo`:


    main = shelly $ do
      csv <- readfile "users.csv"
      mapM_ (sudo . mk_user) (usersFromCSV csv)


Haskell also excels in concurrency. If our script needs send information about user creation to a central login service, we could speed up our script by performing these tasks concurrently.


    announce_user u = echo "contacting remote user service"

    main = shelly $ do
      csv <- readfile "users.csv"
      let users = usersFromCSV csv
      mapM_ (sudo . mk_user) users
      jobs 5 $ \job ->
        mapM_ (background job . announce_user) users


The main difference here is the insertion of `background`, a Shelly function which runs the task in a Haskell thread.
`jobs` sets an upper limit on the number of simultaneous background jobs that can occur: if there are ten users, after the first five `mk_user` jobs are backgrounded, our program will wait for one `mk_user` to finish before starting another. `jobs` will also wait for all background tasks to complete before returning. This ensures our program will not exit early before all users are created.

Haskell threads are extremely light-weight green threads, but can handle multi-core. Haskell's runtime is always asynchronous, so for concurrency a Haskell program needs only to spawn a thread; there is no concern of blocking IO. A Haskell programmer also does not think about whether a thread is justified computationally, just whether it is the right tool for the job. Purity means we avoid the common pitfall of shared state.

The library code seen here is available in the [shelly](http://hackage.haskell.org/package/shelly) library that I authored. Currently I am personally using shelly in a community installer program for Haskell called [cabal-meta](http://hackage.haskell.org/package/cabal-meta) and a personal deployment script. Combined with [shake](http://hackage.haskell.org/package/shelly), a better Haskell version of make, I have a nice deployment toolset. I let the compiler worry about most of the errors that are incidental to what I am trying to get done and get to focus on the deployment commands and logic of the program. Certainly this is not a panacea, and my current setup is lacking in configurability and sharability, but I think I have some important building blocks for even better solutions.

A Future direction of Shelly I am considering is integration with the [shqq](http://hackage.haskell.org/package/shqq-0.1) library, which features a shell command quasi-quoter.

    >>> import System.ShQQ
    >>> let x = "foo bar" in [sh| cat $x |]
    cat: foo bar: No such file or directory

The important thing here is not just that we have string interpolation, but that the interpolated string is properly shell escaped. There is also a syntax to avoid escaping.

    >>> let y = "foo bar" in [sh| cat $+y |]
    cat: foo: No such file or directory
    cat: bar: No such file or directory

The author of a Haskell quasi-quoter must parse the expression and convert it into Haskell source code. The pay off is a way for an author to embed an arbitrary language within Haskell.

[HSH](http://hackage.haskell.org/package/HSH) is a neat Haskell shell library. The book Real World Haskell [goes over how to implement its functionality](http://book.realworldhaskell.org/read/systems-programming-in-haskell.html). HSH focuses on conveniently piping shell command output to other shell commands or to Haskell functions.

    import HSH

    runIO $ "ls -l" -|- "wc -l"
     -> 12

It also uses polymorphic output to get desired information about the command execution.
The following will return a list of strings instead of just a string.

    run $ "ls -l" :: IO [String]

This is a neat demonstration of Haskell's type inference.
In practice the requirement for type signatures can be cumbersome and it can be difficult to remember the type signature for the particular needed result types.

[hsshellscript](http://hackage.haskell.org/package/hsshellscript) is yet another Haskell shell library featuring a complete set of shell utilities that improve upon the standard Haskell libraries.

The Haskell community seems to be recognizing that we do not have to use "scripting languages" for shell scripts, that instead we can leverage Haskell for safety. The point of this article though is not to convert everyone to use Haskell, but to make you think about bringing some of these techniques into your existing shell script environment.


[1]: Sudo permission technique along with some other ideas on shell scripting in Haskell were presented here : http://donsbot.files.wordpress.com/2009/01/semicolon.pdf

# Author

[Greg Weber](blog.gregweber.info) is looking for ways to do things better. He is a major contributor to the Haskell web framework [Yesod](www.yesodweb.com) and now works at Yap.TV on every aspect of building web applications.

