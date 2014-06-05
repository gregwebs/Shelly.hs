# Shelly's Monad: an execution environment and DSL

`IO` is a monad. For our purposes, a monad defines an execution environment.
The goal of Shelly is to define an optimal environment for scripting, the `Sh` monad.
Haskell lets us use the `do` notation for any monad, so shelly gets a DSL syntax for free.

`Sh` can help us accomplish
* a thread-safe environment
* tracing execution
* custom behavior for each command run


## Custom behavior

Lets talk about the last point first. For scripting, we are focused on executing OS commands. But mostly we want to be able to execute external commands. So we can define a `run` command that takes a command and a list of arguments, executes them in `IO`, and returns the stdout.

~~~~~~~~ {.hs}
runIt :: FilePath -> [Text] -> IO Text
~~~~~~~~

This works well for the most common case. But what if we want to print the command to stdout when it executes, continue or exit the program if the command fails, or if we want stderr combined with stdout. Instead of defining new functions for the different possible parameters and return values, we can embed these options in the `Sh` monad.


## Thread-Safe environment

~~~~~~~~ {.hs}
chdir :: FilePath -> IO ()
~~~~~~~~

Some commands, like `chdir` will change the environment.
The current working directory is a global mutable variable: multiple threads will just clobber each other.
Instead, Shelly places a copy of the working directory and environment variables in its monad.
The Shelly apis use these values, leaving the global variable alone.
When creating a new thread, simply enter a new `Sh` monad, and you have a new thread-safe copy of global data.


## Tracing Execution

Haskell doesn't have stack traces.
Shelly instead logs the executed commands, which is often better.
The log is stored in the monad and written out to a file on error.



## Monad definition

~~~~~~~~ {.hs}
data State = State { code :: Int                -- ^ return code from last command
                   , stdin :: Maybe Text        -- ^ stdin for the command to be run
                   , stderr :: Text
                   , directory :: FilePath      -- ^ working directory
                   , printStdout :: Bool        -- ^ print stdout of command that is executed
                   , printCommands :: Bool      -- ^ print command that is executed
                   , environment :: Environment -- ^ environment variables
                   , tracing :: Bool            -- ^ trace executed commands with the 'trace' field
                   , trace :: B.Builder         -- ^ store a trace of executed commands
                   , errExit :: Bool            -- ^ throw an exception when a command returns an error
                   }
~~~~~~~~

`State` holds the state of our environment. `errExit`, `printStdout`, and `printCommands` all configure how running a shell command behaves.
We can set `stdin` before running a command, and we can inspect information about the last executed commands such as `stderr` and `code`.
We hold a copy of the `Environment` variables in `environment` and the working directory in `directory`.

Here is the Monad that shelly calls ShIO:

~~~~~~~~ {.hs}
import Control.Monad.Reader

type Sh a = ReaderT (IORef State) IO a
~~~~~~~~


Actually, this gives a nicer error message of 'Sh' rather than mentioning '`eaderT`

~~~~~~~~ {.hs}
newtype Sh a = Sh {
      unSh :: ReaderT (IORef State) IO a
  } deriving (Applicative, Monad, MonadIO, MonadReader (IORef State), Functor)
~~~~~~~~

`a` is a type parameter: it allows any type. The `a` on the left is used to fill in the `a` on the right.
`IORef` is a mutable variable: what most programming languages would call a variable.
`ReaderT` is the beginning of our `Monad definition. It lets us define an execution environment that can `ask` for access to the `IORef State` variable. T means Tranformer. The transformer is placed on top of the `IO` Monad, which we can still easily access by using the `liftIO` function. Internally we can update the `State` variable with the new state using a `put` function.

~~~~~~~~ {.hs}
put :: State -> Sh ()
put state = do
  stateVar <- ask
  liftIO (writeIORef stateVar state)
~~~~~~~~

`writeIORef` is how we update our mutable `IORef State variable.
This code may seem verbose, but that is OK because these are library internals, and we want to be very explicit about when we are modifying state. The mutation effects must be done in `IO` using `liftIO`.
The library came to me using an IORef, so I just kept it that way, but it might make more sense to use the State Monad, which accomplishes the same thing.


## running a command

Now we can finally look at shelly's `run` command:

~~~~~~~~ {.hs}
run :: FilePath -> [Text] -> ShIO Text
run = do
   stateVar <- ask
   state <- liftIO (readIORef stateVar)

   -- for brevity we omit the following:
   -- * check the state options such as what exactly should be printed
   -- * run the command, sending it any stdin from state
   -- * read stdout and stderr handles

   -- clear the stdin and save the stderr from the command and the exit code
   put state { stdin = Nothing, stderr = err, code = n }
   return stdout
~~~~~~~~

With this new definition of `run`, we have a single interface.
Under the hood we have `State` that we are carefully mutating, but we will expose a nice interface to the user so they never directly modify `State`. Most modifications are made for the duration of a function. We can write `verbosely ...`, where only commands executing in `verbosely` have the verbose configuration

~~~~~~~~ {.hs}
verbosely :: Sh a -> Sh a
verbosely a = sub $ modify (λx → x { sPrintStdout = True, sPrintCommands = True }) » a

modify :: (State -> State) -> Sh ()
modify f = do
  state ← ask
    liftIO (modifyIORef state f)
~~~~~~~~

`verbosely` uses the `sub` function, and modify, which is similar to `put`.
This function restores the original state when it is completed.

~~~~~~~~ {.hs}
sub :: ShIO a -> ShIO a
sub action = do
  stateVar <- ask
  original_state <- liftIO (readIORef stateVar)
  result <- action
  put original_state
  return result
~~~~~~~~

## executing the Monad

~~~~~~~~ {.hs}
runSh :: Sh a -> IORef State -> IO a
runSh = runReaderT . unSh

shelly = do
  environment <- liftIO getEnvironment
  dir <- liftIO getWorkingDirectory
  let def  = State { sCode = 0
                   , sStdin = Nothing
                   , sStderr = LT.empty
                   , sPrintStdout = True
                   , sPrintCommands = False
                   , sRun = runCommand
                   , sEnvironment = environment
                   , sTracing = True
                   , sTrace = B.fromText ""
                   , sDirectory = dir
                   , sErrExit = True
                   }
  stref <- liftIO $ newIORef def
  liftIO $ runSh (action `catches_sh` ...) stref
~~~~~~~~


## Conclusion

Petr Rockai originally wrote the Shellish library with basically the same monad.
Shelly is a fork with lots of changes and improvements, but the Monad operates exactly the same.

I don't know much about designing a monad. I should probably take the time to make some improvements to it, such as using a real State monad.
I hope this shows you how easy it is to create a custom environment/DSL with a Monad and that it doesn't need to be perfect.
