module PrintCommandsFnSpec (printCommandsFnSpec) where

import TestInit
import Data.IORef

printCommandsFnSpec :: Spec
printCommandsFnSpec = do
    describe "sPrintCommandsFn" $ do
      it "calls the custom print function" $ do
        calledRef <- newIORef False
        let printFn = \_ -> writeIORef calledRef True
        _ <- shelly $ print_commands True $ print_commands_with printFn $ run "echo" []
        called <- readIORef calledRef
        called @?= True
