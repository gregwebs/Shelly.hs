{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import Shelly
import System.Process (rawSystem)
import Control.Monad (void)
import Data.Text (Text)

default (Text)

main = shelly $ do
  void $ liftIO $ rawSystem "ls" ["--color=auto", "../dist"]
  run_ "ls" ["--color=auto", "../dist"]
