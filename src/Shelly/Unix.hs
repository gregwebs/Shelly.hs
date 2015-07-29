{-# LANGUAGE OverloadedStrings #-}

-- | commands that only work on Unix
module Shelly.Unix
  ( kill
  ) where

import Shelly
import qualified Data.Text as T

kill :: Int -> Sh ()
kill pid = run_ "kill" ["-15", T.pack $ show pid]
