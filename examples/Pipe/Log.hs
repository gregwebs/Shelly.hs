-- | Log filenames in current directory sorted by name.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Control.Applicative
import Data.List(sort)

import Shelly.Pipe
import Data.Text.Lazy as LT
default (LT.Text)

main = shs $ do
    makeLog
    appendfile logFile . cons '\n' =<< liftSh sort (lsT ".")

logFile = "log"

makeLog =
    unlessM (test_f logFile)
        (touchfile logFile)
