-- | Suppose we have a directory named "pictures".
-- We want to copy all files with specified extensions.
-- So that jpgs go in one directory and pngs in the other.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Control.Applicative

import Shelly.Pipe
import Data.Text.Lazy as LT
default (LT.Text)

main = shs $ mapM_ proc exts

-- | Extensions to find.
exts :: [Text]
exts = ["png", "jpg", "tiff"]

-- | Directory to look at.
pictures :: FilePath
pictures = "pictures"

proc :: Text -> Sh ()
proc ext = do
    mkdir_p ext'
    findExt ext pictures >>= flip cp ext'
    where ext' = fromText ext

findExt a = findWhen (pure . hasExt a)
