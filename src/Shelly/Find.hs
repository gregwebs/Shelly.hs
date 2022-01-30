{-# LANGUAGE OverloadedStrings #-}

-- | File finding utiliites for Shelly
-- The basic 'find' takes a dir and gives back a list of files.
-- If you don't just want a list, use the folding variants like 'findFold'.
-- If you want to avoid traversing certain directories, use the directory filtering variants like 'findDirFilter'

module Shelly.Find
  ( find
  , findWhen
  , findFold
  , findDirFilter
  , findDirFilterWhen
  , findFoldDirFilter
  )
where

import           Shelly.Base
import           Control.Monad                  ( foldM )
#if !MIN_VERSION_base(4,13,0)
import           Data.Monoid                    ( mappend )
#endif
import           System.PosixCompat.Files       ( getSymbolicLinkStatus
                                                , isSymbolicLink
                                                )
import           System.Directory               ( doesDirectoryExist )

-- | List directory recursively (like the POSIX utility "find").
-- listing is relative if the path given is relative.
-- If you want to filter out some results or fold over them you can do that with the returned files.
-- A more efficient approach is to use one of the other find functions.
find :: FilePath -> Sh [FilePath]
find = findFold (\paths fp -> return $ paths ++ [fp]) []

-- | 'find' that filters the found files as it finds.
-- Files must satisfy the given filter to be returned in the result.
findWhen :: (FilePath -> Sh Bool) -> FilePath -> Sh [FilePath]
findWhen = findDirFilterWhen (const $ return True)

-- | Fold an arbitrary folding function over files froma a 'find'.
-- Like 'findWhen' but use a more general fold rather than a filter.
findFold :: (a -> FilePath -> Sh a) -> a -> FilePath -> Sh a
findFold folder startValue =
  findFoldDirFilter folder startValue (const $ return True)

-- | 'find' that filters out directories as it finds.
-- Filtering out directories can make a find much more efficient by avoiding entire trees of files.
findDirFilter :: (FilePath -> Sh Bool) -> FilePath -> Sh [FilePath]
findDirFilter filt = findDirFilterWhen filt (const $ return True)

-- | Similar to 'findWhen', but also filter out directories.
-- Alternatively, similar to 'findDirFilter', but also filter out files.
-- Filtering out directories makes the find much more efficient.
findDirFilterWhen
  :: (FilePath -> Sh Bool) -- ^ directory filter
  -> (FilePath -> Sh Bool) -- ^ file filter
  -> FilePath -- ^ directory
  -> Sh [FilePath]
findDirFilterWhen dirFilt fileFilter = findFoldDirFilter filterIt [] dirFilt
 where
  filterIt paths fp = do
    yes <- fileFilter fp
    return $ if yes then paths ++ [fp] else paths

-- | Like 'findDirFilterWhen' but use a folding function rather than a filter.
-- The most general finder: you likely want a more specific one.
findFoldDirFilter
  :: (a -> FilePath -> Sh a) -> a -> (FilePath -> Sh Bool) -> FilePath -> Sh a
findFoldDirFilter folder startValue dirFilter dir = do
  absDir <- absPath dir
  trace ("find " `mappend` toTextIgnore absDir)
  filt <- dirFilter absDir
  if not filt
    then return startValue
    -- use possible relative path, not absolute so that listing will remain relative
    else do
      (rPaths, aPaths) <- lsRelAbs dir
      foldM traverse' startValue (zip rPaths aPaths)
 where
  traverse' acc (relativePath, absolutePath) = do
    -- optimization: don't use Shelly API since our path is already good
    isDir  <- liftIO $ doesDirectoryExist absolutePath
    sym    <- liftIO $ fmap isSymbolicLink $ getSymbolicLinkStatus absolutePath
    newAcc <- folder acc relativePath
    follow <- fmap sFollowSymlink get
    if isDir && (follow || not sym)
      then findFoldDirFilter folder newAcc dirFilter relativePath
      else return newAcc
