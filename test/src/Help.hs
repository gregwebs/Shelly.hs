module Help (
  with_dir, within_dir,
  (@==)
) where

import Shelly
import Test.HUnit
import Control.Monad.Trans ( MonadIO )

(@==) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
(@==) a b = liftIO (a @?= b)

with_dir :: FilePath -> Sh a -> Sh a
with_dir d action =
  mkdir_p d >> (action `finally_sh` rm_rf d)

within_dir :: FilePath -> Sh a -> Sh a
within_dir d action =
  with_dir d $ chdir d action
