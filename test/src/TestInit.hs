module TestInit (module Export, isWindows) where

import Test.HUnit as Export hiding (path)
import Test.Hspec as Export
#ifdef LIFTED
import Shelly.Lifted as Export
#else
import Shelly as Export
#endif
import Test.Hspec.Contrib.HUnit ()
import System.Info(os)
isWindows :: Bool
isWindows = os == "mingw32"
