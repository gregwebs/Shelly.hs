#!/usr/bin/env runhaskell

I have two laptops connected to monitors of different sizes in two
different rooms.  I always start out with a terminal, Emacs, and
Firefox when I boot up.  I got tired of putting the windows in the
right place every time, and wanted to write a little script that takes
care of that for me.  Using the shell for this turned out to be a
little more complicated than I wanted, since I'm not that familiar
with bash/zsh programming, but quite comfortable with Haskell.  I had
heard of the shelly package, and not because Percy Bysshe is one of my
favorite poets, I decided to give it a try.  I found the examples a
little lacking, so I cobbled this together and hope that it can help
others who want to use Haskell to do the things a shell should do.


Shelly tells you to start with this, so here it is:

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ExtendedDefaultRules #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}
> import Shelly
> import qualified Data.Text as T
> import Data.Text (Text)
> default (T.Text)

Sometimes I can't remember exactly what a function does (especial the
order of the argument) so I just paste the type signature in the
source.

> -- runFoldLines :: a -> FoldCallback a -> FilePath -> [Text] -> Sh a
> -- type FoldCallback a = a -> Text -> a 

> wanted :: [Text]
> wanted = ["terminology",  "emacs", "firefox"] -- The apps I want to run
>
> xps13WindowLocations, xpsWindowLocations :: [(Int,Text)]
> xps13WindowLocations = zip [0..] [
>     "0,  12,38,1894,1012"
>  ,  "0,1967,26,2512,1390"
>  ,  "0,1921,16,2541,1349"
>  ]
> 
> xpsWindowLocations = zip [0..] [
>     "0,  12,38,1894,1012"
>  ,  "0,1967,30,3404,1390"
>  ,  "0,1921,16,3400,1360"
>  ]

> main = do
> --  shelly $ verbosely $ do
>   shelly $ silently $ do
>     c <- concat <$> mapM runIfNotRunning wanted -- if they aren't already running, fire them up
>     echo (T.pack . show $ c)
>     when ((not . null) c) (sleep 5)  -- it takes emacs a while to start up

Depending upon which machine I'm running on, I need different monitor
and window parameters.  Getting the window id of a running app is not
completely trivial.  First I collect the window Ids of all the windows
with "wmctrl -l" and then get the WM_CLASS of that window with "xprop
-id <windowId> | grep WM_CLASS" The text returned by that command
should contain the app name, which is searched with the function
hasApp.  The end result is that windowIdList contains a list of window
Ids in the same order ad the app names in the wanted list above.

>     host    <- run "hostname" []
>     wids    <- wmctl collectWindowIds
>     appNames <- mapM getClass wids
>     let zipped = zip wids appNames
>     -- liftIO $ mapM_ print zipped
>     let windowIdList = windowIds zipped
>     -- echo . T.unwords $ windowIdList
>     case host of   -- Note the "\n" after the host names.  That's what you get back from <run "hostname" []> above
>       "xps13\n" -> do
>             escaping False $ cmd "/usr/bin/xrandr"
>                                  "--output eDP1 --mode 1920x1080 --left-of DP1 --output DP1 --mode 2560x1440"
>             mapM_ (moveWindowsAround windowIdList) xps13WindowLocations
>       "xps\n" -> do
>             escaping False $ cmd "/usr/bin/xrandr"
>                                  "--output eDP1 --mode 1920x1080 --left-of DP1 --output DP1 --mode 3440x1440"
>             mapM_ (moveWindowsAround windowIdList) xpsWindowLocations
>       otherwise -> echo $ T.unwords ["bad host",  host]
>   where
>     collectWindowIds a b = (head . T.words $ b):a  -- break into words and get the first one
>     hasApp :: Text -> (a, Text) -> Bool
>     hasApp x w  = x `T.isInfixOf` (snd w)          -- does the x passed in match?
>     windowIdOfApp :: [(c, Text)] -> Text -> c      -- 
>     windowIdOfApp ws x  = fst . head . (filter (hasApp x)) $ ws
>     windowIds :: [(b, Text)] -> [b]
>     windowIds ws = map (windowIdOfApp ws) wanted

The output of wmctrl looks like this:
0x00400002  0 xps henry@xps: wmctrl -l
0x00c0013e  0 xps emacs@xps
0x00e0002b  0 xps (1) Why Liquid Haskell matters - Tweag : haskell — Mozilla Firefox

I only want the first column, namely the window Ids.  The wmctrl
function I define uses a fold with collectWindowIds over the lines
output by the "wmctrl -l" program to grab the window Ids

> 
> wmctl :: FoldCallback [a] -> Sh [a]
> wmctl f =  runFoldLines [] f "wmctrl" ["-l"]
>

Look for the WM_CLASS line of the output of the "xprop -id <x>"
program.  In my case they look likes this:

WM_CLASS(STRING) = "Navigator", "firefox"
WM_CLASS(STRING) = "main", "terminology"
WM_CLASS(STRING) = "emacs", "Emacs"

> getClass :: Text -> Sh Text
> getClass x = (run "xprop" ["-id", x]) -|- run "grep" ["WM_CLASS"]
> 
> ps :: Sh Text
> ps = run "ps" ["a"]

Given a program name, see if it is already running.  Uses "ps a"
commands and checks if the given <x> string exists in the "ps a"
output

> isRunning :: Text -> Sh Bool
> isRunning x = ps >>= return . T.isInfixOf x

If the program is already running, return an empty list, otherwise
return a non-empty list.  An easy way to check if ALL of the programs
are already running so we won't have to sleep waiting for them

> runIfNotRunning :: Text -> Sh [Int]
> runIfNotRunning name = do
>   running <- isRunning name
>   if running
>     then return []
>     else ((asyncSh $ run_ (fromText name) [])   >> return [1])

> moveWindowsAround :: [Text] -> (Int, Text) -> Sh ()
> moveWindowsAround windowIdList (i,location) =
>   run_ "wmctrl" ["-i", "-r", windowIdList!!i, "-e", location]

