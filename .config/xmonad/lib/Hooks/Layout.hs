module Hooks.Layout
  ( layoutHook,
  )
where

import Graphics.X11 (Window)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts)
import XMonad.Layout (Choose, Full (..), Mirror (..), Tall (..), (|||))
import XMonad.Layout.Decoration (ModifiedLayout)

layoutHook :: ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)) Window
layoutHook = avoidStruts (tall ||| Mirror tall ||| Full)
  where
    tall = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in master pane
    delta = 3 / 100 -- Percent of screen on resize
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
