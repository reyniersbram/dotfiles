module Hooks.Layout
  ( layoutHook,
  )
where

import Graphics.X11 (Window)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts)
import XMonad.Layout (Choose, Full (..), Mirror (..), Tall (..), (|||))
import XMonad.Layout.Decoration (ModifiedLayout)
import XMonad.Layout.Spacing

-- To get the correct types: remove the type and let HLS infer it with a code
-- action

tall :: Tall Window
tall = Tall nmaster delta ratio
  where
    nmaster = 1 -- Default number of windows in master pane
    delta = 3 / 100 -- Percent of screen on resize
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane

layouts :: Choose Tall (Choose (Mirror Tall) Full) Window
layouts = tall ||| Mirror tall ||| Full

spacedLayouts :: ModifiedLayout Spacing (ModifiedLayout AvoidStruts Tall) Window
spacedLayouts = smartSpacing 5 . avoidStruts $ tall

layoutHook :: Choose (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full))) (ModifiedLayout Spacing (ModifiedLayout AvoidStruts Tall)) Window
layoutHook = avoidStruts layouts ||| spacedLayouts
