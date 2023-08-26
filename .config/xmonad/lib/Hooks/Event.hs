module Hooks.Event
  ( Hooks.Event.handleEventHook,
  )
where

import Data.Default.Class (Default (def))
import Data.Monoid (All)
import Graphics.X11.Xlib.Extras (Event)
import XMonad.Core (X, handleEventHook)
import XMonad.Util.Hacks qualified as Hacks
  ( trayerAboveXmobarEventHook,
    trayerPaddingXmobarEventHook,
    windowedFullscreenFixEventHook,
  )

handleEventHook :: Event -> X All
handleEventHook =
  XMonad.Core.handleEventHook def
    <> Hacks.windowedFullscreenFixEventHook
    <> Hacks.trayerAboveXmobarEventHook
    <> Hacks.trayerPaddingXmobarEventHook
