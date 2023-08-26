module KeyBinds
  ( modKey,
    keyBinds,
    mouseBinds,
  )
where

import Data.Default.Class (Default (def))
import Data.Map (Map)
import Data.Map qualified as Map
import Defaults (defaultBrowser, defaultTerminal)
import GHC.Bits ((.|.))
import Graphics.X11.Types
  ( Button,
    ButtonMask,
    KeyMask,
    KeySym,
    Window,
    mod4Mask,
    shiftMask,
    xK_Return,
    xK_b,
    xK_p,
    xK_s,
    xK_z,
  )
import XMonad (Layout, XConfig (keys, mouseBindings))
import XMonad.Core (X, spawn)
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts))
import XMonad.Operations (sendMessage)
import XMonad.Prompt.Pass (passPrompt)
import XMonad.Util.Ungrab (unGrab)
import XPConfig (myXPConfig)

-- mod4Mask: Super-key
modKey :: KeyMask
modKey = mod4Mask

keyBinds :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
keyBinds conf =
  Map.fromList
    [ ((modKey, xK_b), spawn defaultBrowser),
      ((modKey .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"),
      ((modKey .|. shiftMask, xK_s), unGrab *> spawn "scrot $HOME/Pictures/Screenshots/%Y-%m-%d.png -s"),
      ((modKey, xK_s), sendMessage ToggleStruts),
      ((modKey .|. shiftMask, xK_Return), return ()),
      ((modKey, xK_Return), spawn defaultTerminal),
      ((modKey .|. shiftMask, xK_p), passPrompt myXPConfig)
    ]
    `Map.union` keys def conf

mouseBinds :: XConfig Layout -> Map (ButtonMask, Button) (Window -> X ())
mouseBinds conf = Map.empty `Map.union` mouseBindings def conf
