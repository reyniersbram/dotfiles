import XMonad.Main (xmonad)

import XMonad.Core (spawn, X, ManageHook, XConfig (..))

import XMonad.Layout (Choose, Tall (..), Mirror (..), Full (..), (|||))

import XMonad.Operations (sendMessage)

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.Loggers (logTitles)

import XMonad.ManageHook (doFloat, composeAll, (-->))

import XMonad.Hooks.StatusBar (xmonadPropLog)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.DynamicLog
    ( PP (..)
    , wrap, shorten
    , xmobarRaw
    , xmobarColor
    , xmobarBorder
    , xmobarStrip
    , dynamicLogString
    )
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts (ToggleStruts))

import XMonad.Util.SpawnOnce (spawnOnce)
import qualified XMonad.Util.Hacks as Hacks

import Data.Monoid (All)
import Data.Default.Class (Default (def))

import Graphics.X11.Types (KeyMask, mod4Mask)
import Graphics.X11.Xlib.Extras (Event)

import Utils (join)

-- Default Mod-key
-- mod4Mask: Super-key
modKey :: KeyMask
modKey = mod4Mask

-- Default terminal
defaultTerminal :: String
defaultTerminal = "kitty"

-- Default browser
defaultBrowser :: String
defaultBrowser = "firefox"

-- XMobar Pretty Printer
myXmobarPP :: PP
myXmobarPP = def
    { ppCurrent = wrap " " "" . xmobarBorder "Bottom" "#8be9fd" 2
    -- , ppVisible = def
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    -- , ppVisibleNoWindows = def
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    -- , ppRename = def
    , ppSep = magenta " • "
    , ppWsSep = " "
    , ppTitle = id
    , ppTitleSanitize = xmobarStrip
    , ppLayout = id
    , ppOrder = order
    -- , ppSort = def
    , ppExtras = [logTitles formatFocused formatUnfocused]
    -- , ppOutput = def -- useless when working with dynamicLogString
    -- , ppPrinters = def
    }
    where
        formatFocused, formatUnfocused :: String -> String
        formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
        formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

        ppWindow :: String -> String
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 25

        order :: [String] -> [String]
        order (ws:l:_:wins:_) = [ws, l, wins]
        order sections = sections

blue, lowWhite, magenta, red, white, yellow :: String -> String
magenta = xmobarColor "#ff79c6" ""
blue = xmobarColor "#bd93f9" ""
white = xmobarColor "#f8f8f2" ""
yellow = xmobarColor "#f1fa8c" ""
red = xmobarColor "#ff55555" ""
lowWhite = xmobarColor "#bbbbbb" ""

trayerConfig :: [String]
trayerConfig =
    [ "--edge", "top"
    , "--align", "right"
    , "--width", "5"
    , "--height", "22"
    , "--iconspacing", "10"
    , "--tint", "0x0c0c0c"
    , "--transparent", "true"
    , "--alpha", "0"
    , "--expand", "true"
    , "--padding", "10"
    , "--SetDockType", "true"
    , "--SetPartialStrut", "true"
    , "-l"
    ]

-- Startup
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "nitrogen --restore"
    spawn "$XDG_CONFIG_HOME/xmobar/xmobar.sh"
    spawnOnce $ "trayer " ++ join trayerConfig " "

-- Layouts
myLayoutHook :: Choose Tall (Choose (Mirror Tall) Full) a
myLayoutHook = tiled ||| Mirror tiled ||| Full
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1         -- Default number of windows in master pane
        delta   = 3 / 100   -- Percent of screen on resize
        ratio   = 1 / 2     -- Default proportion of screen occupied by master pane

-- Events
myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def
    <> Hacks.windowedFullscreenFixEventHook
    <> Hacks.trayerAboveXmobarEventHook
    <> Hacks.trayerPaddingXmobarEventHook

-- Manage Hook
myManageHook :: ManageHook
myManageHook = composeAll
    [ isDialog --> doFloat
    ]

-- Main
main :: IO ()
main = do
    xmonad
        . ewmh
        . docks
        . Hacks.javaHack
        $ def
        { modMask = modKey
        , logHook = dynamicLogString myXmobarPP >>= xmonadPropLog
        , terminal = defaultTerminal
        , layoutHook = avoidStruts myLayoutHook
        , focusFollowsMouse = False
        , clickJustFocuses = False
        , handleEventHook = myHandleEventHook
        , manageHook = myManageHook
        , startupHook = myStartupHook
        }
        `additionalKeysP`
        [ ("M-b", spawn defaultBrowser)
        , ("M-S-z", spawn "xscreensaver-command -lock")
        , ("M-S-s", unGrab *> spawn "scrot $HOME/Pictures/Screenshots/%Y-%m-%d.png -s")
        , ("M-s", sendMessage ToggleStruts)
        ]
