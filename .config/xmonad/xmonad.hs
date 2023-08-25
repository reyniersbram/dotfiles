import XMonad.Main (xmonad)
import XMonad.Core (spawn, X, ManageHook, XConfig (..), Layout)
import XMonad.Layout (Choose, Tall (..), Mirror (..), Full (..), (|||))
import XMonad.Operations (sendMessage)
import XMonad.ManageHook (appName, doShift, (=?), doFloat, (-->))

import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.Loggers (logTitles)
import XMonad.Util.SpawnOnce (spawnOnce)
import qualified XMonad.Util.Hacks as Hacks

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

import Data.Monoid (All)
import Data.Default.Class (Default (def))
import Data.Map (Map)
import qualified Data.Map as Map

import GHC.Bits ((.|.))

import Graphics.X11.Types
    (shiftMask, mod4Mask
    , xK_b, xK_s, xK_z, xK_p, xK_Return
    , ButtonMask, KeySym, KeyMask, Button, Window
    )
import Graphics.X11.Xlib.Extras (Event)
import XMonad.Prompt.Pass (passPrompt)
import XPConfig (myXPConfig)

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
    spawnOnce $ "trayer " ++ unwords trayerConfig

-- Layouts
myLayoutHook :: Choose Tall (Choose (Mirror Tall) Full) a
myLayoutHook = tall ||| Mirror tall ||| Full
    where
        tall   = Tall nmaster delta ratio
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
myManageHook = manageHook def
    <> (isDialog --> doFloat)
    <> (appName =? "Places" --> doFloat) -- Firefox Popups
    <> (appName =? "discord" --> doShift (myWorkspaces !! 7))
    <> (appName =? "spotify" --> doShift (myWorkspaces !! 8))

-- Key Bindings
myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys conf = Map.fromList
    [ ((modKey, xK_b), spawn defaultBrowser)
    , ((modKey .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((modKey .|. shiftMask, xK_s), unGrab *> spawn "scrot $HOME/Pictures/Screenshots/%Y-%m-%d.png -s")
    , ((modKey, xK_s), sendMessage ToggleStruts)
    , ((modKey .|. shiftMask, xK_Return), return ())
    , ((modKey, xK_Return), spawn defaultTerminal)
    , ((modKey .|. shiftMask, xK_p), passPrompt myXPConfig)
    ]
    `Map.union` keys def conf

myMouseBindings :: XConfig Layout -> Map (ButtonMask, Button) (Window -> X ())
myMouseBindings conf = Map.empty `Map.union` mouseBindings def conf

-- Workspaces
myWorkspaces :: [String]
myWorkspaces = map show ([1 .. 7] :: [Integer])
    ++ [xmobarColor "#738adb" "" "\xf066f"] -- Discord
    ++ [xmobarColor "#1ed761" "" "\xf04c7"] -- Spotify

-- Main
main :: IO ()
main = do
    xmonad
        . ewmh
        . docks
        . Hacks.javaHack
        $ def
        { normalBorderColor = "#dddddd"
        , focusedBorderColor = "#ff0000"
        , terminal = defaultTerminal
        , layoutHook = avoidStruts myLayoutHook
        , manageHook = myManageHook
        , handleEventHook = myHandleEventHook
        , workspaces = myWorkspaces
        , modMask = modKey
        , keys = myKeys
        , mouseBindings = myMouseBindings
        , borderWidth = 1
        , logHook = dynamicLogString myXmobarPP >>= xmonadPropLog
        , startupHook = myStartupHook
        , focusFollowsMouse = False
        , clickJustFocuses = False
        -- , clientMask = undefined
        -- , rootMask = undefined
        -- , handleExtraArgs = undefined
        -- , extensibleConf = undefined
        }
