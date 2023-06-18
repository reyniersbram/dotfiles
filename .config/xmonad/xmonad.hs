import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts (ToggleStruts))
import qualified XMonad.Util.Hacks as Hacks
import Data.Monoid (All)

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
    { ppSep = magenta " • "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Bottom" "#8be9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = order
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
    where
        formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
        formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

        ppWindow :: String -> String
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 25

        order :: [String] -> [String]
        order (ws:l:_:wins:_) = [ws, l, wins]
        order a = a

blue, lowWhite, magenta, red, white, yellow :: String -> String
magenta = xmobarColor "#ff79c6" ""
blue = xmobarColor "#bd93f9" ""
white = xmobarColor "#f8f8f2" ""
yellow = xmobarColor "#f1fa8c" ""
red = xmobarColor "#ff55555" ""
lowWhite = xmobarColor "#bbbbbb" ""

-- Startup
myStartupHook :: X ()
myStartupHook = undefined

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

main :: IO ()
main = do
    -- spawn "killall xmobar"
    xmProc <- spawnPipe "$XDG_CONFIG_HOME/xmobar/xmobar.sh"
    -- xmprocTop <- spawnPipe "sleep 2 && xmobar $XDG_CONFIG_HOME/xmobar/xmobar.hs"
    -- xmprocTop <- spawnPipe "sleep 2 && xmobar $XDG_CONFIG_HOME/xmobar/xmobarrc"
    -- xmprocBottom <- spawnPipe "sleep 2 && xmobar $XDG_CONFIG_HOME/xmobar/sys_info_xmobarrc"
    xmonad
        . ewmh
        . docks
        $ def
        { modMask = modKey
        , logHook = dynamicLogString myXmobarPP >>= xmonadPropLog
        , terminal = defaultTerminal
        , layoutHook = avoidStruts myLayoutHook
        , focusFollowsMouse = False
        , clickJustFocuses = False
        , handleEventHook = myHandleEventHook
        -- , startupHook = myStartupHook
        }
        `additionalKeysP`
        [ ("M-b", spawn defaultBrowser)
        , ("M-S-z", spawn "xscreensaver-command -lock")
        , ("M-S-s", unGrab *> spawn "scrot $HOME/Pictures/Screenshots/%Y-%m-%d.png -s")
        , ("M-s", sendMessage ToggleStruts)
        ]
