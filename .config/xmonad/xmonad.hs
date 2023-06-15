import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- Default Mod-key
-- mod4Mask: Super-key
modKey :: KeyMask
modKey = mod4Mask

-- Default key to toggle struts
myToggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
myToggleStrutsKey XConfig{modMask = m} = (m, xK_s)

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
    , ppOrder = \(ws:l:_:wins:_) -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
    where
        formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
        formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

        ppWindow :: String -> String
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 25

blue, lowWhite, magenta, red, white, yellow :: String -> String
magenta = xmobarColor "#ff79c6" ""
blue = xmobarColor "#bd93f9" ""
white = xmobarColor "#f8f8f2" ""
yellow = xmobarColor "#f1fa8c" ""
red = xmobarColor "#ff55555" ""
lowWhite = xmobarColor "#bbbbbb" ""

-- Layouts
myLayoutHook :: Choose Tall (Choose (Mirror Tall) Full) a
myLayoutHook = tiled ||| Mirror tiled ||| Full
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1         -- Default number of windows in master pane
        delta   = 3 / 100   -- Percent of screen on resize
        ratio   = 1 / 2     -- Default proportion of screen occupied by master pane

main :: IO ()
main = xmonad .
        ewmhFullscreen .
        ewmh .
        withEasySB (statusBarProp "xmobar $HOME/.config/xmobar/xmobarrc" (pure myXmobarPP)) myToggleStrutsKey $
        def
    { modMask = modKey
    , terminal = defaultTerminal
    , layoutHook = myLayoutHook
    , focusFollowsMouse = False
    , clickJustFocuses = False
    }
    `additionalKeysP`
    [ ("M-b", spawn defaultBrowser)
    , ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-s", unGrab *> spawn "scrot $HOME/Pictures/Screenshots/%Y-%m-%d.png -s")
    ]
