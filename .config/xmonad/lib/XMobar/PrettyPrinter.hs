module XMobar.PrettyPrinter
  ( xmobarPP,
  )
where

import Colors qualified
import XMonad.Hooks.DynamicLog
  ( PP (..),
    def,
    shorten,
    wrap,
    xmobarBorder,
    xmobarColor,
    xmobarRaw,
    xmobarStrip,
  )
import XMonad.Hooks.StatusBar.PP (xmobarFont)
import XMonad.Util.Loggers (logTitles)

type ColorWrapper = String -> String

xmobarPP :: PP
xmobarPP =
  def
    { ppCurrent =
        highlight
          . wrapWorkspaceName
          . xmobarFont 1
          . xmobarBorder "Bottom" Colors.blue 3,
      ppVisible = wrapWorkspaceName . xmobarBorder "Bottom" Colors.blue 3,
      ppHidden = wrapWorkspaceName . xmobarFont 1 . white,
      ppHiddenNoWindows = lowWhite . wrapWorkspaceName,
      -- ppVisibleNoWindows = def,
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      -- , ppRename = def
      ppSep = magenta " • ",
      ppWsSep = " ",
      ppTitle = id,
      ppTitleSanitize = xmobarStrip,
      ppLayout = id,
      ppOrder = order,
      -- , ppSort = def
      ppExtras = [logTitles formatFocused formatUnfocused]
      -- , ppOutput = def -- useless when working with dynamicLogString
      -- , ppPrinters = def
    }

wrapWorkspaceName :: String -> String
wrapWorkspaceName = wrap "" ""

wrapColored :: ColorWrapper -> String -> String -> String -> String
wrapColored color prefix postfix = wrap (color prefix) (color postfix)

formatFocused, formatUnfocused :: String -> String
formatFocused = wrapColored white "[" "]" . magenta . ppWindow
formatUnfocused = wrapColored lowWhite "[" "]" . cyan . ppWindow

ppWindow :: String -> String
ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 25

order :: [String] -> [String]
order (ws : l : _ : wins : _) = [ws, l, wins]
order sections = sections

cyan, lowWhite, magenta, red, white, yellow, highlight :: String -> String
magenta = xmobarColor Colors.magenta ""
cyan = xmobarColor Colors.cyan ""
white = xmobarColor Colors.white ""
yellow = xmobarColor Colors.yellow ""
red = xmobarColor Colors.red ""
lowWhite = xmobarColor Colors.color15 ""
highlight = xmobarColor Colors.fgHLight ""
