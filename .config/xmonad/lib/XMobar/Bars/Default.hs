module XMobar.Bars.Default
  ( myDefaultConfig,
  )
where

import Colors qualified (alpha, bgColor, black, fgColor)
import Defaults qualified (xftfont')
import GHC.Float (float2Int)
import Xmobar
  ( Border (NoBorder),
    Config (..),
    TextOutputFormat (Plain),
    defaultConfig,
  )

myDefaultConfig :: Config
myDefaultConfig =
  defaultConfig
    { font = Defaults.xftfont' 13,
      additionalFonts = ["Noto Color Emoji"],
      dpi = 96,
      bgColor = Colors.bgColor,
      fgColor = Colors.fgColor,
      alpha = float2Int $ Colors.alpha * 255,
      textOffset = 0,
      textOffsets = [],
      iconOffset = 0,
      border = NoBorder,
      borderColor = Colors.black,
      borderWidth = 1,
      -- , iconRoot = "/"

      lowerOnStart = True,
      hideOnStart = False,
      allDesktops = False,
      overrideRedirect = True,
      pickBroadest = False,
      persistent = False,
      textOutput = False,
      textOutputFormat = Plain,
      sepChar = "%",
      alignSep = "}{"
    }
