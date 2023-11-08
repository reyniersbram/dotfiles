module XMobar.Bars.Default
  ( myDefaultConfig,
  )
where

import Colors qualified
import Defaults (defaultFont)
import GHC.Float (float2Int)
import Utils.Font (bold, size)
import Xmobar
  ( Border (NoBorder),
    Config (..),
    TextOutputFormat (Plain),
    defaultConfig,
  )

myDefaultConfig :: Config
myDefaultConfig =
  defaultConfig
    { font = size 13 defaultFont,
      additionalFonts = [size 13 . bold $ defaultFont, "Noto Color Emoji"],
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
