module Bars.Default (
    myDefaultConfig
) where

import Xmobar
import qualified Xmobar as Xmobar.Config.Types
import Colors
import GHC.Float (float2Int)

myDefaultConfig :: Config
myDefaultConfig = defaultConfig
    { font = "Hack Nerd Font 13"
    , additionalFonts = ["Noto Color Emoji"]
    , dpi = 96

    , Xmobar.Config.Types.bgColor = Colors.bgColor
    , Xmobar.Config.Types.fgColor = Colors.fgColor
    , Xmobar.Config.Types.alpha = float2Int $ Colors.alpha * 255

    , textOffset = 0
    , textOffsets = []
    , iconOffset = 0

    , border = NoBorder
    , borderColor = black
    , borderWidth = 1

    -- , iconRoot = "/"

    , lowerOnStart = True
    , hideOnStart = False
    , allDesktops = False
    , overrideRedirect = True
    , pickBroadest = False
    , persistent = False

    , textOutput = False
    , textOutputFormat = Plain

    , sepChar = "%"
    , alignSep = "}{"
    }
