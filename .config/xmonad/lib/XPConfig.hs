module XPConfig
  ( myXPConfig,
  )
where

import Colors qualified (color8, fgColor, fgHLight, white)
import Defaults qualified (xftfont')
import Graphics.X11.Types (xK_Tab, xK_grave)
import XMonad.Prompt
  ( ComplCaseSensitivity (..),
    XPConfig (..),
    XPPosition (..),
    def,
    defaultXPKeymap,
    deleteAllDuplicates,
  )
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)

myXPConfig :: XPConfig
myXPConfig =
  def
    { font = Defaults.xftfont' 14,
      bgColor = Colors.color8,
      fgColor = Colors.fgColor,
      fgHLight = Colors.fgHLight,
      bgHLight = Colors.color8,
      borderColor = Colors.white,
      promptBorderWidth = 1,
      position = CenteredAt {xpCenterY = 0.30, xpWidth = 0.40},
      alwaysHighlight = False,
      height = 25,
      maxComplRows = Just 10,
      maxComplColumns = Just 3,
      historySize = 256,
      historyFilter = deleteAllDuplicates,
      promptKeymap = defaultXPKeymap,
      completionKey = (0, xK_Tab),
      changeModeKey = xK_grave,
      defaultText = "",
      autoComplete = Nothing,
      showCompletionOnTab = False,
      complCaseSensitivity = CaseInSensitive,
      searchPredicate = fuzzyMatch, -- fuzzy finding overrides case sensitivity
      defaultPrompter = id,
      sorter = fuzzySort
    }
