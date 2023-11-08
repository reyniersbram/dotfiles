module XPConfig
  ( myXPConfig,
    myUnicodePrompt,
  )
where

import Colors qualified (color8, fgColor, fgHLight, white)
import Control.Arrow (first)
import Data.Map as Map (Map, fromList, union)
import Defaults qualified (defaultFont)
import Graphics.X11.Types
  ( KeyMask,
    KeySym,
    controlMask,
    xK_Tab,
    xK_b,
    xK_f,
    xK_grave,
    xK_n,
    xK_p,
  )
import Utils.Font (xft, xftsize)
import XMonad.Core (X)
import XMonad.Prompt
  ( ComplCaseSensitivity (..),
    Direction1D (Next, Prev),
    XP,
    XPConfig (..),
    XPPosition (..),
    def,
    defaultXPKeymap,
    deleteAllDuplicates,
    moveCursor,
    moveHistory,
  )
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Unicode (mkUnicodePrompt)
import XMonad.StackSet (focusDown', focusUp')

myXPConfig :: XPConfig
myXPConfig =
  def
    { font = xftsize 14 . xft $ Defaults.defaultFont,
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
      promptKeymap = keyMap,
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

keyMap :: Map (KeyMask, KeySym) (XP ())
keyMap =
  Map.fromList
    ( map
        (first $ (,) controlMask)
        [ (xK_b, moveCursor Prev),
          (xK_f, moveCursor Next),
          (xK_p, moveHistory focusUp'),
          (xK_n, moveHistory focusDown')
        ]
    )
    `Map.union` defaultXPKeymap

myUnicodePrompt :: XPConfig -> X ()
myUnicodePrompt = mkUnicodePrompt "xsel" ["-i", "-b"] "/home/reyniersbram/.local/share/xmonad/unicode_chars.txt"
