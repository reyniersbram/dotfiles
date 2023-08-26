import Colors qualified
import Data.Default.Class (Default (def))
import Defaults (defaultTerminal)
import Hooks.Event qualified
import Hooks.Layout qualified
import Hooks.Log qualified
import Hooks.Manage qualified
import Hooks.Startup qualified
import KeyBinds (keyBinds, modKey, mouseBinds)
import XMonad.Core (XConfig (..))
import XMonad.Hooks.DynamicLog (xmobarColor)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Main (xmonad)
import XMonad.Util.Hacks qualified as Hacks (javaHack)

-- Workspaces
myWorkspaces :: [String]
myWorkspaces =
  map show ([1 .. 7] :: [Integer])
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
      { normalBorderColor = Colors.white,
        focusedBorderColor = Colors.red,
        terminal = defaultTerminal,
        layoutHook = Hooks.Layout.layoutHook,
        manageHook = Hooks.Manage.manageHook myWorkspaces,
        handleEventHook = Hooks.Event.handleEventHook,
        workspaces = myWorkspaces,
        modMask = modKey,
        keys = keyBinds,
        mouseBindings = mouseBinds,
        borderWidth = 1,
        logHook = Hooks.Log.logHook,
        startupHook = Hooks.Startup.startupHook,
        focusFollowsMouse = False,
        clickJustFocuses = False
        -- , clientMask = undefined
        -- , rootMask = undefined
        -- , handleExtraArgs = undefined
        -- , extensibleConf = undefined
      }
