module Hooks.Log
  ( logHook,
  )
where

import XMobar.PrettyPrinter (xmobarPP)
import XMonad.Core (X)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import GHC.IO.Handle (Handle)
-- import XMonad.Hooks.StatusBar.PP (filterOutWsPP)
-- import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

logHook :: [Handle] -> X ()
logHook = dynamicLogWithPP . xmobarPP
-- logHook = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] . xmobarPP
