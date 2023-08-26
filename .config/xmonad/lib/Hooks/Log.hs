module Hooks.Log
  ( logHook,
  )
where

import XMobar.PrettyPrinter (xmobarPP)
import XMonad.Core (X)
import XMonad.Hooks.DynamicLog (dynamicLogString, xmonadPropLog)

logHook :: X ()
logHook = dynamicLogString xmobarPP >>= xmonadPropLog
