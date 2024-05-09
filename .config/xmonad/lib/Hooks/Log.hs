module Hooks.Log
  ( logHook,
  )
where

import XMobar.PrettyPrinter (xmobarPP)
import XMonad.Core (X)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import GHC.IO.Handle (Handle)

logHook :: [Handle] -> X ()
logHook = dynamicLogWithPP . xmobarPP
