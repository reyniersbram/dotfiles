module Hooks.Manage
  ( Hooks.Manage.manageHook,
  )
where

import XMonad (def)
import XMonad.Core (ManageHook, manageHook)
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.ManageHook (appName, doFloat, doShift, (-->), (=?))

manageHook :: [String] -> ManageHook
manageHook workspaces =
  XMonad.Core.manageHook def
    <> (isDialog --> doFloat)
    <> (appName =? "Places" --> doFloat) -- Firefox Pop-ups
    <> (appName =? "discord" --> doShift (workspaces !! 7))
    <> (appName =? "spotify" --> doShift (workspaces !! 8))
