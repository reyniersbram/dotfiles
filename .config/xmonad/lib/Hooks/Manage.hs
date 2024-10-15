module Hooks.Manage
  ( Hooks.Manage.manageHook,
  )
where

import XMonad (def)
import XMonad.Core (ManageHook, manageHook)
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.ManageHook (appName, doFloat, doShift, (-->), (=?), (<+>), className)
import XMonad.Hooks.ManageDocks

manageHook :: [String] -> ManageHook
manageHook workspaces =
  XMonad.Core.manageHook def
    <+> manageDocks
    <> (isDialog --> doFloat)
    <> (appName =? "Places" --> doFloat) -- Firefox Pop-ups
    <> (appName =? "xcalc" --> doFloat)
    <> (className =? "thunderbird" --> doShift (workspaces !! 6))
    <> (appName =? "discord" --> doShift (workspaces !! 7))
    <> (appName =? "spotify" --> doShift (workspaces !! 8))
