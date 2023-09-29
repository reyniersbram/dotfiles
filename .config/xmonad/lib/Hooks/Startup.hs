module Hooks.Startup
  ( startupHook,
  )
where

import XMonad.Core (X, spawn)
import XMonad.Util.SpawnOnce (spawnOnce)

startupHook :: X ()
startupHook = do
  spawnOnce "nitrogen --restore"
  spawn "xmobar.sh"
  spawnOnce $ "trayer " ++ unwords trayerConfig

trayerConfig :: [String]
trayerConfig =
  [ "--edge",
    "top",
    "--align",
    "right",
    "--width",
    "7",
    "--height",
    "22",
    "--iconspacing",
    "10",
    "--tint",
    "0x0c0c0c",
    "--transparent",
    "true",
    "--alpha",
    "0",
    "--expand",
    "true",
    "--padding",
    "10",
    "--SetDockType",
    "true",
    "--SetPartialStrut",
    "true",
    "-l"
  ]
