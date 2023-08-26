module Defaults
  ( font,
    xftfont,
    xftfont',
    defaultTerminal,
    defaultBrowser,
  )
where

font :: String
font = "Hack Nerd Font"

xftfont :: String
xftfont = "xft:" ++ font

xftfont' :: Int -> String
xftfont' size = xftfont ++ "-" ++ show size

-- Default terminal
defaultTerminal :: String
defaultTerminal = "kitty"

-- Default browser
defaultBrowser :: String
defaultBrowser = "firefox"
