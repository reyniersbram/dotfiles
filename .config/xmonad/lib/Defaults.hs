module Defaults
  ( font,
    xftfont,
    xftfont',
  )
where

font :: String
font = "Hack Nerd Font"

xftfont :: String
xftfont = "xft:" ++ font

xftfont' :: Int -> String
xftfont' size = xftfont ++ "-" ++ show size
