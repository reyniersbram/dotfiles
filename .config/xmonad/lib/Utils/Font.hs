module Utils.Font
  ( bold,
    size,
    xft,
    xftsize,
  )
where

bold :: String -> String
bold = flip (++) " Bold"

size :: Int -> String -> String
size = flip (++) . (++) " " . show

xft :: String -> String
xft = (++) "xft:"

xftsize :: Int -> String -> String
xftsize = flip (++) . (++) "-" . show
