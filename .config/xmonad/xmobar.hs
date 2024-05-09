import XMobar.Bars.MainBar (mainBar)
import Xmobar (XPosition (TopH), configFromArgs, xmobar)

main :: IO ()
main = do
  config <- configFromArgs . mainBar . TopH $ 22
  xmobar config
