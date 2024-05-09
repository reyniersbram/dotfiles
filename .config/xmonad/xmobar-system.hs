import XMobar.Bars.SystemStatus (systemStatus)
import Xmobar (XPosition (BottomH), configFromArgs, xmobar)

main :: IO ()
main = do
  config <- configFromArgs . systemStatus . BottomH $ 22
  xmobar config
