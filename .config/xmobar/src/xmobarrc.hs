import Xmobar

import Bars.SystemStatus
import Bars.MainBar (mainBar)

main :: IO ()
main = xmobar mainBar
