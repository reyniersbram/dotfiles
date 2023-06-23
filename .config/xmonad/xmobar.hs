import Xmobar (Config, xmobar)

import Bars.SystemStatus (systemStatus)
import Bars.MainBar (mainBar)

import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

data Either3 a b c = Left a | Middle b | Right c

parseArgs :: [String] -> Either3 Config String ExitCode
parseArgs ("-h":_) = Main.Middle "Help"
parseArgs ("main":_) = Main.Left mainBar
parseArgs ("system":_) = Main.Left systemStatus
parseArgs _ = Main.Right $ ExitFailure 1

process :: Either3 Config String ExitCode -> IO ()
process  (Main.Left a) = xmobar a
process  (Main.Middle a) = print a
process  (Main.Right a) = exitWith a

main :: IO ()
main = getArgs >>= process  . parseArgs

