-- Adapted from:
--     https://codeberg.org/xmobar/xmobar/src/branch/master/src/Xmobar/Plugins/ArchUpdates.hs
-- This feature was not included in the release of xmobar yet at the time of writing this

module XMobar.Monitors.ArchUpdates
  ( ArchUpdates (..),
    ArchUpdatesOptions (..),
  )
where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Xmobar (Exec (..), Rate)

data ArchUpdatesOptions = ArchUpdatesOptions
  { none :: String,
    one :: String,
    many :: String,
    err :: String,
    icon :: String
  }
  deriving (Read, Show)

data ArchUpdates = ArchUpdates ArchUpdatesOptions String Rate
  deriving (Read, Show)

instance Exec ArchUpdates where
  alias (ArchUpdates _ a _) = a
  rate (ArchUpdates _ _ r) = r
  run (ArchUpdates ArchUpdatesOptions {none, one, many, err, icon} _ _) = do
    (exitCode, stdout, _) <- readProcessWithExitCode "checkupdates" [] ""
    output <-
      ( do
          return $ case exitCode of
            ExitFailure 1 -> err
            ExitFailure 2 -> none
            ExitSuccess -> case length . lines $ stdout of
              0 -> err
              1 -> one
              n -> many >>= \c -> if c == '%' then show n else pure c
            _ -> err
        )
    return (icon ++ " " ++ output)
