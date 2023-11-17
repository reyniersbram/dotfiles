module XMobar.Bars.MainBar where

import XMobar.Bars.Default (myDefaultConfig)
import XMobar.Monitors
  ( battery,
    conservationStatus,
    sound, brightness,
  )
import Xmobar
  ( Config (commands, position, template, wmClass, wmName, sepChar),
    Runnable (Run),
    XMonadLog (XMonadLog, XPropertyLog),
    XPosition,
  )

mainBar :: XPosition -> Config
mainBar position =
  myDefaultConfig
    { position = position,
      wmClass = "xmobar",
      wmName = "xmobar-main",
      commands =
        [ Run XMonadLog,
          Run sound,
          Run $ brightness 1,
          Run $ battery 50,
          Run $ conservationStatus 10,
          Run $ XPropertyLog "_XMONAD_TRAYPAD"
        ],
      sepChar = "%",
      template =
        "\
        \ \xf31a  %XMonadLog%\
        \ }{ \
        \\xf00e0 %bright%\
        \ · \
        \<action=`amixer sset Master toggle`>%alsa:default:Master%</action>\
        \ | \
        \%battery%\
        \ · \
        \<action=`conservation_mode toggle`>%conservation_status%</action>\
        \%_XMONAD_TRAYPAD%\
        \"
    }
