module XMobar.Bars.MainBar where

import XMobar.Bars.Default (myDefaultConfig)
import XMobar.Monitors
  ( battery,
    conservationStatus,
    date,
    keyboardLayout,
    sound,
  )
import Xmobar
  ( Config (commands, position, template, wmClass, wmName),
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
          Run keyboardLayout,
          Run sound,
          Run $ date 300,
          Run $ battery 50,
          Run $ conservationStatus 10,
          Run $ XPropertyLog "_XMONAD_TRAYPAD"
        ],
      template =
        "\
        \ \xf31a  %XMonadLog%\
        \ }\
        \{ \
        \%date%\
        \ | \
        \\xf030c %kbd%\
        \ | \
        \<action=`amixer sset Master toggle`>%alsa:default:Master%</action>\
        \ | \
        \%battery%\
        \ · \
        \<action=`conservation_mode toggle`>%conservation_status%</action>\
        \%_XMONAD_TRAYPAD%\
        \"
    }
