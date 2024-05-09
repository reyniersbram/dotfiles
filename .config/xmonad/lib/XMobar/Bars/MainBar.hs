module XMobar.Bars.MainBar where

import XMobar.Bars.Default (myDefaultConfig)
import XMobar.Monitors
  ( battery,
    brightness,
    conservationStatus,
    sound,
    traypadding,
  )
import Xmobar
  ( Config (commands, position, sepChar, template, wmClass, wmName),
    Runnable (Run),
    StdinReader (UnsafeStdinReader),
    XPosition,
  )

mainBar :: XPosition -> Config
mainBar position =
  myDefaultConfig
    { position = position,
      wmClass = "xmobar",
      wmName = "xmobar-main",
      commands =
        [ Run UnsafeStdinReader,
          Run sound,
          Run . brightness $ 1,
          Run . battery $ 50,
          Run . conservationStatus $ 10,
          Run . traypadding $ 1
        ],
      sepChar = "%",
      template =
        "\
        \\xf31a %UnsafeStdinReader%\
        \ }{ \
        \\xf00e0 %bright%\
        \ · \
        \<action=`amixer sset Master toggle`>%alsa:default:Master%</action>\
        \ | \
        \%battery%\
        \ · \
        \<action=`conservation_mode toggle`>%conservation_status%</action>\
        \%traypadding%\
        \"
    }
