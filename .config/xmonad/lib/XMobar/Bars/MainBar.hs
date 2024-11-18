module XMobar.Bars.MainBar where

import ColorTheme.CatpuccinMocha (yellow)
import XMobar.Bars.Default (myDefaultConfig)
import XMobar.Monitors
  ( battery,
    conservationStatus,
    traypadding, pacmanUpdates,
    date,
  )
import Xmobar
  ( Config (commands, position, sepChar, template, wmClass, wmName),
    Runnable (Run),
    StdinReader (UnsafeStdinReader),
    XPosition,
  )
import ColorTheme.CatpuccinMocha (flamingo)

mainBar :: XPosition -> Config
mainBar position =
  myDefaultConfig
    { position = position,
      wmClass = "xmobar",
      wmName = "xmobar-main",
      commands =
        [ Run UnsafeStdinReader,
          Run . battery $ 50,
          Run . conservationStatus $ 10,
          Run . traypadding $ 1,
          Run . pacmanUpdates $ 3600
          Run . date $ 100,
        ],
      sepChar = "%",
      template =
        "\
        \\xf31a %UnsafeStdinReader%\
        \ }{ \
        \" ++
        "<action=`kitty --hold sudo pacman -Syyu`><fc=" ++ flamingo ++ ">\xf06b0 %pacUpdates%</fc></action>" ++
        "<hspace=40/>" ++
        "\
        \"
          ++ "<fn=1>%date%</fn>"
          ++ "<hspace=40/>"
          ++ "\
             \<action=`conservation_mode toggle`>%conservation_status%</action>\
             \· \
             \%battery%\
             \ | \
             \%traypadding%\
             \"
    }
