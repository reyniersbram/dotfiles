module XMobar.Bars.MainBar where

import ColorTheme.CatpuccinMocha (yellow)
import XMobar.Bars.Default (myDefaultConfig)
import XMobar.Monitors
  ( archUpdates,
    battery,
    conservationStatus,
    date,
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
          Run . battery $ 50,
          Run . conservationStatus $ 10,
          Run . traypadding $ 1,
          Run . date $ 100,
          Run . archUpdates $ 6000
        ],
      sepChar = "%",
      template =
        "\
        \\xf31a %UnsafeStdinReader%\
        \ }{ \
        \"
          ++ "<fn=1>%date%</fn>"
          ++ "<hspace=40/>"
          ++ "<action=`kitty --hold sudo pacman -Syyu`><fc="
          ++ yellow
          ++ ">%pacupdates%</fc></action>"
          ++ "<hspace=40/>"
          ++ "\
             \<action=`conservation_mode toggle`>%conservation_status%</action>\
             \· \
             \%battery%\
             \ | \
             \%traypadding%\
             \"
    }
