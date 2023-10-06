module XMobar.Bars.SystemStatus where

import XMobar.Bars.Default (myDefaultConfig)
import XMobar.Monitors (cpuFrequency, cpuTemperature, cpuUsage, diskIO, diskUsage, memory, networkIO, swap)
import Xmobar
  ( Config (commands, position, template, wmClass, wmName),
    Runnable (Run),
    XPosition,
  )

systemStatus :: XPosition -> Config
systemStatus position =
  myDefaultConfig
    { position = position,
      wmClass = "xmobar",
      wmName = "xmobar-systemstatus",
      commands =
        [ Run $ memory 10,
          Run $ swap 10,
          Run $ cpuUsage 10,
          Run $ cpuFrequency 10,
          Run $ cpuTemperature 10,
          Run $ diskIO 10,
          Run $ diskUsage 30000,
          Run $ networkIO 10
        ],
      template =
        "\
        \ }{ \
        \<action=`kitty -e htop`>\
        \%multicpu%\
        \ · \
        \%cpufreq%\
        \ · \
        \%multicoretemp%\
        \ | \
        \%memory%\
        \ · \
        \%swap%\
        \ | \
        \%disku%\
        \ · \
        \%diskio%\
        \ | \
        \%dynnetwork%\
        \</action>\
        \ "
    }
