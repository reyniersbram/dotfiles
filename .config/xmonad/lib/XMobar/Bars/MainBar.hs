module XMobar.Bars.MainBar where

import Colors
import XMobar.Bars.Default
import Xmobar

mainBar :: Config
mainBar =
  myDefaultConfig
    { position = TopH 22,
      wmClass = "xmobar",
      wmName = "xmobar-main",
      commands =
        [ Run XMonadLog,
          Run $ Kbd [("us", "us")],
          Run $ Date "%H:%M · %a %Y-%m-%d" "date" 300,
          Run $
            Alsa
              "default"
              "Master"
              [ "--template",
                "<status><volume>",
                "--suffix",
                "True",
                "--",
                "--on",
                "",
                "--onc",
                Colors.fgColor,
                "--off",
                "\xf0581 ",
                "--offc",
                Colors.fgColor,
                "--lows",
                "\xf057f ",
                "--mediums",
                "\xf0580 ",
                "--highs",
                "\xf057e "
              ],
          Run $
            Battery
              [ "--template",
                "<acstatus>",
                -- Tresholds
                "--Low",
                "25", -- %
                "--High",
                "85", -- %
                "--bwidth",
                "15",
                -- Treshold Colors
                "--low",
                red,
                "--normal",
                "orange",
                "--high",
                green,
                "--", -- battery specific options
                -- AC "off" status (discharging)
                "-o",
                "\xf007e <left>% (<timeleft>)",
                -- AC "on" status (charging)
                "-O",
                "\xf0084 <left>%",
                -- AC "idle" status (charged)
                "-i",
                "\xf17e2 <left>%"
              ]
              50,
          Run $
            Com
              "/bin/bash"
              [ "-c",
                "conservation_mode icon"
              ]
              "conservation_status"
              10,
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
