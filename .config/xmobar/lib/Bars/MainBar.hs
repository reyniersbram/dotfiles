module Bars.MainBar where

import Bars.Default
import Xmobar
import Colors

mainBar :: Config
mainBar = myDefaultConfig
    { position = TopH 22
    , wmClass = "xmobar"
    , wmName = "xmobar-main"
    , commands =
        [ Run XMonadLog
        , Run $ Kbd [("us", "us")]
        , Run $ Date "%H:%M · %a %Y-%m-%d" "date" 300
        , Run $ Alsa "default" "Speaker"
            [ "--template", "<status><volume>"
            , "--suffix", "True"
            , "--"
            , "--on", ""
            , "--onc", Colors.fgColor
            , "--off", "\xf0581 "
            , "--offc", Colors.fgColor
            , "--lows", "\xf057f "
            , "--mediums", "\xf0580 "
            , "--highs", "\xf057e "
            ]
        , Run $ Battery
            [ "--template", "<acstatus>"
            -- Tresholds
            , "--Low", "25" -- %
            , "--High", "85" -- %

            , "--bwidth", "15"

            -- Treshold Colors
            , "--low", red
            , "--normal", "orange"
            , "--high", green

            , "--" -- battery specific options
            -- AC "off" status (discharging)
            , "-o", "\xf007e <left>% (<timeleft>)"
            -- AC "on" status (charging)
            , "-O", "\xf0084 <left>%"
            -- AC "idle" status (charged)
            , "-i", "\xf17e2 <left>%"
            ] 50

        , Run $ Com
            "/bin/bash"
            [ "-c"
            , "$XDG_CONFIG_HOME/xmobar/bin/conservation_mode_status.sh"
            ] "conservation_status" 10

        , Run $ Com
            "/bin/bash"
            [ "-c"
            , "$XDG_CONFIG_HOME/xmobar/bin/padding-icon.sh"
            ] "traypadding" 10
        ]
    , template = "\
        \ \xf31a %XMonadLog%\
        \ }\
        \{ \
        \%date%\
        \ | \
        \\xf030c %kbd%\
        \ | \
        \%alsa:default:Speaker%\
        \ | \
        \%battery%\
        \ · \
        \<action=`toggle-conservation-mode`>%conservation_status%</action>\
        \%traypadding%\
        \"
    } 
