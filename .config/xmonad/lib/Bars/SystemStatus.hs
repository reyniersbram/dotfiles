module Bars.SystemStatus where

import Xmobar
import Bars.Default
import Colors

systemStatus :: Config
systemStatus = myDefaultConfig 
    { position = BottomH 22
    , wmClass = "xmobar"
    , wmName = "xmobar-systemstatus"
    , commands =
        [ Run $ Memory
            [ "--template", "\xf035b <used>/<total>G"
            , "--ddigits", "2"
            , "--width", "5"
            , "--Low", "4"
            , "--High", "8"
            , "--low", green
            , "--normal" , "orange"
            , "--high", red
            , "--"
            , "--scale", "1024"
            ] 10
        , Run $ Swap
            [ "--template", "\xf0fb4 <usedratio>%"
            , "--width", "2"
            ] 10
        , Run $ MultiCpu
            [ "--template", "\xf061a <total>%"
            , "--minwidth", "2"
            , "--Low", "20"
            , "--High", "60"
            , "--low", green
            , "--normal", "orange"
            , "--high", red
            ] 10
        , Run $ CpuFreq
            [ "--template", "\xf04c5 <avg>Ghz"
            ] 10
        , Run $ MultiCoreTemp
            [ "--template", "\xf050f <avg>\x2103"
            ] 10
        , Run $ DiskIO
            [ ("/", "<fc=#4e9a06>\xf19b2<read>B/s</fc> <fc=#cc0101>\xf19b3<write>B/s</fc>")
                -- TODO: Reports double in comparison with htop?
            ]
            [ "--width", "4"
            ] 10
        , Run $ DiskU
            [ ("/", "\xf02ca <used>/<size>")
            ]
            [
            ] 30000
        , Run $ DynNetwork
            [ "--template","<fc=#4e9a06>\xf16b6 <rx></fc> <fc=#cc0101>\xf16ba <tx></fc>" 
            , "--suffix", "True"
            , "--width", "7"
            ] 10
        ]

    , template = "\
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

