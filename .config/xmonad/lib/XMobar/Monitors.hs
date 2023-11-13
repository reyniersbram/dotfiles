module XMobar.Monitors where

import Colors (fgColor, green, red)
import Xmobar
  ( Command (Com),
    Date (Date),
    Kbd (Kbd),
    Monitors (Alsa, Battery, Brightness, CpuFreq, DiskIO, DiskU, DynNetwork, Memory, MultiCoreTemp, MultiCpu, Swap),
  )

sound :: Monitors
sound =
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
      fgColor,
      "--off",
      "\xf0581 ",
      "--offc",
      fgColor,
      "--lows",
      "\xf057f ",
      "--mediums",
      "\xf0580 ",
      "--highs",
      "\xf057e "
    ]

brightness :: Int -> Monitors
brightness =
  Brightness
    [ "--template",
      "<percent>",
      "--suffix",
      "True",
      "--",
      "-D",
      "intel_backlight"
    ]

battery :: Int -> Monitors
battery =
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

conservationStatus :: Int -> Command
conservationStatus =
  Com
    "/bin/bash"
    [ "-c",
      "conservation_mode icon"
    ]
    "conservation_status"

date :: Int -> Date
date = Date "%H:%M · %a %Y-%m-%d" "date"

keyboardLayout :: Kbd
keyboardLayout = Kbd [("us", "us")]

memory :: Int -> Monitors
memory =
  Memory
    [ "--template",
      "\xf035b <used>G",
      "--ddigits",
      "2",
      "--width",
      "5",
      "--Low",
      "4",
      "--High",
      "8",
      "--low",
      green,
      "--normal",
      "orange",
      "--high",
      red,
      "--",
      "--scale",
      "1024"
    ]

swap :: Int -> Monitors
swap =
  Swap
    [ "--template",
      "\xf0fb4 <usedratio>%",
      "--width",
      "2"
    ]

cpuUsage :: Int -> Monitors
cpuUsage =
  MultiCpu
    [ "--template",
      "\xf061a <total>%",
      "--minwidth",
      "2",
      "--Low",
      "20",
      "--High",
      "60",
      "--low",
      green,
      "--normal",
      "orange",
      "--high",
      red
    ]

cpuFrequency :: Int -> Monitors
cpuFrequency =
  CpuFreq
    [ "--template",
      "\xf04c5 <avg>Ghz"
    ]

cpuTemperature :: Int -> Monitors
cpuTemperature =
  MultiCoreTemp
    [ "--template",
      "\xf050f <avg>\x2103"
    ]

diskIO :: Int -> Monitors
diskIO =
  DiskIO
    [ ("/", "\xf443 <total>B/s")
    -- TODO: Reports double in comparison with htop?
    ]
    [ "--width",
      "4"
    ]

diskUsage :: Int -> Monitors
diskUsage =
  DiskU
    [ ("/", "\xf02ca <used>/<size>")
    ]
    []

networkIO :: Int -> Monitors
networkIO =
  DynNetwork
    [ "--template",
      "<fc=#4e9a06>\xf16b6 <rx></fc> <fc=#cc0101>\xf16ba <tx></fc>",
      "--suffix",
      "True",
      "--width",
      "7"
    ]
