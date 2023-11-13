#! /usr/bin/env bash

setxkbmap "$(localectl list-x11-keymap-layouts | dmenu)"
