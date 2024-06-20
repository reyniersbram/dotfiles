#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Custom configuration

setxkbmap -option caps:escape
xbindkeys
numlockx on

if [ -x "$(command -v trayer)" ] ; then
    trayer --edge top --align right --width 7 --height 22 --iconspacing 10 \
        --tint 0x0c0c0c --transparent true --alpha 0 --expand true --padding 10 \
        --SetDockType true --SetPartialStrut true -l --monitor primary &
fi

if [ -x "$(command -v nm-applet)" ] ; then
    nm-applet --indicator &
fi

if [ -x "$(command -v birdtray)" ] ; then
    birdtray &
fi

if [ -x "$(command -v autorandr)" ] ; then
    autorandr --change
fi

# if [ -x "$(command -v picom)" ] ; then
#     picom -b
# fi

if [ -x "$(command -v dunst)" ] ; then
    dunst &
fi

if [ -x "$(command -v xscreensaver)" ] ; then
    xscreensaver --no-splash &
fi

if [ -x "$(command -v nitrogen)" ] ; then
    nitrogen --restore &
fi

exec xmonad
