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

start_wm() {
    if [ -x "$(command -v nm-applet)" ] ; then
        nm-applet --indicator &
    fi

    if [ -x "$(command -v feh)" ] ; then
        ~/.fehbg &
    fi

    if [ -x "$(command -v autorandr)" ] ; then
        autorandr --change
    fi
}

start_xmonad () {
    start_wm
    if [ -x "$(command -v trayer)" ] ; then
        trayer --edge top --align right --width 7 --height 22 --iconspacing 10 \
            --tint 0x11111b --transparent true --alpha 0 --expand true --padding 10 \
            --SetDockType true --SetPartialStrut true -l --monitor primary &
    fi

    if [ -x "$(command -v picom)" ] ; then
        picom -b
    fi

    if [ -x "$(command -v xscreensaver)" ] ; then
        xscreensaver --no-splash &
    fi

    exec xmonad
}

start_dwm () {
    start_wm
    if [ -x "$(command -v dwmstatus)" ] ; then
        dwmstatus &>/dev/null &
    fi

    exec dwm
}

setxkbmap -option caps:escape
xbindkeys
numlockx on

session=${1:-xmonad}
case $session in
    xmonad)
        start_xmonad
        ;;
    dwm)
        start_dwm
        ;;
    *)
        exec $session
        ;;
esac
