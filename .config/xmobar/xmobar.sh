killall xmobar
cd "${XDG_CONFIG_HOME}/xmobar" || exit
stack install
xmobar
