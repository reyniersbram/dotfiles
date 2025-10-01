## Misc

- `less`
- `sudo`
- `man-db`, `man-pages`, `texinfo`

## Maple

```sh
sudo pacman -S cifs-utils
sudo mount //install.ugent.be/software -o username=username /mnt # change username
sudo /mnt/Maple/2023/Maple2023.1LinuxX64Installer.run
# or download installer first and run it locally
# choose 'Network License', 'licmaple.ugent.be'
```

## Some nice applications

- Launcher (applications, windows, files...): `rofi`
    - `rofi-calc` to have a calculator
- Wallpaper: `feh`
- Notifications: `libnotify`
    - `dunst` for nice notifications
- Systray: `trayer`
    - `network-manager-applet` for managing networks from systray
    - `systray-x-common` for Thunderbird notifications
- Password Manager:
    - `pass` the password manager
- VPN:
    - `nordvpn-bin` (AUR)
- Messaging:
    - `discord`
        - to show icons: `noto-font-emoji`
    - `thunderbird`
- Markup format conversion:
    - `pandoc`
    - `texlive` for some nice packages for pandoc

## Keybindings

- `xbindkeys` to have custom defined macros
- `numlockx` to enable numlock on startup
- `xclip` for clipboard management
