# Configure personal preferences

## Install packages

```sh
sudo pacman -S kitty autorandr ttf-hack-nerd bat bat-extras neovim \
bash-completion wget pandoc-cli tldr firefox
```

## Get personal configuration

Clone dotfiles repository, and follow the instructions.

```sh
git clone git@github.com:reyniersbram/dotfiles
```

## XMonad

### Install GHCup for managing Haskell and Stack

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

- When asked if `PATH` variables should be added to `.bashrc`, select no.
- Haskell Language Server can be installed after installation.
- Enable better Stack integration.

### Install required dependencies

```sh
sudo pacman -S libxpm # dependency for building XMonad and XMobar
```

### Build the configuration

```sh
cd ~/.config/xmonad
stack install
```

## Background

```sh
sudo pacman -S feh
```

Generate `.fehbg` script:

```sh
feh --bg-(fill|center|...) 'wallpaper.location'
```

## Yubikey

```sh
sudo pacman -S yubikey-manager libfido2
sudo systemctl enable --now pcscd.service
```

### PAM authentication

See https://wiki.archlinux.org/title/Universal_2nd_Factor#Authentication_for_user_sessions

```sh
sudo pacman -s pam-u2f
mkdir -p .config/Yubico
pamu2fcfg -o pam://$HOSTNAME -i pam://$HOSTNAME > .config/Yubico/u2f_keys # add first key
pamu2fcfg -o pam://$HOSTNAME -i pam://$HOSTNAME -n >> .config/Yubico/u2f_keys # add backup key
sudo mkdir -p /etc/Yubico
sudo mv .config/Yubico/u2f_keys /etc/Yubico/u2f_keys
```

Add the following line at the beginning of modules where you want passwordless
login, be sure to edit the hostname. Good options are `/etc/pam.d/sudo`,
`/etc/pam.d/vlock` and `/etc/pam.d/login`.

```
auth    sufficient    pam_u2f.so cue origin=pam://hostname appid=pam://hostname authfile=/etc/Yubico/u2f_keys
```
