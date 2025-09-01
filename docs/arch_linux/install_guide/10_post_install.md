# Post Install Guide

> Log in as root

## Set up an internet connection

```sh
systemctl enable --now NetworkManager
```

Use `nmcli` or `nmtui` to connect to a Wi-Fi network.

## Configure pacman

Enable parallel downloads by setting `ParallelDownloads` in `/etc/pacman.conf`.

```sh
pacman -S pacman-contrib # install some extra pacman utilities
```

Get Belgian mirrors:

```sh
curl -s "https://archlinux.org/mirrorlist/?country=BE&protocol=https" >> /etc/pacman.d/mirrorlist
```

Edit the mirrorlist to include these mirrors, then rank them.

```sh
rankmirrors /etc/pacman.d/mirrorlist > /etc/pacman.d/mirrorlist
```

## Create new user

```sh
useradd --create-home --groups wheel *name*
passwd *name*
```

```sh
pacman -S sudo # install sudo
```

Uncomment `%wheel ALL=(ALL:ALL) ALL` in `etc/sudoers` to allow users in `wheel`
group to use `sudo`.

> Log in as new user

## Configure SSH and GPG

```sh
sudo pacman -S git openssh # install required packages
```

### Import public GPG key

```sh
gpg --auto-key-locate hkps://keys.openpgp.org --locate-keys *email* # import public key
gpg --edit-key *email* # update trust level of key
>trust
>ultimate
>exit
gpg --card-status # verify YubiKey can be read
```

### Configure GPG

Put this in `~/.gnupg/gpg-agent.conf`

```sh
cat > ~/.gnupg/gpg-agent.conf << HERE
enable-ssh-support
ttyname $GPG_TTY
default-cache-ttl 60
max-cache-ttl 120
HERE
```

Append this to `~/.bashrc`

```sh
cat > ~/.bashrc << HERE
GPG_TTY=$(tty)
export GPG_TTY
SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export SSH_AUTH_SOCK
gpgconf --launch gpg-agent
gpg-connect-agent updatestartuptty /bye > /dev/null
HERE
```

Reload the GPG configuration

```sh
killall gpg-agent
source .bashrc
```

## Audio playback

```sh
sudo pacman -S sof-firmware alsa-firmware alsa-utils
```

## TRIM for SSDs

See [TRIM](https://wiki.archlinux.org/title/Solid_state_drive#TRIM)

```sh
sudo systemctl enable fstrim.timer
```
