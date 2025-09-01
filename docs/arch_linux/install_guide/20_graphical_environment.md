## Xorg

```sh
sudo pacman -S xorg-server xorg-xinit
```

Edit `.xinitrc` to set applications to be launched, e.g.:

```sh
numlockx on
exec xmonad
```

And start the window manager

```sh
startx
```

To launch an application without window manager:

```sh
startx /usr/bin/<application>
```

## Multiple monitors

```sh
sudo pacman -S xorg-xrandr
```

## Frameworks

- `gtk*` is required by some applications in [30_applications.md](./30_applications.md)
- `qt6` package group is required for e.g. pyplot
    - `qt6-wayland` can be uninstalled after installing the package group
