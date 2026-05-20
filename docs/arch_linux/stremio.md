Stremio:

Requires qt5-webengine, which is not present in the AUR. qt5-webengine is built
against libvpx1.15 (libvpx.so.11).

```sh
wget https://mirror.cachyos.org/repo/x86_64/cachyos/qt5-webengine-5.15.19-5-x86_64.pkg.tar.zst
sudo pacman -U qt5-webengine-5.15.19-5-x86_64.pkg.tar.zst
paru -S libvpx1.15
```

