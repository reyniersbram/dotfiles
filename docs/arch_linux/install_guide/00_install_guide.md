# Arch Install Guide

> https://wiki.archlinux.org/title/Installation_guide

This is a compact version of the official Arch Linux Installation Guide, 
tailored to my specific needs.

## Preparation

[Download](https://archlinux.org/download/) an ISO, and burn it on a flash 
drive.

## Boot the live environment

> [!WARNING]
> Disable Secure Boot in UEFI settings

### Set  keyboard layout

```sh
localectl list-keymaps # list available layouts
loadkeys us # select us
```

### Verify UEFI bitness

```sh
cat /sys/firmware/efi/fw_platform_size # should return 64
```

### Set up internet connection

- An ethernet connection works out of the box by plugging in the cable.
- Use `iwctl` to set up a Wi-Fi connection:

```iwctl
device list
device *name* set-property Powered on
adapter *name* set-property Powered on
station *name* scan
station *name* get-networks
station *name* connect *network-name*
```

```sh
ping ping.archlinux.org # verify connection
```

### Update system clock

```sh
timedatectl # verify this displays the right date
```

### Create disk partitions

```sh
lsblk # identify available disks
```

#### Required partitions

- EFI system partition for booting (already exists on dual boot)
    - 1GiB
    - EFI system partition
- Root directory
    - Remainder of available space
    - Type: Linux x86-64 root

#### Optional partitions

- Swap
    - At least 4GiB, equal to available RAM
    - Type: Linux swap
    - Alternative is swap on zram, set up after installation
- Separate /home partition
    - Type: Linux home

Use `fdisk` (cli) or `cfdisk` (ncurses) to create the partition table.

#### Format partitions

```sh
mkfs.ext4 /dev/*root_partition*
mkswap /dev/*swap_partition* # if swap was created
mkfs.fat -F 32 /dev/*efi_system_partition* # only when partition did not exist yet
```

In case other partitions where created, format these as well.

#### Mount file systems

```sh
mount /dev/*root_partition* /mnt
mount --mkdir /dev/*efi_system_partition* /mnt/boot/efi
swapon /dev/*swap_partition* # if swap was created
```
In case other partitions where created, mount them to their respective location
under `/mnt`, e.g.:

```sh
mound /dev/*home_partition* /mnt/home
```

## Installation of the base system

```sh
pacman -Sy --needed archlinux-keyring
pacstrap -K /mnt base linux linux-lts linux-firmware intel-ucode grub \
efibootmgr e2fsprogs networkmanager vi vim
```

- Choose `intel-ucode` or `amd-ucode` according to CPU
- Other kernels can be chosen as well
- Firmware is not required when installing in a virtual machine

## Configuration of the system

### Generate fstab

```sh
genfstab -U /mnt >> /mnt/etc/fstab
```

### Chroot

```sh
arch-chroot /mnt
```

### Time

```sh
ln -sf /usr/share/zoneinfo/*Region*/*City* /etc/localtime # choose timezone
hwclock --systohc
```

### Localization

Edit `/etc/locale.gen`, and uncomment `en_US.UTF-8 UTF-8`.

```sh
locale-gen # generate locales
echo 'LANG=en_US.UTF-8' > /etc/locale.conf
echo 'KEYMAP=us' > /etc/vconsole.conf
```

### Network configuration

Choose a name for the device

```sh
echo *hostname* > /etc/hostname
```

### Root password

```sh
passwd
```

### Boot loader

#### Install GRUB

```sh
grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=GRUB
```

#### Configure GRUB

```sh
pacman -S os-prober
```

Edit `/etc/default/grub`:
- To allow detection of other operating systems, set `GRUB_DISABLE_OS_PROBER=false`
- To disable sub menus, set `GRUB_DISABLE_SUBMENU=y`
    - e.g. to disable *'Advanced Arch Linux Options'*
- To boot to last used entry by default:
    - Set `GRUB_DEFAULT=saved`
    - Set `GRUB_SAVE_DEFAULT=true`
- When using multiple kernels, set `GRUB_TOP_LEVEL="/boot/vmlinuz-linux` to put
*linux* before *linux-lts*

```sh
grub-mkconfig -o /boot/grub/grub.cfg
```

### Modify kernel parameters

Edit `/etc/mkinitcpio.conf` as desired

```sh
mkinitcpio -P
```

## Reboot

```sh
exit # exit chroot environment
umount -R /mnt
reboot
```
