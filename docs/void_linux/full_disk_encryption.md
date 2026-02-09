# Full Disk Encryption

## Example

This example will go over all steps to set up full disk encryption for a fresh
install of Void Linux. This guide is tailored to my preferences, e.g. usage of a
YubiKey. The goal of this guide is to have root and swap partitions with LVM on
a LUKS encrypted partition. The LUKS passphrase to the LUKS partition will be a
GPG-encrypted keyfile, that can be unlocked with the private GPG key stored on a
YubiKey. The initramfs and encrypted keyfile will be stored on a separate,
unencrypted partition.

> [!NOTE]
> This guide does not cover Secure Boot or other additional security techniques
> (yet).
> See here to configure:
> - https://wiki.gentoo.org/wiki/Encrypted_bootable_media_with_SecureBoot/GRUB/LUKS

### Disk preparation

This guide will use UEFI, GPT as partition schema and GRUB as bootloader. The
partition layout used in this guide will be the following:

```
nvme0n1             259:0    0   200G  0 disk
├─nvme0n1p1         259:1    0     1G  0 part   /boot/efi
├─nvme0n1p2         259:2    0     1G  0 part   /mnt
└─nvme0n1p3         259:3    0    98G  0 part
  └─root            253:0    0    98G  0 crypt
    ├─void-swap     253:1    0    24G  0 lvm
    └─void-root     253:2    0   174G  0 lvm    /
```

This system has 16GB of RAM, to support hibernation, [1.5 times this
amount](https://wiki.gentoo.org/wiki/Handbook:AMD64/Installation/Disks#What_about_swap_space.3F)
is chosen as swap size. Other configurations are possible as well. When
multiple disks are available, it might be useful to spread swap across these
disks. Using a separate boot drive is possible as well.

#### Partitioning

Use `fdisk`, or any other partitioning tool of choice, to format the disk like
this:

```
Device           Size Type
/dev/nvme0n1p1     1G EFI System
/dev/nvme0n1p2     1G Linux extended boot
/dev/nvme0n1p3   100G Linux root (x86-64)
```

#### LUKS partition

This guide uses a detached header for LUKS. This header will be stored together
with the encrypted keyfile on the boot partition.

First, create and encrypt the keyfile. The recipient of the encryption is the
private key you want to use to decrypt the keyfile.
```sh
dd bs=8388608 count=1 if=/dev/urandom | gpg --recipient <email> --output crypt_key.luks.gpg --encrypt
```

Create a LUKS container with a detached header using this keyfile. As it is
encrypted, it has to be decrypted first using `gpg`. Open the container with
both the decrypted keyfile and the LUKS header.
```sh
gpg --decrypt crypt_key.luks.gpg | \
    cryptsetup luksFormat --key-size 512 \
        --type luks2 \
        --header luks_header.img \
        /dev/nvme0n1p3 -
```

#### Create filesystems

First create the EFI System Partition (ESP). If an ESP is already present, skip
this step.

```sh
mkfs.vfat -F /dev/nvme0n1p1
```

The Extended Boot filesystem will be `ext4`.

```sh
mkfs.ext4 -L boot /dev/nvme0n1p2
```

To initialize LVM for the encrypted partitions, the LUKS container has to be
opened first.

```sh
gpg --decrypt crypt_key.luks.gpg | \
    cryptsetup open --type luks2 \
        --header luks_header.img \
        --key-file - \
        /dev/nvme0n1p3 root
```

Now the LVM volumes can be created.

```sh
pvcreate /dev/mapper/root
vgcreate void /dev/mapper/root
lvcreate --name swap -L 24G void
lvcreate --name root -l 100%FREE void
```

Format the volumes.

```sh
mkfs.ext4 -L root /dev/void/root
mkswap -L swap /dev/void/swap
```

<!-- TODO: look into e2scrub: https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system#Preparing_the_logical_volumes -->

### Installing Void Linux

Mount the created partitions.

```sh
mount /dev/void/root /mnt
mkdir -p /mnt/boot
mount /dev/nvme0n1p2 /mnt/boot
mkdir -p /mnt/boot/efi
mount /dev/nvme0n1p1 /mnt/boot/efi
swapon /dev/void/swap
```

#### Base installation

Follow the [base installation
instructions](https://docs.voidlinux.org/installation/guides/chroot.html#base-installation)
for Void Linux.

> [!NOTE]
> When installing from another distribution, chroot into the the environment to
> use `xbps-*` commands. E.g. on Arch Linux, you can use `arch-chroot`.

Install the following packages as well:
- cryptsetup
- grub-x86_64-efi
- lvm2
- gnupg
- gnupg2-scdaemon
- pinentry

```sh
xbps-install -r /mnt \
    cryptsetup grub-x86_64-efi lvm2 \
    gnupg gnupg2-scdaemon pinentry
```

#### Configuration

Generate the `fstab` file, this can be done by a tool of choice, e.g.
`xgenfstab` on Void, or `genfstab` on Arch. Check if this has the right entries.
It should contain entries for `/` (`/dev/mapper/void-root`), `/boot`
(`/dev/nvme0n1p2`), `/boot/efi` (`/dev/nvme0n1p1`) and swap
(`/dev/mapper/void-swap`).

```sh
xgenfstab -U /mnt >> /mnt/etc/fstab
```

Now enter the chroot, and set up the root user.

```sh
chown root:root /
chmod 755 /
passwd root
echo <hostname> > /etc/hostname
ln -sf /usr/share/zoneinfo/<zone> /etc/localtime
```

#### Initramfs

Copy the encrypted keyfile and detached LUKS header to the extended boot
partition (this has to be done outside of the chroot environment). The public
key of the used GPG key for the keyfile should be available for `dracut` as
well.

```sh
cp crypt_key.luks.gpg /mnt/boot
cp luks_header.img /mnt/boot
gpg --output crypt-public-key.gpg --export <email>
cp crypt-public-key.gpg /mnt/etc/dracut.conf.d/crypt-public-key.gpg
```

Edit `/etc/dracut.conf` and add this line:

```conf
add_dracutmodules+=" crypt crypt-gpg dm rootfs-block lvm "
kernel_cmdline+=" root=LABEL=root-crypt \
    rd.luks.partuuid=<luks_partuuid> \
    rd.luks.crypttab=1 "
install_items+=" /boot/crypt_key.luks.gpg /boot/luks_header.img /etc/crypttab "
hostonly="yes"
```

<!-- TODO: figure out hostonly mode -->
<!-- TODO: try to keep files out of initramfs -->

`<luks_partuuid>` and `<boot_uuid>` are the partition UUIDs of the encrypted
partition and the extended boot partition respectively. They can be found with:

```sh
blkid -o value -s PARTUUID /dev/nvme0n1p3
blkid -o value -s UUID /dev/nvme0n1p2
```

Create the `/etc/crypttab` file with the following contents:

```conf
root-crypt  PARTUUID=<luks_partuuid>    /boot/crypt_key.luks.gpg    luks,header=/boot/luks_header.img
```

Install the bootloader:
```sh
grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id="Void"
```

Generate initramfs:

```sh
xbps-reconfigure -fa
```

This command will configure GRUB as well, but this can be done more explicit by
running:

```sh
grub-mkconfig -o /boot/grub/grub.cfg
```

### Resources

- [Void Linux FDE Guide](https://docs.voidlinux.org/installation/guides/fde.html)
- [Gentoo FDE Guide](https://wiki.gentoo.org/wiki/Full_Disk_Encryption_From_Scratch)
- [Gentoo dm-crypt Guide](https://wiki.gentoo.org/wiki/Dm-crypt)
- [Arch Linux dm-crypt/FDE Guide](https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system)
- [Arch Linux dm-crypt detached headers Guide](https://wiki.archlinux.org/title/Dm-crypt/Specialties#Encrypted_system_using_a_detached_LUKS_header)
- [How To Use LUKS with a detached header](https://linuxconfig.org/how-to-use-luks-with-a-detached-header)
- [Gentoo LVM Guide](https://wiki.gentoo.org/wiki/LVM)
- [Arch Linux LVM Guide](https://wiki.archlinux.org/title/LVM)
- [This Reddit post](https://www.reddit.com/r/archlinux/comments/7np36m/detached_luks_header_full_disk_encryption_with/)
- [This archived Gist](https://web.archive.org/web/20230611114013/https://gist.github.com/Le0xFF/ff0e3670c06def675bb6920fe8dd64a3)
