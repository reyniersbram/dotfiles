# Installation Media

> Prepare the installation media and verify its integrity before starting the
> installation. This is a concise summary of the
> [Void Linux installation documentation](https://docs.voidlinux.org/installation/index.html).

## Goal

Download a Void Linux live image, verify that it is authentic and unmodified,
and write it to a USB drive that can be used to boot the installer.

Verifying the image is important: a corrupted or tampered installation image
should **never** be used to install the system.

## Prerequisites

You will need:

- An existing Linux installation (any distribution)
- An internet connection
- A USB drive
- `wget`
- `minisign`
- `sha256sum` (or another SHA-256 implementation)
- `dd`

On most Linux distributions, `wget`, `sha256sum`, and `dd` are already
installed. `minisign` may need to be installed separately.

## Choose an image

Void Linux provides images using either **glibc** or **musl**.

- **glibc** is the conventional choice and generally provides the widest
compatibility with precompiled third-party software.
- **musl** is a lightweight alternative and is particularly useful if you
specifically want a musl-based system.

This guide uses **x86_64 + musl**. Change `ARCH` if you want a different
architecture or C library.

Refer to the [official documentation](https://voidlinux.org/download/) to choose
the right image.

Set the version to the release you intend to install:

```sh
VERSION=20250202
ARCH=x86_64-musl
```

> [!NOTE]
> Replace `VERSION` with the version of the image you actually want to download,
> probably the latest version. Do not blindly copy the example version above.

## Download

Download the ISO together with its SHA-256 checksum and Minisign signature:

```sh
wget \
    "https://repo-default.voidlinux.org/live/current/void-live-${ARCH}-${VERSION}-base.iso" \
    "https://repo-default.voidlinux.org/live/current/sha256sum.txt" \
    "https://repo-default.voidlinux.org/live/current/sha256sum.sig"
```

You should now have:

```text
void-live-${ARCH}-${VERSION}-base.iso
sha256sum.txt
sha256sum.sig
```

## Verify the image

Verification consists of two separate steps:

1. Verify that `sha256sum.txt` was signed by Void Linux.
2. Verify that the ISO matches the checksum contained in that signed file.

### 1. Verify the checksum file's signature

Download the Void Linux release signing key from a trusted source, for example
the Void Linux GitHub repository:

```sh
wget "https://raw.githubusercontent.com/void-linux/void-packages/refs/heads/master/srcpkgs/void-release-keys/files/void-release-${VERSION}.pub"
```

Verify the signature:

```sh
minisign \
    -V \
    -p "void-release-${VERSION}.pub" \
    -x sha256sum.sig \
    -m sha256sum.txt
```

A successful verification looks similar to:

```text
Signature and comment signature verified
Trusted comment: This key is only valid for images with date 20250202.
```

The exact trusted comment will depend on the version being used.

> [!WARNING]
> Do not continue if Minisign reports an invalid signature.

### 2. Verify the ISO checksum

Use `sha256sum` to compare the downloaded ISO against the signed checksum file:

```sh
sha256sum -c sha256sum.txt --ignore-missing
```

The expected result is:

```text
void-live-x86_64-musl-20250202-base.iso: OK
```

The filename will naturally differ if you selected another architecture or
version.

Alternatively, if your system provides `sha256`:

```sh
sha256 -C sha256sum.txt "void-live-${ARCH}-${VERSION}-base.iso"
```

You should see:

```text
(SHA256) void-live-x86_64-musl-20250202-base.iso: OK
```

> [!WARNING]
> Do not continue if the checksums do not match.

If either the Minisign verification or the SHA-256 verification fails, **do not
use the image**. Delete it and investigate the failure before proceeding.

## Write the image to a USB drive

> [!WARNING]
> The following operation **destroys all data on the target device**.
> Carefully identify the USB drive before running `dd`.

First identify the USB device:

```sh
lsblk
```

For example:

```text
NAME        SIZE TYPE
nvme0n1    1.0T  disk
├─nvme0n1p1
└─nvme0n1p2
sda        16G   disk
```

Here, `/dev/sda` is the USB drive. **Do not assume that `/dev/sda` is your USB
drive**; device names depend on the system.

> [!WARNING]
> Never run the `dd` command against a disk containing data you do not have
> backed up. Double-check the device path before running the command.

Once you have confirmed the correct device, write the ISO:

```sh
sudo dd \
    if="void-live-${ARCH}-${VERSION}-base.iso" \
    of=/dev/sda \
    bs=4M \
    status=progress \
    conv=fsync
```

Replace `/dev/sda` with the device corresponding to your USB drive.

Wait for `dd` to finish successfully. `conv=fsync` ensures that the data is
flushed before `dd` exits.

You can also explicitly synchronize pending writes afterwards:

```sh
sync
```

## Result

You can safely eject the drive:

```sh
sudo eject /dev/sda
```

Replace `/dev/sda` with the appropriate device if necessary.

The USB drive now contains a bootable Void Linux live environment which can be
used to install Void Linux on the target computer.

## Sources

- https://docs.voidlinux.org/installation/index.html
- https://voidlinux.org/download/
