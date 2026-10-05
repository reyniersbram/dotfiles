# Disk Partitioning

> Create the partition layout of the target computer.

## Goal

Create a partition layout that provides the space required for the system while
leaving room for additional features later without having to repartition the
disk or move existing partitions.

## Prerequisites

You will need:

- A target computer with an x86_64 CPU and UEFI firmware
- A bootable Void Linux USB drive (see [Installation
Media](./01-installation-media.md))

## Before You Start

These steps will **completely erase the target disk**.

Make sure that any data you want to keep is backed up before continuing.

The following instructions assume that the target disk is `/dev/nvme0n1`. **Do
not assume this is the correct device on your system.** Verify the device name
before modifying the partition table.

## Partition Layout

This guide uses the following GPT partition layout:

| Partition |          Size | Type                                                         | Purpose                    |
| --------- | ------------: | ------------------------------------------------------------ | -------------------------- |
| 1         | Total − 2 GiB | Linux root (x86-64) (`4F68BCE3-E8CD-4DB1-96E7-FBCAF984B709`) | Encrypted root partition   |
| 2         |         1 GiB | Linux extended boot (`BC13C2FF-59E6-4262-A352-B275FD6F7172`) | `/boot` partition          |
| 3         |         1 GiB | EFI System (`C12A7328-F81F-11D2-BA4B-00A0C93EC93B`)          | EFI System Partition (ESP) |

The root partition contains the system and user data.

The boot partition contains the kernel and initramfs. The ESP contains the EFI
bootloader used by the UEFI firmware to start the system.

The boot partition and ESP are intentionally placed **after the root
partition**. This makes it possible for later guides to remove one or both
partitions and reclaim their space without having to move the root partition.

> [!NOTE]
> The partition sizes above are minimums. The root partition should consume all
> remaining space after reserving 2 GiB for `/boot` and the ESP.

## Commands

Boot the target computer from the Void Linux USB drive. The exact procedure
depends on the hardware.

> [!NOTE]
> This guide assumes Secure Boot is disabled.

When the live environment has started, log in as `root` with the password
`voidlinux`.

### Identify the target disk

Before modifying anything, inspect the available disks:

```sh
lsblk -o NAME,SIZE,TYPE,MODEL
```

Identify the disk on which Void Linux will be installed.

For the remainder of this guide, this disk is assumed to be:

```text
/dev/nvme0n1
```

**Replace this device with the correct one for your system.**

### Create the partition table

Use `fdisk` to create a new GPT partition table and the three partitions
described above:

```sh
fdisk /dev/nvme0n1
```

Create:

1. A Linux root partition using all but the final 2 GiB of the disk.
2. A 1 GiB Linux extended boot partition.
3. A 1 GiB EFI System Partition.

Set the partition types appropriately before writing the partition table.

When you are satisfied with the layout, write the changes to disk.

> [!WARNING]
> Writing the partition table destroys the existing partition layout and makes
> the data on the disk inaccessible.

If you made a mistake, do **not** continue. Correct the partition layout before
proceeding.

## Verification

After writing the partition table, verify the result:

```sh
lsblk -o NAME,SIZE,TYPE,FSTYPE,PARTTYPE
```

For a 1 TiB disk, the result should look approximately like:

```text
NAME          SIZE TYPE FSTYPE PARTTYPE
nvme0n1         1T disk
├─nvme0n1p1   998G part        4f68bce3-e8cd-4db1-96e7-fb... 
├─nvme0n1p2     1G part        bc13c2ff-59e6-4262-a352-b275fd6f7172
└─nvme0n1p3     1G part        c12a7328-f81f-11d2-ba4b-00a0c93ec93b
```

The exact size reported for the root partition may differ slightly because of
disk geometry and partition alignment.

Verify that:

- The disk is using GPT.
- There are exactly three partitions.
- The first partition occupies the remaining space.
- The second partition is 1 GiB.
- The third partition is 1 GiB.
- The third partition has the EFI System type.
- The partition order matches the layout above.

## Result

The target disk now has the partition layout required to continue the
installation.

The next step is to [encrypt the root partition](./03-luks-encryption.md) if
full-disk encryption is desired.

**Decide on encryption now.** Encrypting the root partition later requires
reinstalling the system.

If encryption is not required, continue with the filesystem and installation
steps.
