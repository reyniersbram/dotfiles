# LUKS Full Disk Encryption

> Encrypt the root partition to protect user data.

## Goal

Encrypt the root partition with LUKS2 so that the data stored on it cannot be
read without the corresponding passphrase.

This guide uses a **detached LUKS header**, meaning the LUKS metadata is stored
in a separate file rather than on the encrypted partition itself.

> [!NOTE]
> This does not encrypt the entire disk. The EFI System Partition (ESP) and
> `/boot` partition remain unencrypted so that the firmware and bootloader can
> access the files required to start the system.

## Scope

Encrypting the root partition protects the confidentiality of the data stored on
it when the computer is powered off. An attacker who obtains the disk without
the required LUKS header and passphrase cannot normally decrypt its contents.

A detached header can also make the encrypted partition less immediately
identifiable as a LUKS volume. The presence of an EFI System Partition and
separate `/boot` partition may still provide clues that encryption is being
used. Further measures can be introduced later if this is a requirement.

## Prerequisites

You will need:

- A correctly partitioned target computer with an x86_64 CPU and UEFI firmware
(see [Disk Partitioning](./02-disk-partitioning.md))
- A bootable Void Linux USB drive (see [Installation
Media](./01-installation-media.md))
- A secure place to store recovery material
- A secure and memorable LUKS passphrase

The live environment must provide `cryptsetup`.

## Risks

The LUKS header contains essential metadata required to access the encrypted
data.

With a detached header, **both the header and the passphrase are required**.

If the passphrase is lost, the data cannot be decrypted.

If the detached header is lost or corrupted, the data cannot be decrypted.

If both the header and passphrase are compromised, an attacker may be able to
decrypt the data.

Keep at least one backup of the detached header in a secure location separate
from the computer. Protect the backup because possession of the header
facilitates offline attacks against the passphrase.

> [!NOTE]
> Do not store the only copy of the detached header on the encrypted partition
> itself. It must be available before that partition can be unlocked.

## Before You Start

Boot the target computer from the Void Linux USB drive as described in the [Disk
Partitioning](./02-disk-partitioning.md) guide.

Make sure Secure Boot remains disabled.

Log in as `root` using the password `voidlinux`.

## Commands

This guide uses `cryptsetup` to create a LUKS2 container on the root partition.

The root partition is assumed to be:

```text
/dev/nvme0n1p1
```

The detached LUKS header will be stored as:

```text
luks_header.img
```

Choose a secure location for this file before continuing.

### Identify the target partition

Verify the current partition layout:

```sh
lsblk -o NAME,SIZE,TYPE,FSTYPE,PARTTYPE
```

For example:

```text
NAME          SIZE TYPE FSTYPE PARTTYPE
nvme0n1         1T disk
├─nvme0n1p1   998G part        4f68bce3-e8cd-4db1-96e7-fb...
├─nvme0n1p2     1G part        bc13c2ff-59e6-4262-a352-b275fd6f7172
└─nvme0n1p3     1G part        c12a7328-f81f-11d2-ba4b-00a0c93ec93b
```

> [!WARNING]
> Make absolutely sure that `/dev/nvme0n1p1` is the intended root partition.

The following operation initializes the partition as a new encrypted volume and
destroys any existing data on it.

### Create the LUKS container

Create a LUKS2 container with a detached header:

```sh
cryptsetup luksFormat \
    --type luks2 \
    --header luks_header.img \
    /dev/nvme0n1p1
```

`cryptsetup` will ask you to confirm the operation and choose a passphrase.

Choose a strong passphrase that you can reliably remember or recover.

> [!WARNING]
> The partition will be overwritten as part of this operation. Verify the target
> device before confirming.

The `--header` option causes the LUKS header to be stored in `luks_header.img`
instead of on `/dev/nvme0n1p1`.

## Protect the Header

The detached header is critical recovery material.

Immediately create one or more secure backups:

```sh
cp luks_header.img /path/to/secure/backup/
```

The backup should be stored somewhere **separate from the target computer**.

For example, a removable storage device that is kept securely offline can be
used.

Do not proceed until you are confident that you can recover the header if the
original file is lost.

You should also make sure that the LUKS passphrase itself is recoverable. Do not
store the only copy of the passphrase together with the only copy of the header.

## Verification

Verify that the LUKS container can be opened using the detached header.

### Manually unlock the partition

```sh
cryptsetup open \
    --header luks_header.img \
    /dev/nvme0n1p1 \
    root-crypt
```

Enter the LUKS passphrase when prompted.

If successful, `cryptsetup` creates:

```text
/dev/mapper/root-crypt
```

Verify that the mapping exists:

```sh
lsblk
```

You should see a mapping similar to:

```text
NAME
nvme0n1
├─nvme0n1p1
│ └─root-crypt
├─nvme0n1p2
└─nvme0n1p3
```

### Manually close the container

Once the mapping has been verified, close it again:

```sh
cryptsetup close root-crypt
```

Verify that `/dev/mapper/root-crypt` no longer exists:

```sh
ls /dev/mapper/
```

The encrypted partition should now be locked again.

## Recovery

The detached header and passphrase are both required to access the encrypted
data.

If the passphrase is permanently lost, the data cannot be recovered through
normal LUKS mechanisms.

If the detached header is permanently lost or corrupted, the data cannot be
recovered through normal LUKS mechanisms.

If you lose either one, **do not overwrite or recreate the LUKS container** if
you are attempting to recover existing data. Preserve the encrypted partition
and all remaining header backups.

## Rollback

If you decide not to use encryption anymore, the LUKS container can be discarded
and the root partition reused for another purpose.

Because this guide uses a detached header, the LUKS metadata is stored in
`luks_header.img`. Removing this file makes the LUKS container inaccessible, but
does not erase the encrypted data from `/dev/nvme0n1p1`.

For a fresh installation where no data needs to be preserved, remove the
detached header. The root partition itself can then be reformatted with the
desired filesystem during the next installation step.

> [!WARNING]
> Deleting the detached header is effectively destructive: without a valid copy
> of the header and the corresponding passphrase, the existing encrypted data
> cannot be accessed. This does not securely erase the underlying data.

If the encrypted partition needs to be securely wiped before reusing it, follow
a separate disk-erasure procedure appropriate for the storage device. Do not
confuse deleting the detached header with securely erasing the partition.

## Result

The root partition is now encrypted with LUKS2, while its LUKS header is stored
in a separate file.

The detached header must be kept safe and backed up. The LUKS passphrase must
also be kept recoverable.

The encrypted root partition is now ready to be formatted with the appropriate
filesystem. Continue with [Filesystem
Formatting](./04-filesystem-formatting.md).
