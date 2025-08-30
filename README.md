# My dotfiles

This is my dotfiles repo.

| My Environment |   |
|---|---|
| **Distro** | Arch Linux |
| **Window Manager** | dwm |
| **Shell** | Bash |
| **Terminal Emulator** | st |
| **Text Editor** | Neovim |

## Installation

An installation script is provided:

```sh
git clone git@github.com:reyniersbram/dotfiles.git
cd dotfiles
./install.sh
```

> ![WARN]
> This script is primarily intended for setting up a fresh system with my
> preferred configurations and programs.  
> It may not behave correctly on existing setups, and there is no guarantee that
> it will work as expected in every environment.  
> Use with caution.

The script will:
- Install the required packages needed for the script itself (requires root)
- Create XDG-compliant directories
- Symlink configuration files using `stow`
- Install packages listed in [packages.txt](./etc/packages.txt) (requires root)
- Apply some system-level configurations (requires root)

It can be extended by placing additional scripts in [install.d](./install.d).
The helper library [lib.sh](./lib.sh) provides common functions and variables
that can be sourced within those scripts.  
The scripts are designed to be idempotent, so they can be safely re-run.

Feedback, tips, or questions are welcome. Please open an issue if you'd like to
share something.
