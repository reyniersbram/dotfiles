# Neovim

## Installation

First time using:

```sh
nvim --headless -c "Lazy! restore" -c "qa"
nvim --headless -c "Lazy! clean" -c "qa"
nvim --headless -c "TSUpdateSync" -c "qa"
```

## Install spell files

Automatic installation of spell files requires the `netrw` plugin. Other file
explorers might disable `netrw`, so these should be disabled for this.
