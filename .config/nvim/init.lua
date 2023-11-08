-- Remap space as leader key
-- Should happen before lazy.setup
vim.api.nvim_set_keymap(
    "", "<Space>", "<Nop>",
    { noremap = true, silent = true }
)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

require "lazy-setup"
vim.notify = require("notify")

require "core"
require "core.colorscheme"

require "config.keymaps"
