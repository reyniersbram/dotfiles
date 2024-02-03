local default_opts = { noremap = true, silent = true }

---@param mode string|table<string> See :help modes for available modes
---@param lhs string
---@param rhs string|fun()
---@param custom_opts? table Optional, if not specified default values are used
---     See :help vim.keymap.set for possible values
---     Default values: { noremap = true, silent = true }
local function keymap(mode, lhs, rhs, custom_opts)
    local opts = vim.tbl_deep_extend("force", default_opts, custom_opts or {})
    vim.keymap.set(mode, lhs, rhs, opts)
end

-- Remap space as leader key
keymap("", "<Space>", "<Nop>", { desc = "Space does nothing" })
vim.g.mapleader = " "
vim.g.maplocalleader = " "

keymap("n", "<C-h>", "<cmd>wincmd h<CR>", { desc = "Focus window to the left" })
keymap("n", "<C-j>", "<cmd>wincmd j<CR>", { desc = "Focus window below" })
keymap("n", "<C-k>", "<cmd>wincmd k<CR>", { desc = "Focus window above" })
keymap("n", "<C-l>", "<cmd>wincmd l<CR>", { desc = "Focus window to the right" })

keymap("n", "<C-Up>", "<cmd>resize +2<CR>", { desc = "Increase window height" })
keymap("n", "<C-Down>", "<cmd>resize -2<CR>", { desc = "Decrease window height" })
keymap("n", "<C-Left>", "<cmd>vertical resize -2<CR>", { desc = "Decrease window width" })
keymap("n", "<C-Right>", "<cmd>vertical resize +2<CR>", { desc = "Increase window width" })

keymap("n", "<leader>w", "<cmd>write<CR>", { desc = "Save buffer" })
keymap("n", "<leader>q", "<cmd>quit<CR>", { desc = "Close window" })
keymap("n", "<leader>Q", "<cmd>wqall<CR>", { desc = "Save all buffers and quit Neovim" })

keymap("n", "<leader>y", "<cmd>%yank+<CR>", { desc = "Copy whole buffer" })

keymap("v", "<", "<gv", { desc = "Keep selected selection after indenting" })
keymap("v", ">", ">gv", { desc = "Keep selected selection after indenting" })

-- For some reason using '<cmd>' gives unexpected behavior here
keymap("v", "J", ":move '>+1<CR>gv=gv", { desc = "Move line down, fix indentation" })
keymap("v", "K", ":move '<-2<CR>gv=gv", { desc = "Move line up, fix indentation" })
keymap("v", "<A-j>", ":move '>+1<CR>gv-gv", { desc = "Move line down, don't fix indentation" })
keymap("v", "<A-k>", ":move '<-2<CR>gv-gv", { desc = "Move line up, don't fix indentation" })

keymap("v", "p", '"_dP', { desc = "Paste and keep in register" })
