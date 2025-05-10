vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local default_opts = { noremap = true }

local function map(mode, lhs, rhs, custom_opts)
    local opts = vim.tbl_deep_extend("force", default_opts, custom_opts or {})
    vim.keymap.set(mode, lhs, rhs, opts)
end

map("", "<Space>", "<Nop>", { desc = "Unmap <Space>" })

map("n", "<leader>w", "<cmd>write<CR>", { desc = "Write the current file" })
map("n", "<leader>y", "<cmd>%yank<CR>", { desc = "Yank the current file" })

map({ "n", "t" }, "<localleader>t", "<Plug>FloatTermToggle",
    { desc = "Toggle the floating terminal" })

-- TODO: do I want this?
-- very basic auto-pairs
-- map("i", "'", "''<left>")
-- map("i", "\"", "\"\"<left>")
-- map("i", "{", "{}<left>")
-- map({ "i", "c" }, "(", "()<left>")
-- map("i", "[", "[]<left>")
-- map("i", "/*", "/**/<left><left>")

map("n", "<C-d>", "<C-d>zz", { desc = "Center cursor after scrolling down" })
map("n", "<C-u>", "<C-u>zz", { desc = "Center cursor after scrolling up" })

return map
