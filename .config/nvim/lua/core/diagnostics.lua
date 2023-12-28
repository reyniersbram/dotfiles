local icons = require("util.icons")

-- Diagnostics
local signs = {
    { name = "DiagnosticSignError", text = icons.diagnostics.ERROR },
    { name = "DiagnosticSignWarn",  text = icons.diagnostics.WARN },
    { name = "DiagnosticSignInfo",  text = icons.diagnostics.INFO },
    { name = "DiagnosticSignHint",  text = icons.diagnostics.HINT },
}

-- NOTE: use vim.diagnostic.config starting from v0.10
for _, sign in ipairs(signs) do
    vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

vim.diagnostic.config {
    virtual_text = true,
    signs = {
        priority = 10,
        -- NOTE: use this starting from v0.10
        -- text = {
        --     [vim.diagnostic.severity.ERROR] = icons.diagnostics.ERROR,
        --     [vim.diagnostic.severity.WARN] = icons.diagnostics.WARN,
        --     [vim.diagnostic.severity.INFO] = icons.diagnostics.INFO,
        --     [vim.diagnostic.severity.HINT] = icons.diagnostics.HINT,
        -- },
    },
    update_in_insert = true,
    severity_sort = true,
    float = {
        focusable = true,
        style = "minimal",
        border = icons.ui.window.float.border,
        source = true,
    },
}

local opts = { noremap = true, silent = true }
vim.keymap.set("n", "gl", vim.diagnostic.open_float, opts)
vim.keymap.set("n", "ge", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "gE", vim.diagnostic.goto_prev, opts)
