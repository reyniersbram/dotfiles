local icons = require("util.icons")

if vim.fn.has("nvim-0.10.0") ~= 1 then
    local signs = {
        { hl = "DiagnosticSignError", text = icons.diagnostics.ERROR },
        { hl = "DiagnosticSignWarn",  text = icons.diagnostics.WARN },
        { hl = "DiagnosticSignInfo",  text = icons.diagnostics.INFO },
        { hl = "DiagnosticSignHint",  text = icons.diagnostics.HINT },
    }
    for _, sign in ipairs(signs) do
        vim.fn.sign_define(sign.hl, { texthl = sign.hl, text = sign.text, numhl = "" })
    end
end

vim.diagnostic.config {
    virtual_text = true,
    signs = {
        priority = 10,
        text = {
            [vim.diagnostic.severity.ERROR] = icons.diagnostics.ERROR,
            [vim.diagnostic.severity.WARN] = icons.diagnostics.WARN,
            [vim.diagnostic.severity.INFO] = icons.diagnostics.INFO,
            [vim.diagnostic.severity.HINT] = icons.diagnostics.HINT,
        },
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
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
