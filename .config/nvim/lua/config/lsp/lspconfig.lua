local prequire = require("helpers.utils").prequire

local lspconfig_status_ok, lspconfig = prequire("lspconfig")
if not lspconfig_status_ok then
    vim.notify("lspconfig not found!")
    return
end

local icons = require("helpers.icons")

-- Diagnostics
local signs = {
    { name = "DiagnosticSignError", text = icons.diagnostics.Error },
    { name = "DiagnosticSignWarn", text = icons.diagnostics.Warning },
    { name = "DiagnosticSignHint", text = icons.diagnostics.Hint },
    { name = "DiagnosticSignInfo", text = icons.diagnostics.Information },
}

for _, sign in ipairs(signs) do
    vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

vim.diagnostic.config {
    virtual_text = true,
    signs = true,
    update_in_insert = true,
    severity_sort = true,
    float = {
        focusable = true,
        style = "minimal",
        border = "rounded",
        source = true,
    },
}

-- LSP
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "rounded",
})

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = "rounded",
})

-- Language Servers
local opts = {
    on_attach = require("config.lsp.handlers").on_attach,
    capabilities = require("config.lsp.handlers").capabilities,
    autostart = true,
}
local lua_ls_opts = require("config.lsp.settings.lua_ls")
opts = vim.tbl_deep_extend("keep", lua_ls_opts, opts)
lspconfig.lua_ls.setup(opts)
