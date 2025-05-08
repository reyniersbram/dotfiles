vim.lsp.config("*", {
    root_markers = { ".git" },
})
vim.lsp.enable({
    "gopls",
    "lua_ls",
    "pyright",
})

require("core.lsp.document_highlight").setup({enable = true})
