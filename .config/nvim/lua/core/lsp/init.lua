vim.lsp.config("*", {
    root_markers = { ".git" },
})
vim.lsp.enable({
    "gopls",
    "hls",
    "lua_ls",
    "pyright",
    "ruff",
})

require("core.lsp.document_highlight").setup({ enable = true })
