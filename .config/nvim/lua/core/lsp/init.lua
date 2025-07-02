vim.lsp.config("*", {
    root_markers = { ".git" },
})
vim.lsp.enable({
    "cssls", -- css
    "gopls", -- go
    "hls", -- haskell
    "html", -- html
    "lua_ls", -- lua
    "pyright", "ruff", --python
    "taplo", -- toml
    "ts_ls", -- typescript
    "vue_ls", -- vue
})

require("core.lsp.document_highlight").setup({ enable = true })
