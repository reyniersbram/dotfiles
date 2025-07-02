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

vim.api.nvim_create_autocmd("LSPAttach", {
    group = vim.api.nvim_create_augroup("my.lsp", {}),
    callback = function (args)
        local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
        if client:supports_method("textDocument/inlayHint") then
            vim.lsp.inlay_hint.enable(true, {bufnr = args.buf})
        end
    end,
})

require("core.lsp.document_highlight").setup({ enable = true })
