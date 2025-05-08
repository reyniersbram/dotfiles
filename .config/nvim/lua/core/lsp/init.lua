vim.lsp.config("*", {
    root_markers = { ".git" },
})

vim.lsp.enable("lua_ls")
require("core.lsp.document_highlight").setup({enable = true})
