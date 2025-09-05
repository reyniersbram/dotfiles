local lsp_util = require("core.lsp.util")

vim.lsp.config("*", {
    root_markers = { ".git" },
})
vim.lsp.enable({
    "bashls",          -- shell
    "cssls",           -- css
    "gopls",           -- go
    "hls",             -- haskell
    "html",            -- html
    "lua_ls",          -- lua
    "pyright", "ruff", -- python
    "taplo",           -- toml
    "ts_ls",           -- typescript
    "vue_ls",          -- vue
})

vim.api.nvim_create_autocmd("LSPAttach", {
    group = vim.api.nvim_create_augroup("my.lsp", {}),
    callback = function(args)
        local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
        if client:supports_method("textDocument/inlayHint") then
            vim.lsp.inlay_hint.enable(true, { bufnr = args.buf })
        end
    end,
})

vim.api.nvim_create_user_command("LspRestart", function(args)
    local clients = args.fargs
    clients = #clients == 0 and lsp_util.client_names():totable() or clients
    for _, client in ipairs(clients) do
        if vim.lsp.config[client] == nil then
            vim.notify(
                "Invalid server name: " .. client,
                vim.log.levels.ERROR
            )
        else -- For starting clients manually
            vim.lsp.enable(client, false)
        end
    end
    local timer = assert(vim.uv.new_timer())
    timer:start(500, 0, function()
        for _, client in ipairs(clients) do
            vim.schedule_wrap(function(name)
                vim.lsp.enable(name)
            end)(client)
        end
    end)
end, {
    desc = "Restart the LSP client",
    complete = lsp_util.complete_client,
    nargs = "*",
})

require("core.lsp.document_highlight").setup({ enable = true })
