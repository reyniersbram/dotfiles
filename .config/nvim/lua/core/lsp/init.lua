local M = {}
local icons = require("util.icons")

function M.setup()
    vim.lsp.set_log_level(vim.lsp.log_levels.ERROR)

    -- UI --
    vim.lsp.handlers["textDocument/hover"] =
        vim.lsp.with(
            vim.lsp.handlers.hover,
            {
                border = icons.ui.window.float.border,
                -- title = "Hover Information",
                -- title_pos = "right",
            }
        )
    vim.lsp.handlers["textDocument/signatureHelp"] =
        vim.lsp.with(
            vim.lsp.handlers.signature_help,
            {
                border = icons.ui.window.float.border,
                -- title = "Signature Help",
                -- title_pos = "right",
            }
        )
end

-- TODO: e.g. make this work with Telescope
M.server_capabilities = function()
    vim.ui.select(
        vim.lsp.get_clients(),
        {
            prompt = "Select client:",
            ---@param item vim.lsp.Client
            format_item = function(item)
                return item.name
            end,
        },
        ---@param choice? vim.lsp.Client
        function(choice)
            if not choice then
                return
            end
            vim.print(vim.inspect(choice.server_capabilities))
        end
    )
end

function M.on_attach(client, bufnr)
    require("core.lsp.keymaps").on_attach(client, bufnr)
    require("core.lsp.highlight").on_attach(client, bufnr)
    -- TODO:
    -- require("core.lsp.format_on_save")
    -- require("core.lsp.inlay_hints")
end

-- TODO: look at codelens, inlay_hints, semantic_tokens and client_capabilities (protocol), and some keymaps (buf)
-- https://neovim.io/doc/user/lsp.html

return M
