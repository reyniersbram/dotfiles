local M = {}

local icons = require("util.icons")

vim.lsp.set_log_level("off")

-- Diagnostics
local signs = {
    { name = "DiagnosticSignError", text = icons.diagnostics.ERROR },
    { name = "DiagnosticSignWarn",  text = icons.diagnostics.WARN },
    { name = "DiagnosticSignInfo",  text = icons.diagnostics.INFO },
    { name = "DiagnosticSignHint",  text = icons.diagnostics.HINT },
}

for _, sign in ipairs(signs) do
    vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

vim.diagnostic.config {
    virtual_text = true,
    signs = {
        priority = 10,
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

-- LSP
vim.lsp.handlers["textDocument/hover"] =
    vim.lsp.with(
        vim.lsp.handlers.hover,
        {
            border = icons.ui.window.float.border,
        }
    )

vim.lsp.handlers["textDocument/signatureHelp"] =
    vim.lsp.with(
        vim.lsp.handlers.signature_help,
        {
            border = icons.ui.window.float.border,
        }
    )


M.server_capabilities = function()
    local active_clients = vim.lsp.get_active_clients()
    local active_client_map = {}

    for index, value in ipairs(active_clients) do
        active_client_map[value.name] = index
    end

    vim.ui.select(
        vim.tbl_keys(active_client_map),
        {
            prompt = "Select client:",
            format_item = function(item)
                return "capabilites for: " .. item
            end,
        },
        function(choice)
            -- print(active_client_map[choice])
            print(vim.inspect(
                vim.lsp.get_active_clients()[active_client_map[choice]]
                .server_capabilities
                .executeCommandProvider
            ))
            vim.pretty_print(
                vim.lsp.get_active_clients()[active_client_map[choice]]
                .server_capabilities
            )
        end
    )
end

return M
