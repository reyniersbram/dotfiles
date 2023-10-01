local M = {}

local icons = require("helpers.icons")

-- Diagnostics
local signs = {
    { name = "DiagnosticSignError", text = icons.diagnostics.Error },
    { name = "DiagnosticSignWarn",  text = icons.diagnostics.Warning },
    { name = "DiagnosticSignHint",  text = icons.diagnostics.Hint },
    { name = "DiagnosticSignInfo",  text = icons.diagnostics.Information },
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
vim.lsp.handlers["textDocument/hover"] =
    vim.lsp.with(
        vim.lsp.handlers.hover,
        {
            border = "rounded",
        }
    )

vim.lsp.handlers["textDocument/signatureHelp"] =
    vim.lsp.with(
        vim.lsp.handlers.signature_help,
        {
            border = "rounded",
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
