---@param client vim.lsp.Client
---@param bufnr integer
local function setup_higlight_symbol(client, bufnr)
    if client == nil or not client.supports_method("textDocument/documentHighlight") then
        return
    end

    local group = vim.api.nvim_create_augroup(
        "HighlightSymbol",
        { clear = false }
    )
    vim.api.nvim_clear_autocmds {
        buffer = bufnr,
        group = group,
    }
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
        group = group,
        buffer = bufnr,
        callback = function() vim.lsp.buf.document_highlight() end,
        desc = "Highlight symbol under cursor",
    })

    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
        group = group,
        buffer = bufnr,
        callback = function() vim.lsp.buf.clear_references() end,
        desc = "Clear highlight symbol under cursor",
    })
end

local M = {}

---@param client vim.lsp.Client
---@param bufnr integer
function M.on_attach(client, bufnr)
    setup_higlight_symbol(client, bufnr)
end

return M
