local function setup_higlight_symbol(event)
    local id = vim.tbl_get(event, "data", "client_id")
    local client = id and vim.lsp.get_client_by_id(id)
    if client == nil or not client.supports_method("textDocument/documentHighlight") then
        return
    end

    local group = vim.api.nvim_create_augroup(
        "HighlightSymbol",
        { clear = false }
    )
    vim.api.nvim_clear_autocmds {
        buffer = event.buf,
        group = group,
    }
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
        group = group,
        buffer = event.buf,
        callback = function() vim.lsp.buf.document_highlight() end,
        desc = "Highlight symbol under cursor",
    })

    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
        group = group,
        buffer = event.buf,
        callback = function() vim.lsp.buf.clear_references() end,
        desc = "Clear highlight symbol under cursor",
    })
end

vim.api.nvim_create_autocmd("LspAttach", {
    desc = "Setup symbol highlighting",
    callback = setup_higlight_symbol,
})
