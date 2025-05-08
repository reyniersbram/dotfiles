local M = {}

---@class documentHighlight.Opts
---@field enable? boolean

---@type documentHighlight.Opts
local default_opts = {
    enable = true,
}

--- Track which buffers are enabled
---@type table<integer, boolean>
local enabled_buffers = {}

--- Get the name of the LSP document highlight augroup for a buffer
---@param bufnr integer
---@return string
local function augroup_name(bufnr)
    return ("LSPHighlight_%d"):format(bufnr)
end

--- Find a client supporting textDocument/documentHighlight
---@param bufnr? integer
---@return vim.lsp.Client | nil
function M.get_supporting_client(bufnr)
    local clients = vim.lsp.get_clients({ bufnr = bufnr })
    return vim.iter(clients):find(function(client)
        return client:supports_method("textDocument/documentHighlight")
    end)
end

--- Create autocmds to set and reset document highlighting. Does nothing when no
--- client attached to the buffer supports textDocument/documentHighlight.
---@param bufnr? integer The buffer to which the autocmds are aplied, takes the
---current buffer when nil.
function M.create_highlighting_autocmds(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()

    -- Skip if already enabled
    if enabled_buffers[bufnr] then
        vim.notify("Buffer already enabled", vim.log.levels.DEBUG)
        return
    end

    -- Check if a supporting client is attached to the buffer
    local supporting_client = M.get_supporting_client(bufnr)
    if not supporting_client then
        return
    end

    local lsp_group = vim.api.nvim_create_augroup(
        augroup_name(bufnr),
        { clear = true }
    )

    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
        group = lsp_group,
        buffer = bufnr,
        callback = function()
            vim.lsp.buf.document_highlight()
        end,
    })
    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
        group = lsp_group,
        buffer = bufnr,
        callback = function()
            vim.lsp.buf.clear_references()
        end,
    })

    enabled_buffers[bufnr] = true
end

--- Remove the autocmds for highlighting.
---@param bufnr? integer The buffer of which the autocmds should be removed,
---takes current buffer when nil.
local function remove_highlighting_autocmds(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()
    if not enabled_buffers[bufnr] then
        return
    end
    pcall(vim.api.nvim_del_augroup_by_name, augroup_name(bufnr))
    enabled_buffers[bufnr] = false
    vim.lsp.buf.clear_references()
end

--- Enable document highlighting
function M.enable()
    local augroup = vim.api.nvim_create_augroup(
        "LSPHighlight_Setup",
        { clear = true }
    )

    vim.api.nvim_create_autocmd({ "LspAttach" }, {
        group = augroup,
        desc = "Setup LSP highlight symbols",
        callback = function(args)
            M.create_highlighting_autocmds(args.buf)
        end
    })
    vim.api.nvim_create_autocmd({ "LspDetach" }, {
        group = augroup,
        desc = "Unset LSP highlight symbols",
        callback = function(args)
            -- Another client still supports highlighting
            if M.get_supporting_client(args.buf) then
                return
            end
            remove_highlighting_autocmds(args.buf)
        end
    })
    local bufnr = vim.api.nvim_get_current_buf()
    M.create_highlighting_autocmds(bufnr)
end

--- Disable document highlighting
function M.disable()
    remove_highlighting_autocmds(vim.api.nvim_get_current_buf())
    pcall(vim.api.nvim_del_augroup_by_name, "LSPHighlight_Setup")
end

--- Toggle document highlighting
function M.toggle()
    local bufnr = vim.api.nvim_get_current_buf()
    if enabled_buffers[bufnr] then
        M.disable()
    else
        M.enable()
    end
end

--- Setup LSP document highlighting
---@param user_opts? documentHighlight.Opts
function M.setup(user_opts)
    local opts = vim.tbl_deep_extend("force", default_opts, user_opts or {})

    if opts.enable then
        M.enable()
    end

    vim.api.nvim_create_user_command("LSPDocumentHighlightEnable", M.enable,
        { desc = "Enable LSP Document Highlighing" })
    vim.api.nvim_create_user_command("LSPDocumentHighlightDisable", M.disable,
        { desc = "Disable LSP Document Highlighing" })
    vim.api.nvim_create_user_command("LSPDocumentHighlightToggle", M.toggle,
        { desc = "Toggle LSP Document Highlighing" })
end

return M
