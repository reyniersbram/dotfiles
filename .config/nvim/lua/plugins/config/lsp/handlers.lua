local M = {}

local icons = require("util.icons")

-- LSP Progress Updates
local function format_title(title, client_name)
    return "[" .. client_name .. "]" .. (#title > 0 and ": " .. title or "")
end

local function format_message(message, percentage)
    if not (message or percentage) then
        return nil
    end
    return (percentage and percentage .. "%\t" or "") .. (message or "")
end

local function configure_progress_notifications()
    local spinners = {}
    local group = vim.api.nvim_create_augroup("LspNotify", { clear = true })
    vim.api.nvim_create_autocmd("User", {
        pattern = "LspProgressUpdate",
        group = group,
        desc = "LSP progress notifications",
        callback = function()
            local Spinner = require("util.notify.Spinner")
            for _, client in ipairs(vim.lsp.get_active_clients()) do
                for token, progress in pairs(client.messages.progress) do
                    if not spinners[client.id] then
                        spinners[client.id] = {}
                    end
                    local spinner = spinners[client.id][token]
                    if not progress.done then
                        local message = format_message(progress.message, progress.percentage) or "Starting up..."
                        if not spinner then
                            local opts = { title = format_title(progress.title, client.name) }
                            spinners[client.id][token] = Spinner(message, vim.log.levels.INFO, opts)
                        else
                            spinner:update(message)
                        end
                    else
                        client.messages.progress[token] = nil
                        if spinner then
                            local opts = {
                                icon = icons.ui.status.Done,
                            }
                            spinner:done(progress.message or "Done", nil, opts)
                            spinners[client.id][token] = nil
                        end
                    end
                end
            end
        end
    })
end

-- local severity = {
--     "error", "warn", "info", "info",
-- }

-- FIXME: probably won't do anything because of default handlers
-- https://www.reddit.com/r/neovim/comments/sxlkua/comment/hxtedzz/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
-- vim.lsp.handlers["window/showMessage"] = function(err, method, params, client_id)
--     -- NOTE: this notification is to figure out when this event occurs
--     vim.notify("window/showMessage invoked")
--     vim.notify(method.message, severity[params.type])
-- end


-- Capabilities
M.capabilities = vim.lsp.protocol.make_client_capabilities()
M.capabilities.textDocument.completion.completionItem.snippetSupport = true

-- CMP integration
require("util").try_with_module(
    "cmp_nvim_lsp",
    function(cmp_lsp)
        M.capabilities = cmp_lsp.default_capabilities(M.capabilities)
    end
)

-- On Attach
local function set_keymaps(bufnr)
    local opts = { noremap = true, silent = true, buffer = bufnr }

    vim.keymap.set("n", "<leader>r", vim.lsp.buf.rename, opts)
    vim.keymap.set("n", "gd",
        function()
            vim.lsp.buf.definition { reuse_win = true }
        end, opts)
    vim.keymap.set("n", "gD",
        function()
            vim.lsp.buf.declaration { reuse_win = true }
        end, opts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
    vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, opts)

    vim.keymap.set("n", "gf", vim.lsp.buf.format, opts)
    vim.keymap.set("n", "ga", vim.lsp.buf.code_action, opts)

    -- Using Telescope
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<cmd>Telescope lsp_declarations<<CR>", opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>Telescope lsp_definitions<CR>", opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>Telescope lsp_implementations<CR>", opts)

    -- TODO check if I want these
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>Telescope lsp_references<CR>", opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
    -- vim.cmd([[ command! Format execute 'lua vim.lsp.buf.format()' ]])
end

M.on_attach = function(client, bufnr)
    set_keymaps(bufnr)
end


local function code_action_listener()
    local context = { diagnostics = vim.lsp.diagnostic.get_line_diagnostics() }
    local params = vim.lsp.util.make_range_params()
    params.context = context
    vim.lsp.buf_request(
        0, "textDocument/codeAction", params,
        function(err, result, ctx, config)
            -- TODO: do something, e.g. set sign
        end
    )
end

function M.setup()
    configure_progress_notifications()

    -- Check for code actions on CursorHold
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
        group = vim.api.nvim_create_augroup("code_action_sign", { clear = true }),
        callback = function()
            code_action_listener()
        end
    })
end

return M
