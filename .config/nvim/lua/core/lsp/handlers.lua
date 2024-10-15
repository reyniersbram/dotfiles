local M = {}

local icons = require("util.icons")
local Spinner = require("util.notify.Spinner")

-- LSP Progress Updates

---@param title string
---@param client_name string | nil
local function format_title(title, client_name)
    if not client_name then
        return "[lsp]" .. (#title > 0 and ": " .. title or "")
    end
    return "[" .. client_name .. "]" .. (#title > 0 and ": " .. title or "")
end

local function format_message(message, percentage)
    if not (message or percentage) then
        return nil
    end
    return (percentage and percentage .. "%\t" or "") .. (message or "")
end

---@deprecated Use `progress_notification_callback` instead
local function progress_notification_callback_legacy(spinners)
    for _, client in ipairs(vim.lsp.get_active_clients()) do
        for token, progress in ipairs(client.messages.progress) do
            if not spinners[client.id] then
                spinners[client.id] = {}
            end
            local spinner = spinners[client.id][token]
            if not progress.done then
                local message = format_message(progress.message, progress.percentage) or "Starting up..."
                if not spinner then
                    local opts = { title = format_title(progress.title, client.name), render = "compact" }
                    spinners[client.id][token] = Spinner(message, vim.log.levels.INFO, opts)
                else
                    spinner:update(message)
                end
            else
                client.messages.pending[token] = nil
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


local function progress_notification_callback(event, spinners)
    local client_id = event.data.client_id --[[@as integer]]
    local client = vim.lsp.get_client_by_id(client_id)
    local params = event.data.params --[[@as lsp.ProgressParams]]
    local progress = params.value
    local token = params.token
    if not progress then
        return
    end
    if not spinners[client_id] then
        spinners[client_id] = {}
    end
    if progress.kind == "begin" then
        local message = format_message(progress.message, progress.percentage) or "Starting up..."
        local opts = { title = format_title(progress.title, client and client.name or nil), render = "compact" }
        spinners[client_id][token] = Spinner(message, vim.log.levels.INFO, opts)
    elseif progress.kind == "report" then
        local spinner = spinners[client_id][token]
        spinner:update(format_message(progress.message, progress.percentage))
    elseif progress.kind == "end" then
        local spinner = spinners[client_id][token]
        local opts = {
            icon = icons.ui.status.Done,
        }
        spinner:done(progress.message or "Done", nil, opts)
        spinners[client_id][token] = nil
    end
end

local NVIM_V_010 = vim.fn.has("nvim-0.10.0") > 0

local function configure_progress_notifications()
    local spinners = {}
    if NVIM_V_010 then
        vim.api.nvim_create_autocmd("LspProgress", {
            callback = function(event)
                progress_notification_callback(event, spinners)
            end
        })
    else
        local group = vim.api.nvim_create_augroup("LspNotify", { clear = true })
        vim.api.nvim_create_autocmd("User", {
            pattern = "LspProgressUpdate",
            group = group,
            desc = "LSP progress notifications",
            callback = function()
                progress_notification_callback_legacy(spinners)
            end,
        })
    end
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

local function code_action_listener()
    local context = { diagnostics = vim.lsp.diagnostic.get_line_diagnostics() }
    local params = vim.lsp.util.make_range_params()
    params.context = context
    -- vim.lsp.buf_request(
    --     0, "textDocument/codeAction", params,
    --     function(err, result, ctx, config)
    --         -- TODO: do something, e.g. set sign 󰁨  or 
    --     end
    -- )
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
