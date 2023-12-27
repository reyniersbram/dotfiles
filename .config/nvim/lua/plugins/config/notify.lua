local status_ok, notify = pcall(require, "notify")
if not status_ok then
    vim.notify("Plugin nvim-notify not loaded", vim.log.levels.WARN)
    return
end

local icons = require("helpers.icons")

vim.notify = notify

require("helpers.utils").try_with_module("telescope", function(telescope)
    telescope.load_extension("notify")
end)

notify.setup {
    level = vim.log.levels.INFO,
    timeout = 5000,
    max_width = 80,
    max_height = nil,
    stages = "fade_in_slide_out",
    background_colour = "NotifyBackground",
    icons = icons.log_level,
    on_open = function(win)
        local buf = vim.api.nvim_win_get_buf(win)
        vim.api.nvim_buf_set_option(buf, "filetype", "markdown")
    end,
    on_close = nil,
    render = "default",
    minimum_width = 50,
    fps = 30,
    top_down = false,
}

-- LSP Progress
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
    vim.api.nvim_create_augroup("lsp_notify", { clear = true })
    vim.api.nvim_create_autocmd("User", {
        pattern = "LspProgressUpdate",
        group = "lsp_notify",
        desc = "LSP progress notifications",
        callback = function()
            local Spinner = require("helpers.spinner")
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
configure_progress_notifications()

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
