local status_ok, notify = pcall(require, "notify")
if not status_ok then
    vim.notify("Plugin nvim-notify not loaded", vim.log.levels.WARN)
    return
end

local icons = require("helpers.icons")

vim.notify = notify

local telescope_installed, telescope = pcall(require, "telescope")
if telescope_installed then
    telescope.load_extension("notify")
end

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
local client_notifs = {}

local function get_notif_data(client_id, token)
    if not client_notifs[client_id] then
        client_notifs[client_id] = {}
    end

    if not client_notifs[client_id][token] then
        client_notifs[client_id][token] = {}
    end

    return client_notifs[client_id][token]
end

local function update_spinner(notif_data)
    if notif_data.spinner then
        local new_spinner = (notif_data.spinner + 1) % #icons.ui.spinner_frames
        notif_data.spinner = new_spinner

        notif_data.notification = vim.notify("", nil, {
            hide_from_history = true,
            icon = icons.ui.spinner_frames[notif_data.spinner],
            replace = notif_data.notification,
        })

        vim.defer_fn(function ()
            update_spinner(notif_data)
        end, 100)
    end
end

local function format_title(title, client_name)
    return "[" .. client_name .. "]" .. (#title > 0 and ": " .. title or "")
end

local function format_message(message, percentage)
    if not (message or percentage) then
        return nil
    end
    return (percentage and percentage .. "%\t" or "") .. (message or "")
end

vim.api.nvim_create_augroup("lsp_notify", { clear = true })
vim.api.nvim_create_autocmd("User", {
    pattern = "LspProgressUpdate",
    group = "lsp_notify",
    desc = "LSP progress notifications",
    callback = function()
        -- TODO: refactor this with proper spinner class etc.
        local bufnr = vim.api.nvim_get_current_buf()
        for _, client in ipairs(vim.lsp.get_active_clients({ bufnr = bufnr })) do
            local name = client.messages.name
            for token, progress in pairs(client.messages.progress) do
                local notification_data = get_notif_data(client.id, token)
                if not progress.done then
                    local message = format_message(progress.message, progress.percentage) or "Starting up..."
                    if not notification_data.notification then
                        -- Create new notification
                        notification_data.spinner = 1
                        notification_data.notification = vim.notify(
                            message,
                            vim.log.levels.INFO,
                            {
                                title = format_title(progress.title, name),
                                icon = icons.ui.spinner_frames[notification_data.spinner],
                                hide_from_history = false,
                                timeout = false,
                            }
                        )
                        update_spinner(notification_data)
                    else
                        -- Update notification
                        notification_data.notification = vim.notify(
                            message,
                            vim.log.levels.INFO,
                            {
                                replace = notification_data.notification,
                                hide_from_history = false,
                            }
                        )
                    end
                else
                    -- Lsp is done
                    notification_data.spinner = nil
                    notification_data.notification = vim.notify(
                        format_message(progress.message) or "Done",
                        vim.log.levels.INFO,
                        {
                            icon = icons.ui.status.Done,
                            replace = notification_data.notification,
                            timeout = 1000,
                        }
                    )
                end
            end
        end
    end
})

-- TODO: figure this out
-- local severity = {
--     "error", "warn", "info", "info",
-- }

-- vim.lsp.handlers["window/showMessage"] = function(err, method, params, client_id)
--     vim.notify(method.message, severity[params.type])
-- end
