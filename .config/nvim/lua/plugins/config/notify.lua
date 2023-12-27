local status_ok, notify = pcall(require, "notify")
if not status_ok then
    vim.notify("Plugin nvim-notify not loaded", vim.log.levels.WARN)
    return
end


vim.notify = notify

require("util").try_with_module("telescope", function(telescope)
    telescope.load_extension("notify")
end)

notify.setup {
    level = vim.log.levels.INFO,
    timeout = 5000,
    max_width = 80,
    max_height = nil,
    stages = "fade_in_slide_out",
    background_colour = "NotifyBackground",
    icons = require("util.icons").log_level,
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
