local dressing_status_ok, dressing = pcall(require, "dressing")
if not dressing_status_ok then
    vim.notify("dressing not found!")
    return
end

local icons = require("util.icons")

dressing.setup {
    input = {
        enabled = true,
        default_prompt = "Input:",
        title_pos = "left",
        insert_only = false,
        start_in_insert = true,
        border = icons.ui.window.float.border,
        relative = "editor",
        prefer_width = 0.2,
        width = nil,
        max_width = 0.7,
        min_width = 0.2,
        buf_options = {},
        win_options = {
            wrap = false,
            list = true,
            listchars = "precedes:" .. icons.misc.Ellipsis .. ",extends:" .. icons.misc.Ellipsis,
            sidescrolloff = 0,
        },
        mappings = {
            n = {
                ["<Esc>"] = "Close",
                ["<CR>"] = "Confirm",
            },
            i = {
                ["<C-c>"] = "Close",
                ["<CR>"] = "Confirm",
                ["<C-p>"] = "HistoryPrev",
                ["<C-n>"] = "HistoryNext",
            },
        },
        override = function(conf)
            return conf
        end,
        get_config = nil,
    },
    select = {
        enabled = true,
        backend = { "telescope", "fzf_lua", "fzf", "builtin", "nui", },
        trim_prompt = true,
        telescope = nil,
        fzf_lua = nil,
        fzf = nil,
        builtin = {
            show_numbers = true,
            border = icons.ui.window.float.border,
            relative = "editor",
            buf_options = {},
            win_options = {
                cursorline = true,
                cursorlineopt = "both",
            },
            width = nil,
            max_width = 0.7,
            min_width = 0.2,
            height = nil,
            max_height = 0.7,
            min_height = 0.3,
            mappings = {
                ["<Esc>"] = "Close",
                ["<C-c>"] = "Close",
                ["<CR>"] = "Confirm",
            },
            override = function(conf)
                return conf
            end,
            format_item_override = {},
            get_config = nil,
        },
        nui = nil,
    },
}
