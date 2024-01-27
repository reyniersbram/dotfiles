local require = require("util").cb_require

return {
    {
        "stevearc/dressing.nvim",
        config = require("plugins.config.dressing"),
        -- lazy = true,
    },
    {
        "nvim-tree/nvim-web-devicons",
        lazy = true,
    },
    {
        "karb94/neoscroll.nvim",
        opts = {
            mappings = {
                "<C-u>", "<C-d>", "<C-b>", "<C-f>",
                "<C-y>", "<C-e>", "zt", "zz", "zb"
            },
            hide_cursor = true,
            stop_eof = true,
            respect_scrolloff = false,
            cursor_scrolls_alone = true,
            easing_function = "sine",
            pre_hook = nil,
            post_hook = nil,
            performance_mode = false,
        },
    },
}
