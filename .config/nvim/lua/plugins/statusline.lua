local require = require("util").cb_require

return {
    {
        "nvim-lualine/lualine.nvim",
        dependencies = {
            "nvim-tree/nvim-web-devicons",
        },
        config = require("plugins.config.statusline"),
    },
}
